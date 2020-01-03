{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent hiding (yield)
import           Control.Concurrent.STM.Delay (newDelay, waitDelay)
import           Control.Monad

import qualified Data.ByteString.Lazy as BL

import           Data.Maybe (catMaybes)
import qualified Data.Text as T

import           Pipes
import           Pipes.Concurrent

import qualified Sound.MIDI.File as MidiFile
import qualified Sound.MIDI.File.Load as MidiFile.Load
import qualified Sound.MIDI.Parser.Report as MidiParser
import           Sound.MIDI.PortMidi (fromMidiFileRelative)
import qualified Sound.PortMidi as PM

import           System.Directory
import           System.Exit


import           HtmlGUI (UICmd, htmlGUI)
import qualified HtmlGUI as UI


type MidiCmd = Either String PM.PMMsg
type MidiTimed = (Rational, [MidiCmd])

data PlayerInput = LoadMidi ![MidiTimed] | PlayPause | Stop | Tick | Tock | PProgress

data PlayerEvent = NoEvent | Clock | Midi !MidiTimed | Delay Rational | Progress Rational String
  deriving Show

data MidiPlayerStatus
  = Playing ![MidiTimed] ![MidiTimed]
  | Paused  ![MidiTimed] ![MidiTimed]
  | Stopped ![MidiTimed]

midiFromPath :: String -> IO (Maybe MidiFile.T)
midiFromPath path = do
  putStrLn $ "Opening " ++ path
  exists <- doesFileExist path
  if exists then do
    file <- BL.readFile path
    let mMidifile = MidiFile.Load.maybeFromByteString file
    case MidiParser.result mMidifile of
      Left err -> do
        putStrLn $ "Error " ++ err
        putStrLn "Try again..."
        pure Nothing
      Right midifile -> do
        putStrLn $ "Loading " ++ path
        pure $ Just midifile
  else do
    putStrLn $ "File " ++ path ++ " does not exist"
    putStrLn "Try again..."
    pure Nothing

listenerUiCmd :: MVar UICmd -> MVar Int -> Producer PlayerInput IO ()
listenerUiCmd cmdVar streamIdxVar = forever getCmd
  where
  getCmd = do
    ev <- lift $ takeMVar cmdVar
    case ev of
      (UI.SelPort idx) -> lift $ do
        putStrLn $ "SelPort " ++ show idx
        _ <- swapMVar streamIdxVar idx
        pure ()
      (UI.LoadMidi path) -> openMidiFile path
      UI.PlayPause -> yield PlayPause
      UI.Stop -> yield Stop
  openMidiFile filename = do
    mMidifile <- lift $ midiFromPath $ T.unpack filename
    case mMidifile of
      Just midifile -> yield (LoadMidi $ preprocess midifile)
      Nothing -> pure ()
    where
      preprocess = groupByTime . fromMidiFileRelative
      groupByTime :: [(Rational, b)] -> [(Rational, [b])]
      groupByTime []     = []
      groupByTime (x:xs) =
        let (ps, qs) = span ((== 0) . fst) xs
            cmds = snd x : fmap snd ps
        in (fst x, cmds) : groupByTime qs

outputHandler :: MVar Int -> [PM.PMStream] -> Consumer PlayerEvent IO ()
outputHandler streamIdxVar streams = forever $ do
  ev <- await
  lift $ case ev of
    Midi (_, cmds) -> do
      streamIdx <- readMVar streamIdxVar
      let stream = streams !! streamIdx
      mapM_ (handleMidiCmd stream) cmds
    _ -> pure ()
  where
    handleMidiCmd stream (Right msg) = do
      eErr <- PM.writeShort stream (PM.PMEvent (PM.encodeMsg msg) 0)
      case eErr of
        Right _  -> pure ()
        Left err -> putStrLn $ "Error: " ++ show err
    handleMidiCmd _ (Left str) = putStrLn ("Output: " ++ str)

progressHandler :: MVar UI.UIStatus -> Consumer PlayerEvent IO ()
progressHandler uiStatusVar = forever $ do
  ev <- await
  liftIO $ case ev of
    Progress pos msg -> modifyMVar_ uiStatusVar (newUiStatus pos msg)
    _          -> pure ()
  where
    newUiStatus pos msg uiStatus = pure $ uiStatus
      { UI.progress = fromRational pos, UI.display = msg }

clockHandler :: Pipe PlayerEvent PlayerInput IO ()
clockHandler = forever $ do
  ev <- await
  case ev of
    Delay t -> do
      let microsecs = round $ t * (10^(6::Int))
      lift $ atomically . waitDelay =<< newDelay microsecs
      yield Tock
    Midi _ -> yield PProgress *> yield Tick
    Clock  -> yield PProgress *> yield Tick
    _ -> pure ()

handleInput :: PlayerInput -> MidiPlayerStatus -> (PlayerEvent, MidiPlayerStatus)
handleInput ev status =
  case (status, ev) of
    (_, LoadMidi l) -> (notesOff, Stopped l)
    (Stopped remain        , PlayPause) -> (Clock, Playing remain [])
    (Stopped _             , PProgress) -> (Progress 0 "stopped", status)
    (Paused  []      played, PlayPause) -> (notesOff, Stopped $ reverse played)
    (Paused  remain  played, PlayPause) -> (Clock, Playing remain played)
    (Paused  remain  played, Stop)      -> (notesOff, Stopped (combine remain played))
    (Paused  remain  played, PProgress) -> (Progress (progress remain played) "paused", status)
    (Playing remain  played, PlayPause) -> (notesOff, Paused remain played)
    (Playing remain  played, Stop)      -> (notesOff, Stopped (combine remain played))
    (Playing []      played, Tick)      -> (notesOff, Stopped $ reverse played)
    (Playing (x:_)        _, Tick)      -> (Delay (fst x), status)
    (Playing (x:xs)  played, Tock)      -> (Midi x, Playing xs (x:played))
    (Playing remain  played, PProgress) -> (Progress (progress remain played) "playing", status)
    _ -> (NoEvent, status)
  where
    calcTime = sum . fmap fst
    progress :: [MidiTimed] -> [MidiTimed] -> Rational
    progress rs ps =
      let ptime = calcTime ps
          rtime = calcTime rs
      in (100 * ptime) / (rtime + ptime)
    combine = foldl (flip (:))
    notesOff :: PlayerEvent
    notesOff = Midi (0, [Right $ PM.PMMsg (0xB0 + n) 0x7B 0 | n <- [0..15]])

midiPlayer :: Monad m => Pipe PlayerInput PlayerEvent m ()
midiPlayer = loop (Stopped [])
  where
    loop status = do
      ev <- await
      let (foo, status') = handleInput ev status
      yield foo
      loop status'

main :: IO ()
main = do
  _ <- PM.initialize
  deviceCount <- PM.countDevices
  putStrLn "Output devices:"
  streams <- fmap catMaybes $ forM [0..deviceCount - 1] $ \deviceId -> do
    info <- PM.getDeviceInfo deviceId
    when (PM.output info) $
      putStrLn $ "  " ++ show deviceId ++ ". " ++ PM.name info
    eStream <- PM.openOutput deviceId 0
    pure $ either (const Nothing) (\stream -> pure (PM.name info, stream)) eStream
  case streams of
    _:_ -> do
      streamIdxVar <- newMVar 0
      cmdVar <- newEmptyMVar
      uiStatusVar <- newMVar UI.UIStatus { UI.progress = 0, UI.display = "stopped" }

      mbPlayer <- spawn unbounded
      mbClock <- spawn unbounded
      mbMidi <- spawn unbounded
      mbProgress <- spawn unbounded

      _ <- forkIO $ runEffect $
        fromMailbox mbPlayer >-> midiPlayer >-> toMailbox (mbClock <> mbMidi <> mbProgress)
      _ <- forkIO $ runEffect $
        fromMailbox mbClock >-> clockHandler >-> toMailbox mbPlayer
      _ <- forkIO $ runEffect $
        fromMailbox mbMidi >-> outputHandler streamIdxVar (fmap snd streams)
      _ <- forkIO $ runEffect $
        fromMailbox mbProgress >-> progressHandler uiStatusVar
      _ <- forkIO $ runEffect $
        listenerUiCmd cmdVar streamIdxVar >-> toMailbox mbPlayer

      htmlGUI uiStatusVar cmdVar (fmap fst streams)
      exitSuccess
    [] -> do
      _ <- error "Output device not available"
      exitFailure

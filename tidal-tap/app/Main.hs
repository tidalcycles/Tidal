{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Main where

-- import qualified Sound.Osc.Time.Timeout as O

import Control.Concurrent (forkIO, killThread, newEmptyMVar, threadDelay)
import Control.Concurrent.MVar
  ( MVar,
    modifyMVar_,
    newMVar,
    putMVar,
    readMVar,
    takeMVar,
  )
import Control.Monad (forever, when)
import Control.Monad.State
import Data.Time (NominalDiffTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Graphics.Vty
import Graphics.Vty.CrossPlatform (mkVty)
import qualified Network.Socket as N
import Options.Applicative
import qualified Sound.Osc.Fd as O
import qualified Sound.Osc.Transport.Fd.Udp as O
import qualified Sound.PortMidi as PM
import Sound.PortMidi.Simple (ChannelMessage (controllerNumber))
import qualified Sound.PortMidi.Simple as PM
import System.IO (hPrint, hPutStrLn, stderr)

data Parameters = Parameters {mididevice :: Maybe PM.DeviceID, showdevices :: Bool}

parameters :: Parser Parameters
parameters =
  Parameters
    <$> optional
      ( option
          auto
          ( long "mididevice"
              <> help "Midi input device number"
              <> metavar "INT"
          )
      )
    <*> flag
      False
      True
      ( long "showdevices"
          <> help "Show available midi input devices"
      )

-- data MState = MState
--   { taps :: [NominalDiffTime],
--     nudged :: Int
--   }

data TapState = TapState
  { lastEv :: String,
    taps :: [NominalDiffTime],
    -- mstate :: MVar MState,
    inputQ :: MVar InputQ,
    vty :: Vty,
    running :: Bool,
    sender :: O.Message -> IO (),
    cps :: Maybe Float,
    nudgeAll :: Double,
    nudged :: Int,
    muted :: Bool
  }

data InputQ
  = MidiInput {iTime :: NominalDiffTime, iTimestamp :: PM.Timestamp, iMessage :: PM.Message}
  | EventInput {iTime :: NominalDiffTime, iEvent :: Event}

type TapM = StateT TapState IO

newState :: Vty -> (O.Message -> IO ()) -> MVar InputQ -> IO TapState
newState v senderv mv =
  do
    -- mv <- newMVar $ MState {taps = [], nudged = 0}
    return $
      TapState
        { lastEv = "",
          -- mstate = mv,
          taps = [],
          inputQ = mv,
          vty = v,
          running = True,
          sender = senderv,
          cps = Nothing,
          muted = False,
          nudgeAll = 0,
          nudged = 0
        }

resolve :: String -> Int -> IO N.AddrInfo
resolve host port = do
  let hints = N.defaultHints {N.addrSocketType = N.Stream}
  addr : _ <- N.getAddrInfo (Just hints) (Just host) (Just $ show port)
  return addr

broadcast :: Int
broadcast = 0

maxGap :: NominalDiffTime
maxGap = 1

discardGaps :: [NominalDiffTime] -> [NominalDiffTime]
discardGaps [] = []
discardGaps [t] = [t]
discardGaps (a : b : ts) = if (a - b) > maxGap then [a] else a : discardGaps (b : ts)

timeOut :: NominalDiffTime -> [NominalDiffTime] -> [NominalDiffTime]
timeOut _ [] = []
timeOut now (t : ts)
  | (now - t) > maxGap = []
  | otherwise = t : ts

diffs :: (Num a) => [a] -> [a]
diffs (a : b : xs) = (a - b) : diffs (b : xs)
diffs _ = []

sendCps :: Float -> TapM ()
sendCps c = do
  prev <- gets cps
  when (Just c /= prev) $ do
    send $ O.Message "/setcps" [O.Float c]
    modify $ \s -> s {cps = Just c}

sendTempo :: [NominalDiffTime] -> TapM ()
sendTempo ts
  | length ts >= 2 = do
      -- liftIO $ hPutStrLn stderr $ show ts
      let xs = diffs ts
          avg = sum xs / fromIntegral (length xs)
          tempo = realToFrac $ 1 / (avg * 4)
      sendCps tempo
  | otherwise = return ()
  where
    ds = diffs ts

updateTempo :: TapM ()
updateTempo = do
  t <- liftIO getPOSIXTime
  tapsv <- gets taps
  let ts = discardGaps $ timeOut t tapsv
  sendTempo ts
  modify $ \s -> s {taps = ts}
  return ()

sendNudge :: Double -> TapM ()
sendNudge new = do
  send $ O.Message "/nudgeAll" [O.Double new]
  modify $ \s -> s {nudgeAll = new}

mute :: TapM ()
mute = do
  send $ O.Message "/muteAll" []
  modify $ \s -> s {muted = True}

unmute :: TapM ()
unmute = do
  send $ O.Message "/unmuteAll" []
  modify $ \s -> s {muted = False}

muteToggle :: TapM ()
muteToggle = do
  mutedv <- gets muted
  if mutedv then unmute else mute

send :: O.Message -> TapM ()
send message = do
  senderv <- gets sender
  liftIO $ senderv message

reset :: TapM ()
reset =
  do
    unmute
    sendNudge 0
    send $ O.Message "/resetCycles" []
    return ()

event :: NominalDiffTime -> Event -> TapM ()
event _ (EvKey (KChar 'r') []) = reset
event _ (EvKey (KChar 'm') []) = do
  muteToggle
event t (EvKey (KChar 't') []) = do
  modify $ \s -> s {taps = t : taps s}
  updateTempo
event _ (EvKey (KChar 'q') []) = modify $ \s -> s {running = False}
event _ _ = return ()

tapsToString :: [NominalDiffTime] -> String
tapsToString [] = "[]"
tapsToString [_] = "[]"
tapsToString (a : b : ts) = show (a - b) ++ " : " ++ tapsToString (b : ts)

showcps :: TapState -> String
showcps (TapState {cps = Nothing}) = "unset"
showcps (TapState {cps = Just t}) = show t

processInput :: InputQ -> TapM ()
processInput (EventInput t e) = event t e
processInput (MidiInput t timestamp m) = doMessage t timestamp m

doMessage :: NominalDiffTime -> PM.Timestamp -> PM.Message -> TapM ()
doMessage t timestamp (PM.Channel _ (PM.NoteOn 34 _)) =
  do
    modify $ \s -> s {taps = prepend t $ taps s}
    updateTempo
  where
    prepend a [] = [a]
    prepend a (b : xs)
      | a == b = b : xs
      | otherwise = a : b : xs
doMessage t timestamp (PM.Channel _ (PM.NoteOn 20 _)) = reset
doMessage t timestamp (PM.Channel _ (PM.NoteOn 23 _)) = muteToggle
doMessage t timestamp (PM.Channel _ (PM.ControlChange {PM.controllerNumber = 36, PM.controllerValue = val})) =
  do
    x <- gets cps
    case x of
      Just cpsv -> sendCps $ cpsv + (fromIntegral (if val < 64 then val else val - 128) / 2000)
      Nothing -> return ()
doMessage t timestamp (PM.Channel _ (PM.ControlChange {PM.controllerNumber = 37, PM.controllerValue = val})) =
  do
    nudgedv <- gets nudged
    let newnudge = nudgedv - (if val < 64 then val else val - 128)
    liftIO $ hPrint stderr newnudge
    sendNudge $ fromIntegral newnudge / 1000
    modify $ \s -> s {nudged = newnudge}
doMessage t _ (PM.Channel _ (PM.NoteOff {})) = return ()
doMessage _ _ msg = liftIO $ hPutStrLn stderr $ "Unhandled: " ++ show msg

-- updateNudge :: TapM ()
-- updateNudge = do
--   prev <- gets nudgeAll
--   mv <- gets mstate
--   v <- liftIO $ readMVar mv
--   let new = fromIntegral (nudged v) / 1000
--   when (new /= prev) $ do
--     sendNudge new

taploop :: TapM ()
taploop = do
  showcpsv <- gets showcps
  mutedv <- gets muted
  let img =
        string (defAttr `withBackColor` blue) "Tap tap"
          <-> string
            (defAttr `withBackColor` blue)
            ( if mutedv
                then "[-muted-]"
                else
                  "[unmuted]"
            )
          <-> string (defAttr `withForeColor` green) "r = restart (+unmute), t = tap m = toggle mute"
          <-> string (defAttr `withForeColor` blue) showcpsv
      pic = picForImage img
  vtyv <- gets vty
  liftIO $ update vtyv pic
  mInput <- gets inputQ
  input <- liftIO $ takeMVar mInput
  processInput input
  -- updateTempo
  -- updateNudge
  runningv <- gets running
  when runningv taploop

sendO :: O.Udp -> N.AddrInfo -> O.Message -> IO ()
sendO u addr msg = O.sendTo u (O.Packet_Message {O.packetMessage = msg}) (N.addrAddress addr)

printDevices :: IO ()
printDevices = do
  deviceCount <- PM.countDevices
  putStrLn "\nAvailable input devices:"
  mapM_
    ( \i -> do
        info <- PM.getDeviceInfo i
        when (PM.input info) $ putStrLn $ show i ++ ": " ++ show info
    )
    [0 .. deviceCount - 1]

runMidi :: Maybe PM.DeviceID -> MVar InputQ -> IO ()
runMidi Nothing _ = return ()
runMidi (Just input) mv =
  PM.withInput input $ \stream -> PM.withReadMessages stream 256 $ \readMessages ->
    forever $ do
      t <- getPOSIXTime
      readMessages >>= mapM_ (\(ts, msg) -> putMVar mv $ MidiInput t ts msg)
      threadDelay 1000

readEvents :: Vty -> MVar InputQ -> IO ()
readEvents v mv = forever $ do
  e <- liftIO $ nextEvent v
  t <- getPOSIXTime
  putMVar mv $ EventInput t e

runTap :: Parameters -> IO ()
runTap (Parameters {showdevices = True}) = printDevices
runTap ps =
  do
    PM.initialize
    addr <- resolve "127.0.0.1" 6010
    u <-
      O.udp_socket
        (\sock _ -> do N.setSocketOption sock N.Broadcast broadcast)
        "127.0.0.1"
        6010
    v <- mkVty defaultConfig
    mvInput <- newEmptyMVar
    eventThread <- forkIO $ readEvents v mvInput
    midiThread <- forkIO $ runMidi (mididevice ps) mvInput
    s <- newState v (sendO u addr) mvInput
    evalStateT taploop s
    shutdown v
    killThread midiThread
    killThread eventThread

main :: IO ()
main = do
  ps <- execParser $ info (parameters <**> helper) fullDesc
  runTap ps

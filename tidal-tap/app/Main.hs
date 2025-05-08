{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Main where

import Control.Monad (when, forever)
-- import qualified Sound.Osc.Time.Timeout as O

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
import qualified Sound.PortMidi.Simple as PM
import Control.Concurrent.MVar
    ( modifyMVar_, newMVar, putMVar, takeMVar, MVar )
import Control.Concurrent (threadDelay, forkIO, killThread)
import System.IO (stderr, hPutStrLn, hPrint)
import Sound.PortMidi.Simple (ChannelMessage(controllerNumber))

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

data MState = MState {
  taps :: [NominalDiffTime],
  nudged :: Int
}

data TapState = TapState
  { lastEv :: String,
    mstate :: MVar MState,
    vty :: Vty,
    running :: Bool,
    sender :: O.Message -> IO (),
    cps :: Maybe Float,
    muted :: Bool
  }

type TapM = StateT TapState IO

newState :: Vty -> (O.Message -> IO ()) -> IO TapState
newState v senderv =
  do mv <- newMVar $ MState {taps = [], nudged = 0}
     return $ TapState
      { lastEv = "",
        mstate = mv,
        vty = v,
        running = True,
        sender = senderv,
        cps = Nothing,
        muted = False
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

sendTempo :: [NominalDiffTime] -> TapM ()
sendTempo ts
  | length ts >= 2 = do
      -- liftIO $ hPutStrLn stderr $ show ts
      let xs = diffs ts
          avg = sum xs / fromIntegral (length xs)
          tempo = realToFrac $ 1 / (avg * 4)
      send $ O.Message "/setcps" [O.Float tempo]
      modify $ \s -> s {cps = Just tempo}
  | otherwise = return ()
  where
    ds = diffs ts

updateTempo :: TapM ()
updateTempo = do
  -- liftIO $ hPutStrLn stderr "aha"
  t <- liftIO getPOSIXTime
  mv <- gets mstate
  v <- liftIO $ takeMVar mv
  let ts = discardGaps $ timeOut t (taps v)
  sendTempo ts
  liftIO $ putMVar mv (v {taps = ts})
  -- liftIO $ hPutStrLn stderr "aho"
  return ()

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

event :: Maybe Event -> TapM ()
event (Just (EvKey (KChar 'r') [])) = do
  unmute
  send $ O.Message "/resetCycles" []
  return ()
event (Just (EvKey (KChar 'm') [])) = do
  senderv <- gets sender
  muteToggle
event (Just (EvKey (KChar 't') [])) = do
  t <- liftIO getPOSIXTime
  mv <- gets mstate
  liftIO $ modifyMVar_ mv $ \v -> return v {taps = t : taps v}
  updateTempo
event (Just (EvKey (KChar 'q') [])) = modify $ \s -> s {running = False}
event _ = return ()

tapsToString :: [NominalDiffTime] -> String
tapsToString [] = "[]"
tapsToString [_] = "[]"
tapsToString (a : b : ts) = show (a - b) ++ " : " ++ tapsToString (b : ts)

showcps :: TapState -> String
showcps (TapState {cps = Nothing}) = "unset"
showcps (TapState {cps = Just t}) = show t

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
  e <- liftIO $ nextEventNonblocking vtyv
  event e
  updateTempo
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

doMessage :: MVar MState -> (PM.Timestamp, PM.Message) -> IO ()
doMessage mv (ts, msg@(PM.Channel _ (PM.NoteOn {}))) =
   do t <- getPOSIXTime
      modifyMVar_ mv $ \v -> return v {taps = prepend t $ taps v}
      return ()
  where prepend a [] = [a]
        prepend a (b:xs) | a == b = b:xs
                         | otherwise = a:b:xs
doMessage mv (_, PM.Channel _ (PM.ControlChange {PM.controllerNumber = 37, PM.controllerValue = val})) = 
  do modifyMVar_ mv $ \v -> do let newnudge = nudged v + (if val < 64 then val else val - 128)
                               hPrint stderr newnudge
                               return v {nudged = newnudge}
doMessage _ (_, PM.Channel _ (PM.NoteOff {})) = return ()
doMessage _ msg = hPutStrLn stderr $ "Unhandled: " ++ show msg


runMidi :: Maybe PM.DeviceID -> MVar MState -> IO ()
runMidi Nothing _ = return ()
runMidi (Just input) mv = 
  PM.withInput input $ \stream -> PM.withReadMessages stream 256 $ \readMessages ->
                                   forever $ do
                                     readMessages >>= mapM_ (doMessage mv)
                                     threadDelay 1000


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
    s <- newState v (sendO u addr)
    midiThread <- forkIO $ runMidi (mididevice ps) (mstate s)
    evalStateT taploop s
    shutdown v
    killThread midiThread

main :: IO ()
main = do
  ps <- execParser $ info (parameters <**> helper) fullDesc
  runTap ps

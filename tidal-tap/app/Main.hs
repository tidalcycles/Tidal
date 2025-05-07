{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Main where

import Control.Monad (when)
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
import Control.Concurrent (threadDelay, forkIO)
import System.IO (stderr, hPutStrLn)

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

data TapState = TapState
  { lastEv :: String,
    taps :: MVar [NominalDiffTime],
    vty :: Vty,
    running :: Bool,
    sender :: O.Message -> IO (),
    cps :: Maybe Float,
    muted :: Bool
  }

type TapM = StateT TapState IO

newState :: Vty -> (O.Message -> IO ()) -> IO TapState
newState v send =
  do tapsmv <- newMVar []
     return $ TapState
      { lastEv = "",
        taps = tapsmv,
        vty = v,
        running = True,
        sender = send,
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
  t <- liftIO getPOSIXTime
  tapsmv <- gets taps
  tapsv <- liftIO $ takeMVar tapsmv
  let ts = discardGaps $ timeOut t tapsv
  sendTempo ts
  liftIO $ putMVar tapsmv ts
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
  tapsmv <- gets taps
  liftIO $ modifyMVar_ tapsmv $ \ts -> return $ t : ts
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

runMidi :: Maybe PM.DeviceID -> MVar [NominalDiffTime] -> IO ()
runMidi Nothing _ = return ()
runMidi (Just input) tapsmv = 
  PM.withInput input $ \stream -> PM.withReadMessages stream 256 $ \readMessages ->
                                   forever $ do
                                     t <- liftIO getPOSIXTime
                                     modifyMVar_ tapsmv $ \ts -> return $ t : ts
                                     readMessages >>= mapM_ (hPutStrLn stderr . show)
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
    threadid <- forkIO $ runMidi (mididevice ps) (taps s)
    evalStateT taploop s
    shutdown v

main :: IO ()
main = do
  ps <- execParser $ info (parameters <**> helper) fullDesc
  runTap ps

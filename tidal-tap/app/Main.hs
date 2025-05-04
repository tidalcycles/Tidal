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
    taps :: [NominalDiffTime],
    vty :: Vty,
    running :: Bool,
    sender :: O.Message -> IO (),
    cps :: Maybe Float,
    midiIn :: Maybe PM.PMStream,
    muted :: Bool
  }

type TapM = StateT TapState IO

newState :: Vty -> (O.Message -> IO ()) -> Maybe PM.PMStream -> TapState
newState v send mi =
  TapState
    { lastEv = "",
      taps = [],
      vty = v,
      running = True,
      sender = send,
      cps = Nothing,
      midiIn = mi,
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
      modify $ \s -> s {cps = Just tempo, taps = ts}
  | otherwise = modify $ \s -> s {taps = ts}
  where
    ds = diffs ts

updateTempo :: TapM ()
updateTempo = do
  t <- liftIO getPOSIXTime
  tapsv <- gets taps
  let ts = discardGaps $ timeOut t tapsv
  sendTempo ts

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

event :: Event -> TapM ()
event (EvKey (KChar 'r') []) = do
  unmute
  send $ O.Message "/resetCycles" []
  return ()
event (EvKey (KChar 'm') []) = do
  senderv <- gets sender
  muteToggle
event (EvKey (KChar 't') []) = do
  t <- liftIO getPOSIXTime
  modify $ \s -> s {taps = t : taps s}
  updateTempo
event (EvKey (KChar 'q') []) = modify $ \s -> s {running = False}
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
  e <- liftIO $ nextEvent vtyv
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

runTap :: Parameters -> IO ()
runTap (Parameters {showdevices = True}) = printDevices
runTap ps =
  do
    PM.initialize
    mi <-
      maybe
        (return Nothing)
        ( \i -> do
            md <- liftIO $ PM.openInput i
            either
              ( \err -> do
                  putStrLn $ "Couldn't open midi device " ++ show err
                  return Nothing
              )
              (return . Just)
              md
        )
        (mididevice ps)
    addr <- resolve "127.0.0.1" 6010
    u <-
      O.udp_socket
        (\sock _ -> do N.setSocketOption sock N.Broadcast broadcast)
        "127.0.0.1"
        6010
    v <- mkVty defaultConfig
    evalStateT taploop $ newState v (sendO u addr) mi
    shutdown v

main :: IO ()
main = do
  ps <- execParser $ info (parameters <**> helper) fullDesc
  runTap ps

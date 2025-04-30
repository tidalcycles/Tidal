{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Main where

import Control.Monad (when)
import Data.Time (NominalDiffTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Graphics.Vty
import Graphics.Vty.CrossPlatform (mkVty)
import qualified Network.Socket as N
import Options.Applicative
import qualified Sound.Osc.Fd as O
import qualified Sound.Osc.Time.Timeout as O
import qualified Sound.Osc.Transport.Fd.Udp as O
import qualified Sound.PortMidi as PM

data Parameters = Parameters {mididevice :: Maybe PM.DeviceID}

parameters :: Parser Parameters
parameters =
  Parameters
    <$> optional
      ( option
          auto
          ( long "mididevice"
              <> help "Midi input device number"
              --   <> showDefault
              --   <> value 1
              <> metavar "INT"
          )
      )

data State = State
  { lastEv :: String,
    taps :: [NominalDiffTime],
    vty :: Vty,
    running :: Bool,
    sender :: O.Message -> IO (),
    cps :: Maybe Float
  }

newState :: Vty -> (O.Message -> IO ()) -> State
newState v send = State {lastEv = "", taps = [], vty = v, running = True, sender = send, cps = Nothing}

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

sendTempo :: State -> [NominalDiffTime] -> IO State
sendTempo s ts
  | length ts >= 2 = do
      let xs = diffs ts
          avg = sum xs / fromIntegral (length xs)
          tempo = realToFrac $ 1 / (avg * 4)
      sender s $ O.Message "/setcps" [O.Float $ tempo]
      return $ s {cps = Just tempo, taps = ts}
  | otherwise = return $ s {taps = ts}
  where
    ds = diffs ts

updateTempo :: State -> IO State
updateTempo s = do
  t <- getPOSIXTime
  let ts = discardGaps $ timeOut t $ taps s
  sendTempo s ts

event :: State -> Event -> IO State
event s (EvKey (KChar 'r') []) = do
  sender s $ O.Message "/unmuteAll" []
  sender s $ O.Message "/resetCycles" []
  return s
event s (EvKey (KChar 's') []) = do
  sender s $ O.Message "/muteAll" []
  return s
event s (EvKey (KChar 't') []) = do
  t <- getPOSIXTime
  let s' = s {taps = t : taps s}
  updateTempo s'
event s (EvKey (KChar 'q') []) = return $ s {running = False}
event s _ = return s

tapsToString :: [NominalDiffTime] -> String
tapsToString ([]) = "[]"
tapsToString ([_]) = "[]"
tapsToString (a : b : ts) = show (a - b) ++ " : " ++ tapsToString (b : ts)

showcps :: State -> String
showcps (State {cps = Nothing}) = "unset"
showcps (State {cps = Just t}) = show t

loop :: State -> IO ()
loop s = do
  let line1 = string (defAttr `withBackColor` blue) "Tap tap"
      line2 = string (defAttr `withForeColor` green) "r = retrigger (+unmute), t = tap"
      line3 = string (defAttr `withForeColor` blue) $ showcps s
      img = line1 <-> line2 <-> line3
      pic = picForImage img
  update (vty s) pic
  e <- nextEvent (vty s)
  s' <- event s e
  s'' <- updateTempo s'
  when (running s') $ loop s''

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

start :: Parameters -> IO ()
start (Parameters {mididevice = Nothing}) = do
  putStrLn "Please specify midi device with --mididevice <id>"
  printDevices
start (Parameters {mididevice = Just mididev}) = do
  PM.initialize
  midiin <- PM.openInput mididev
  either (const $ putStrLn $ "Couldn't open midi device " ++ show mididev) (const $ return ()) midiin

--   addr <- resolve "127.0.0.1" 6010
--   u <-
--     O.udp_socket
--       (\sock _ -> do N.setSocketOption sock N.Broadcast broadcast)
--       "127.0.0.1"
--       6010
--   v <- mkVty defaultConfig
--   loop (newState v (sendO u addr))
--   shutdown v

main :: IO ()
main = do
  ps <- execParser $ info (parameters <**> helper) fullDesc
  start ps

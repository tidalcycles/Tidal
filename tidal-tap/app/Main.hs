{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Main where

import Control.Monad (when)
import Data.Time (NominalDiffTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Graphics.Vty
import Graphics.Vty.Platform.Unix (mkVty)
import qualified Network.Socket as N
import qualified Sound.Osc.Fd as O
import qualified Sound.Osc.Time.Timeout as O
import qualified Sound.Osc.Transport.Fd.Udp as O

data State = State
  { lastEv :: String,
    taps :: [NominalDiffTime],
    vty :: Vty,
    running :: Bool,
    sender :: O.Message -> IO ()
  }

newState :: Vty -> (O.Message -> IO ()) -> State
newState v send = State {lastEv = "", taps = [], vty = v, running = True, sender = send}

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

sendTempo :: State -> [NominalDiffTime] -> IO ()
sendTempo s ts
  | length ts >= 4 = do
      let xs = diffs ts
          avg = sum xs / fromIntegral (length xs)
          tempo = 1 / (avg * 4)
      sender s $ O.Message "/setcps" [O.Float $ realToFrac tempo]
      return ()
  | otherwise = return ()
  where
    ds = diffs ts

updateTempo :: State -> IO State
updateTempo s = do
  t <- getPOSIXTime
  let ts = discardGaps $ timeOut t $ taps s
  sendTempo s ts
  return $ s {taps = ts}

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

loop :: State -> IO ()
loop s = do
  let line1 = string (defAttr `withBackColor` blue) "Tap tap"
      line2 = string (defAttr `withForeColor` green) "r = reset"
      line3 = string (defAttr `withForeColor` blue) $ tapsToString $ taps s
      img = line1 <-> line2 <-> line3
      pic = picForImage img
  update (vty s) pic
  e <- nextEvent (vty s)
  s' <- event s e
  s'' <- updateTempo s'
  when (running s') $ loop s''

sendO :: O.Udp -> N.AddrInfo -> O.Message -> IO ()
sendO u addr msg = O.sendTo u (O.Packet_Message {O.packetMessage = msg}) (N.addrAddress addr)

main :: IO ()
main = do
  addr <- resolve "127.0.0.1" 6010
  u <-
    O.udp_socket
      (\sock _ -> do N.setSocketOption sock N.Broadcast broadcast)
      "127.0.0.1"
      6010
  v <- mkVty defaultConfig
  loop (newState v (sendO u addr))
  shutdown v

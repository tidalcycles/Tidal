module Sound.Tidal.Stream.Target where

import qualified Sound.Osc.Fd           as O
import qualified Sound.Osc.Transport.Fd.Udp as O
import qualified Network.Socket         as N
import           Data.Maybe             (fromJust, isJust)
import           Control.Concurrent     (forkOS, threadDelay)
import           Foreign                (Word8)

import           Sound.Tidal.Pattern
import           Sound.Tidal.Stream.Types
import           Sound.Tidal.Stream.Config

{-
    Target.hs - Create and send to OSC targets
    Copyright (C) 2020, Alex McLean and contributors

    This library is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this library.  If not, see <http://www.gnu.org/licenses/>.
-}


getCXs :: Config -> [(Target, [OSC])] -> IO [Cx]
getCXs config oscmap = mapM (\(target, os) -> do
                                          remote_addr <- resolve (oAddress target) (show $ oPort target)
                                          remote_bus_addr <- if isJust $ oBusPort target
                                                              then Just <$> resolve (oAddress target) (show $ fromJust $ oBusPort target)
                                                              else return Nothing
                                          let broadcast = if cCtrlBroadcast config then 1 else 0
                                          u <- O.udp_socket (\sock sockaddr -> do N.setSocketOption sock N.Broadcast broadcast
                                                                                  N.connect sock sockaddr
                                                             ) (oAddress target) (oPort target)
                                          return $ Cx {cxUDP = u, cxAddr = remote_addr, cxBusAddr = remote_bus_addr, cxTarget = target, cxOSCs = os}
            ) oscmap

resolve :: String -> String -> IO N.AddrInfo
resolve host port = do let hints = N.defaultHints { N.addrSocketType = N.Stream }
                       addr:_ <- N.getAddrInfo (Just hints) (Just host) (Just port)
                       return addr

-- send has three modes:
-- Send events early using timestamp in the OSC bundle - used by Superdirt
-- Send events early by adding timestamp to the OSC message - used by Dirt
-- Send events live by delaying the thread
send :: Maybe O.Udp -> Cx -> Double -> Double -> (Double, Bool, O.Message) -> IO ()
send listen cx latency extraLatency (time, isBusMsg, m)
  | oSchedule target == Pre BundleStamp = sendBndl isBusMsg listen cx $ O.Bundle timeWithLatency [m]
  | oSchedule target == Pre MessageStamp = sendO isBusMsg listen cx $ addtime m
  | otherwise = do _ <- forkOS $ do now <- O.time
                                    threadDelay $ floor $ (timeWithLatency - now) * 1000000
                                    sendO isBusMsg listen cx m
                   return ()
    where addtime (O.Message mpath params) = O.Message mpath ((O.int32 sec):((O.int32 usec):params))
          ut = O.ntpr_to_posix timeWithLatency
          sec :: Int
          sec = floor ut
          usec :: Int
          usec = floor $ 1000000 * (ut - (fromIntegral sec))
          target = cxTarget cx
          timeWithLatency = time - latency + extraLatency

sendBndl :: Bool -> (Maybe O.Udp) -> Cx -> O.Bundle -> IO ()
sendBndl isBusMsg (Just listen) cx bndl = O.sendTo listen (O.Packet_Bundle bndl) (N.addrAddress addr)
  where addr | isBusMsg && isJust (cxBusAddr cx) = fromJust $ cxBusAddr cx
             | otherwise = cxAddr cx
sendBndl _ Nothing cx bndl = O.sendBundle (cxUDP cx) bndl

sendO :: Bool -> (Maybe O.Udp) -> Cx -> O.Message -> IO ()
sendO isBusMsg (Just listen) cx msg = O.sendTo listen (O.Packet_Message msg) (N.addrAddress addr)
 where addr | isBusMsg && isJust (cxBusAddr cx) = fromJust $ cxBusAddr cx
            | otherwise = cxAddr cx
sendO _ Nothing cx msg = O.sendMessage (cxUDP cx) msg


superdirtTarget :: Target
superdirtTarget = Target {oName = "SuperDirt",
                          oAddress = "127.0.0.1",
                          oPort = 57120,
                          oBusPort = Just 57110,
                          oLatency = 0.2,
                          oWindow = Nothing,
                          oSchedule = Pre BundleStamp,
                          oHandshake = True
                         }

superdirtShape :: OSC
superdirtShape = OSC "/dirt/play" $ Named {requiredArgs = ["s"]}

dirtTarget :: Target
dirtTarget = Target {oName = "Dirt",
                     oAddress = "127.0.0.1",
                     oPort = 7771,
                     oBusPort = Nothing,
                     oLatency = 0.02,
                     oWindow = Nothing,
                     oSchedule = Pre MessageStamp,
                     oHandshake = False
                    }

dirtShape :: OSC
dirtShape = OSC "/play" $ ArgList [("cps", fDefault 0),
                                   ("s", Nothing),
                                   ("offset", fDefault 0),
                                   ("begin", fDefault 0),
                                   ("end", fDefault 1),
                                   ("speed", fDefault 1),
                                   ("pan", fDefault 0.5),
                                   ("velocity", fDefault 0.5),
                                   ("vowel", sDefault ""),
                                   ("cutoff", fDefault 0),
                                   ("resonance", fDefault 0),
                                   ("accelerate", fDefault 0),
                                   ("shape", fDefault 0),
                                   ("kriole", iDefault 0),
                                   ("gain", fDefault 1),
                                   ("cut", iDefault 0),
                                   ("delay", fDefault 0),
                                   ("delaytime", fDefault (-1)),
                                   ("delayfeedback", fDefault (-1)),
                                   ("crush", fDefault 0),
                                   ("coarse", iDefault 0),
                                   ("hcutoff", fDefault 0),
                                   ("hresonance", fDefault 0),
                                   ("bandf", fDefault 0),
                                   ("bandq", fDefault 0),
                                   ("unit", sDefault "rate"),
                                   ("loop", fDefault 0),
                                   ("n", fDefault 0),
                                   ("attack", fDefault (-1)),
                                   ("hold", fDefault 0),
                                   ("release", fDefault (-1)),
                                   ("orbit", iDefault 0) -- ,
                                   -- ("id", iDefault 0)
                                  ]

sDefault :: String -> Maybe Value
sDefault x = Just $ VS x
fDefault :: Double -> Maybe Value
fDefault x = Just $ VF x
rDefault :: Rational -> Maybe Value
rDefault x = Just $ VR x
iDefault :: Int -> Maybe Value
iDefault x = Just $ VI x
bDefault :: Bool -> Maybe Value
bDefault x = Just $ VB x
xDefault :: [Word8] -> Maybe Value
xDefault x = Just $ VX x

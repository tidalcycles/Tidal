{-# OPTIONS_GHC -fno-warn-dodgy-imports -fno-warn-name-shadowing #-}
module Sound.Tidal.Carabiner where

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString (send, recv)
import qualified Data.ByteString.Char8 as B8
import Control.Concurrent (forkIO, takeMVar, putMVar)
import qualified Sound.Tidal.Stream as S
import Sound.Tidal.Tempo
import System.Clock
import Text.Read (readMaybe)
import Control.Monad (when, forever)
import Data.Maybe (isJust, fromJust)
import qualified Sound.OSC.FD as O

carabiner :: S.Stream -> Int -> Double -> IO Socket
carabiner tidal bpc latency = do sock <- client tidal bpc latency "127.0.0.1" 17000
                                 sendMsg sock "status\n"
                                 return sock

client :: S.Stream -> Int -> Double -> String -> Int -> IO Socket
client tidal bpc latency host port = withSocketsDo $
                       do addrInfo <- getAddrInfo Nothing (Just host) (Just $ show port)
                          let serverAddr = head addrInfo
                          sock <- socket (addrFamily serverAddr) Stream defaultProtocol
                          connect sock (addrAddress serverAddr)
                          _ <- forkIO $ listener tidal bpc latency sock
                          -- sendMsg sock "status\n"
                          -- threadDelay 10000000
                          return sock

listener :: S.Stream -> Int -> Double -> Socket -> IO ()
listener tidal bpc latency sock =
  forever $ do rMsg <- recv sock 1024
               let msg = B8.unpack rMsg
                   (name:_:ws) = words msg
                   pairs = pairs' ws
                   pairs' (a:b:xs) = (a,b):(pairs' xs)
                   pairs' _ = []
               act tidal bpc latency name pairs

act :: S.Stream -> Int -> Double -> String -> [(String, String)] -> IO ()
act tidal bpc latency "status" pairs
  = do let start = (lookup ":start" pairs >>= readMaybe) :: Maybe Integer
           bpm   = (lookup ":bpm"   pairs >>= readMaybe) :: Maybe Double
           beat  = (lookup ":beat"  pairs >>= readMaybe) :: Maybe Double
       when (and [isJust start, isJust bpm, isJust beat]) $ do
         nowM <- getTime Monotonic
         nowO <- O.time
         let m = (fromIntegral $ sec nowM) + ((fromIntegral $ nsec nowM)/1000000000)
             d = nowO - m
             start' = ((fromIntegral $ fromJust start) / 1000000)
             startO = start' + d
             -- cyc = toRational $ (fromJust beat) / (fromIntegral bpc)
         tempo <- takeMVar (S.sTempoMV tidal)
         let tempo' = tempo {atTime = startO + latency,
                             atCycle = 0,
                             cps = ((fromJust bpm) / 60) / (fromIntegral bpc)
                            }
         putMVar (S.sTempoMV tidal) $ tempo'
act _ _ _ name _ = putStr $ "Unhandled thingie " ++ name

sendMsg :: Socket -> String -> IO ()
sendMsg sock msg = do _ <- send sock $ B8.pack msg
                      return ()

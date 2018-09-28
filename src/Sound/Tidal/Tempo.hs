module Sound.Tidal.Tempo where

import Data.Time (getCurrentTime, UTCTime, NominalDiffTime, diffUTCTime, addUTCTime)
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import Safe (readNote)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Control.Concurrent.MVar
import qualified Sound.Tidal.Pattern as P
import qualified Sound.OSC.FD as O
import qualified Network.Socket as N
import Control.Concurrent (forkIO, ThreadId, threadDelay)
import Control.Monad (forM_, forever, void)

data Tempo = Tempo {at :: O.Time,
                    cyclePos :: Double,
                    cps :: O.Time,
                    paused :: Bool,
                    nudged :: Double
                   }

data State = State {ticks :: Int,
                    start :: O.Time,
                    now :: O.Time,
                    arc :: P.Arc
                   }

defaultTempo :: IO Tempo
defaultTempo = do now <- O.time
                  return $ Tempo {at       = now,
                                  cyclePos = 0,
                                  cps      = 2,
                                  paused   = True,
                                  nudged   = 0
                                 }


getClockIp :: IO String
getClockIp = fromMaybe "127.0.0.1" <$> lookupEnv "TIDAL_TEMPO_IP"

getClockPort :: IO Int
getClockPort =
   maybe 9160 (readNote "port parse") <$> lookupEnv "TIDAL_TEMPO_PORT"

-- | given a Tempo and a cycle position, returns the POSIX time of
-- that cycle position
logicalTime :: Tempo -> Double -> Double
logicalTime t c = changeT + timeDelta
  where cycleDelta = c - (cyclePos t)
        timeDelta = cycleDelta / (realToFrac $ cps t)
        changeT = O.ntpr_to_ut $ at t

-- | Returns the time now in terms of
-- cycles relative to metrical grid of a given Tempo
cyclesNow :: Tempo -> IO (Double)
cyclesNow t = do now <- O.time
                 let delta = now - (at t)
                     cycleDelta = (realToFrac $ cps t) * delta
                 return $ cyclePos t + cycleDelta

getCurrentCycle :: MVar Tempo -> IO Rational
getCurrentCycle t = (readMVar t) >>= (cyclesNow) >>= (return . toRational)

tickLength :: O.Time
tickLength = 0.1

clocked :: (Tempo -> IO ()) -> IO ()
clocked callback = do (mt, _) <- listen
                      start <- O.time
                      let st = State {ticks = 0,
                                      start = start,
                                      now = start,
                                      arc = (0,0)
                                     }
                      loop mt st
  where loop mt st =
          do now <- O.time
             t <- readMVar mt
             let next = (start st + (fromIntegral $ (ticks st)+1) * tickLength) - now
             putStrLn "tick"
             putStrLn $ show $ arc st
             threadDelay (floor $ next * 1000000)
             let s = snd $ arc st
                 e = s + (toRational $ tickLength * (cps t))
             loop mt $ st {ticks = (ticks st) + 1, arc = (s,e)}

listen :: IO (MVar Tempo, ThreadId)
listen = do udp <- O.udpServer "127.0.0.1" 0
            addr <- getClockIp
            port <- getClockPort
            remote_addr <- N.inet_addr addr
            let remote_sockaddr = N.SockAddrInet (fromIntegral port) remote_addr
            O.sendTo udp (O.Message "/hello" [O.int32 1]) remote_sockaddr
            t <- defaultTempo
            mt <- newMVar t
            tempoChild <- (forkIO $ listenTempo udp mt)
            return (mt, tempoChild)

listenTempo :: O.UDP -> (MVar Tempo) -> IO ()
listenTempo udp mt = forever $ do pkt <- O.recvPacket udp
                                  act Nothing pkt
                                  return ()
  where act _ (O.Packet_Bundle (O.Bundle ts ms)) = mapM_ (act (Just ts) . O.Packet_Message) ms
        act (Just ts) (O.Packet_Message (O.Message "/cps" [O.Float cps'])) =
          do putStrLn "cps change"
             tempo <- takeMVar mt
             putMVar mt $ tempo {at = ts, cps = realToFrac cps'}
        act _ pkt = putStrLn $ "Unknown packet: " ++ show pkt

{-
clockedTick :: Int -> (Tempo -> Int -> IO ()) -> IO ()
clockedTick tpb callback =
  do (mTempo, _, mCps) <- runClient
     t <- readMVar mTempo
     now <- getCurrentTime
     let delta = realToFrac $ diffUTCTime now (at t)
         beatDelta = cps t * delta
         nowBeat = beat t + beatDelta
         nextTick = ceiling (nowBeat * (fromIntegral tpb))
         -- next4 = nextBeat + (4 - (nextBeat `mod` 4))
     loop mTempo nextTick
  where loop mTempo tick =
          do tempo <- readMVar mTempo
             tick' <- doTick tempo tick
             loop mTempo tick'
        doTick tempo tick | paused tempo =
          do let pause = 0.01
             -- TODO - do this via blocking read on the mvar somehow
             -- rather than polling
             threadDelay $ floor (pause * 1000000)
             -- reset tick to 0 if cps is negative
             return $ if cps tempo < 0 then 0 else tick
                          | otherwise =
          do now <- getCurrentTime
             let tps = (fromIntegral tpb) * cps tempo
                 delta = realToFrac $ diffUTCTime now (at tempo)
                 actualTick = ((fromIntegral tpb) * beat tempo) + (tps * delta)
                 -- only wait by up to two ticks
                 tickDelta = min 2 $ (fromIntegral tick) - actualTick
                 delay = tickDelta / tps
             -- putStrLn $ "tick delta: " ++ show tickDelta
             --putStrLn ("Delay: " ++ show delay ++ "s Beat: " ++ show (beat tempo))
             threadDelay $ floor (delay * 1000000)
             callback tempo tick
             -- putStrLn $ "hmm diff: " ++ show (abs $ (floor actualTick) - tick)
             let newTick | (abs $ (floor actualTick) - tick) > 4 = floor actualTick
                         | otherwise = tick + 1
             return $ newTick
-}

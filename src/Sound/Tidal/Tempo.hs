module Sound.Tidal.Tempo where

-- import Data.Time (getCurrentTime, UTCTime, NominalDiffTime, diffUTCTime, addUTCTime)
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import Safe (readNote)
-- import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Control.Concurrent.MVar
import qualified Sound.Tidal.Pattern as P
import qualified Sound.OSC.FD as O
import qualified Network.Socket as N
import Control.Concurrent (forkIO, ThreadId, threadDelay)
import Control.Monad (forever, when)

data Tempo = Tempo {atTime  :: O.Time,
                    atCycle :: Rational,
                    cps     :: O.Time,
                    paused  :: Bool,
                    nudged  :: Double
                   }

data State = State {ticks   :: Int,
                    start   :: O.Time,
                    nowTime :: O.Time,
                    nowArc  :: P.Arc
                   }

defaultTempo :: O.Time -> Tempo
defaultTempo t = Tempo {atTime   = t,
                        atCycle  = 0,
                        cps      = 2,
                        paused   = True,
                        nudged   = 0
                       }


getClockIp :: IO String
getClockIp = fromMaybe "127.0.0.1" <$> lookupEnv "TIDAL_TEMPO_IP"

getClockPort :: IO Int
getClockPort =
   maybe 9160 (readNote "port parse") <$> lookupEnv "TIDAL_TEMPO_PORT"

-- | Returns the given time in terms of
-- cycles relative to metrical grid of a given Tempo
timeToCycles :: Tempo -> O.Time -> Rational
timeToCycles tempo t = (atCycle tempo) + (toRational cycleDelta)
  where delta = t - (atTime tempo)
        cycleDelta = (realToFrac $ cps tempo) * delta

{-
getCurrentCycle :: MVar Tempo -> IO Rational
getCurrentCycle t = (readMVar t) >>= (cyclesNow) >>= (return . toRational)
-}

tickLength :: O.Time
tickLength = 0.125

clocked :: (Tempo -> State -> IO ()) -> IO ()
clocked callback = do s <- O.time
                      (mt, _) <- listen s
                      let st = State {ticks = 0,
                                      start = s,
                                      nowTime = s,
                                      nowArc = (0,0)
                                     }
                      loop mt st
  where loop mt st =
          do -- putStrLn $ show $ nowArc st

             tempo <- readMVar mt
             let logicalNow = start st + (fromIntegral $ (ticks st)+1) * tickLength
                 s = snd $ nowArc st
                 e = timeToCycles tempo logicalNow
                 st' = st {ticks = (ticks st) + 1, nowArc = (s,e)}
             t <- O.time
             when (t < logicalNow) $ threadDelay (floor $ (logicalNow - t) * 1000000)
             callback tempo st'
             loop mt st'

listen :: O.Time -> IO (MVar Tempo, ThreadId)
listen s = do udp <- O.udpServer "127.0.0.1" 0
              addr <- getClockIp
              port <- getClockPort
              remote_addr <- N.inet_addr addr
              let remote_sockaddr = N.SockAddrInet (fromIntegral port) remote_addr
                  t = defaultTempo s
              O.sendTo udp (O.Message "/hello" [O.int32 (1 :: Int)]) remote_sockaddr
              mt <- newMVar t
              tempoChild <- (forkIO $ listenTempo udp mt)
              return (mt, tempoChild)

listenTempo :: O.UDP -> (MVar Tempo) -> IO ()
listenTempo udp mt = forever $ do pkt <- O.recvPacket udp
                                  act Nothing pkt
                                  return ()
  where act _ (O.Packet_Bundle (O.Bundle ts ms)) = mapM_ (act (Just ts) . O.Packet_Message) ms
        act (Just ts) (O.Packet_Message (O.Message "/cps" [O.Float atCycle',
                                                           O.Float cps',
                                                           O.Int32 paused'
                                                          ]
                                        )
                      ) =
          do putStrLn "cps change"
             tempo <- takeMVar mt
             putMVar mt $ tempo {atTime = ts,
                                 atCycle = realToFrac atCycle',
                                 cps = realToFrac cps',
                                 paused = (paused' == 1)
                                }
        act _ pkt = putStrLn $ "Unknown packet: " ++ show pkt



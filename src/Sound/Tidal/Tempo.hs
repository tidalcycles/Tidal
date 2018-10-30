module Sound.Tidal.Tempo where

-- import Data.Time (getCurrentTime, UTCTime, NominalDiffTime, diffUTCTime, addUTCTime)
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import Safe (readNote)
-- import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Control.Concurrent.MVar
import qualified Sound.Tidal.Pattern as P
import qualified Sound.OSC.FD as O
-- import qualified Sound.OSC.Transport.FD.UDP as O
import qualified Network.Socket as N
import Control.Concurrent (forkIO, ThreadId, threadDelay)
import Control.Monad (forever, when, foldM)
import Data.List (nub,isPrefixOf)

data Tempo = Tempo {atTime  :: O.Time,
                    atCycle :: Rational,
                    cps     :: O.Time,
                    paused  :: Bool,
                    nudged  :: Double
                   }
             deriving Show

data State = State {ticks   :: Int,
                    start   :: O.Time,
                    nowTime :: O.Time,
                    nowArc  :: P.Arc
                   }

setCps :: MVar Tempo -> O.Time -> IO (Tempo)
setCps tempoMV newCps = do t <- O.time
                           tempo <- takeMVar tempoMV
                           let c = timeToCycles tempo t
                               tempo' = tempo {atTime = t,
                                               atCycle = c,
                                               cps = newCps
                                              }
                           putMVar tempoMV $ tempo'
                           return tempo'
            

defaultTempo :: O.Time -> Tempo
defaultTempo t = Tempo {atTime   = t,
                        atCycle  = 0,
                        cps      = 0.5625,
                        paused   = False,
                        nudged   = 0
                       }


getClockHostname :: IO String
getClockHostname = fromMaybe "127.0.0.1" <$> lookupEnv "TIDAL_TEMPO_IP"

getClockPort :: IO Int
getClockPort =
   maybe 9160 (readNote "port parse") <$> lookupEnv "TIDAL_TEMPO_PORT"

-- | Returns the given time in terms of
-- cycles relative to metrical grid of a given Tempo
timeToCycles :: Tempo -> O.Time -> Rational
timeToCycles tempo t = (atCycle tempo) + (toRational cycleDelta)
  where delta = t - (atTime tempo)
        cycleDelta = (realToFrac $ cps tempo) * delta

getHz :: IO Double
getHz = maybe 100 (readNote "Hz parse") <$> lookupEnv "TIDAL_HZ"

getTickLength :: IO O.Time
getTickLength = do hz <- getHz
                   return $ 1/hz

{-
getCurrentCycle :: MVar Tempo -> IO Rational
getCurrentCycle t = (readMVar t) >>= (cyclesNow) >>= (return . toRational)
-}


clocked :: (MVar Tempo -> State -> IO ()) -> IO (MVar Tempo, [ThreadId])
clocked callback = do s <- O.time
                      (tempoMV, listenTid) <- clientListen s
                      let st = State {ticks = 0,
                                      start = s,
                                      nowTime = s,
                                      nowArc = (0,0)
                                     }
                      clockTid <- forkIO $ loop tempoMV st
                      return (tempoMV, [listenTid, clockTid])
  where loop tempoMV st =
          do -- putStrLn $ show $ nowArc ts

             tempo <- readMVar tempoMV
             tickLength <- getTickLength
             let -- 'now' comes from clock ticks, nothing to do with cycles
                 logicalNow = start st + (fromIntegral $ (ticks st)+1) * tickLength
                 -- the tempo is just used to convert logical time to cycles
                 s = snd $ nowArc st
                 e = timeToCycles tempo logicalNow
                 st' = st {ticks = (ticks st) + 1, nowArc = (s,e)}
             t <- O.time
             when (t < logicalNow) $ threadDelay (floor $ (logicalNow - t) * 1000000)
             callback tempoMV st'
             loop tempoMV st'

clientListen :: O.Time -> IO (MVar Tempo, ThreadId)
clientListen s =
  do -- Listen on random port
     udp <- O.udpServer "127.0.0.1" 0
     hostname <- getClockHostname
     port <- getClockPort
     (remote_addr:_) <- N.getAddrInfo Nothing (Just hostname) Nothing
     let (N.SockAddrInet _ a) = N.addrAddress remote_addr
         remote_sockaddr = N.SockAddrInet (fromIntegral port) (a)
         t = defaultTempo s
     -- Send to clock port from same port that's listened to
     putStrLn "sending hello."
     O.sendTo udp (O.Message "/hello" []) remote_sockaddr
     putStrLn "sent."
     -- Make tempo mvar
     tempoMV <- newMVar t
     -- Listen to tempo changes
     tempoChild <- (forkIO $ listenTempo udp tempoMV)
     return (tempoMV, tempoChild)

listenTempo :: O.UDP -> (MVar Tempo) -> IO ()
listenTempo udp tempoMV = forever $ do pkt <- O.recvPacket udp
                                       act Nothing pkt
                                       return ()
  where act _ (O.Packet_Bundle (O.Bundle ts ms)) = mapM_ (act (Just ts) . O.Packet_Message) ms
        act (Just ts) (O.Packet_Message (O.Message "/cps/cycle" [O.Float atCycle',
                                                                 O.Float cps',
                                                                 O.Int32 paused'
                                                                ]
                                        )
                      ) =
          do putStrLn "cps change"
             tempo <- takeMVar tempoMV
             putMVar tempoMV $ tempo {atTime = ts,
                                 atCycle = realToFrac atCycle',
                                 cps = realToFrac cps',
                                 paused = (paused' == 1)
                                }
        act _ pkt = putStrLn $ "Unknown packet: " ++ show pkt


serverListen :: IO ()
serverListen = do port <- getClockPort
                  -- iNADDR_ANY deprecated - what's the right way to do this?
                  udp <- O.udpServer "0.0.0.0" port
                  loop udp []
  where loop udp cs = do (pkt,c) <- O.recvFrom udp
                         cs' <- act udp c Nothing cs pkt
                         loop udp cs'
        act :: O.UDP -> N.SockAddr -> Maybe O.Time -> [N.SockAddr] -> O.Packet -> IO [N.SockAddr]
        act udp c _ cs (O.Packet_Bundle (O.Bundle ts ms)) = foldM (act udp c (Just ts)) cs $ map (O.Packet_Message) ms
        act _ c _ cs (O.Packet_Message (O.Message "/hello" []))
          = do putStrLn $ "hello from " ++ show c
               return $ nub $ c:cs
        act udp _ _ cs (O.Packet_Message (O.Message path params))
          | isPrefixOf "/transmit" path =
              do let path' = drop 9 path
                     msg = O.Message path' params
                 putStrLn $ "transmit " ++ show msg
                 mapM_ (O.sendTo udp msg) cs
                 return cs
        act _ _ _ cs pkt = do putStrLn $ "Unknown packet: " ++ show pkt
                              return cs


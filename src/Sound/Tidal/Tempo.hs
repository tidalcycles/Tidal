{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Sound.Tidal.Tempo where

-- import Data.Time (getCurrentTime, UTCTime, NominalDiffTime, diffUTCTime, addUTCTime)
-- import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Control.Concurrent.MVar
import qualified Sound.Tidal.Pattern as P
import qualified Sound.OSC.FD as O
-- import qualified Sound.OSC.Transport.FD.UDP as O
import qualified Network.Socket as N
import Control.Concurrent (forkIO, ThreadId, threadDelay)
import Control.Monad (forever, when, foldM)
import Data.List (isPrefixOf, nub)
import qualified Control.Exception as E

import Sound.Tidal.Config

data Tempo = Tempo {atTime  :: O.Time,
                    atCycle :: Rational,
                    cps     :: O.Time,
                    paused  :: Bool,
                    nudged  :: Double,
                    localUDP   :: O.UDP,
                    remoteAddr :: N.SockAddr
                   }
           -- deriving Show

-- sendTempo udp tempo remote_sockaddr            
-- 

data State = State {ticks   :: Int,
                    start   :: O.Time,
                    nowTime :: O.Time,
                    nowArc  :: P.Arc
                   }

resetCycles :: MVar Tempo -> IO (Tempo)
resetCycles tempoMV = do t <- O.time
                         tempo <- takeMVar tempoMV
                         let tempo' = tempo {atTime = t,
                                             atCycle = (-0.5)
                                            }
                         sendTempo tempo'
                         putMVar tempoMV $ tempo'
                         return tempo'


setCps :: MVar Tempo -> O.Time -> IO (Tempo)
setCps tempoMV newCps = do t <- O.time
                           tempo <- takeMVar tempoMV
                           let c = timeToCycles tempo t
                               tempo' = tempo {atTime = t,
                                               atCycle = c,
                                               cps = newCps
                                              }
                           sendTempo tempo'
                           -- TODO - should we set the tempo ASAP rather than waiting for (possibly failing) network round trip?
                           putMVar tempoMV $ tempo'
                           return tempo'

defaultTempo :: O.Time -> O.UDP -> N.SockAddr -> Tempo
defaultTempo t local remote = Tempo {atTime   = t,
                                     atCycle  = 0,
                                     cps      = 0.5625,
                                     paused   = False,
                                     nudged   = 0,
                                     localUDP   = local,
                                     remoteAddr = remote
                                    }

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


clocked :: Config -> (MVar Tempo -> State -> IO ()) -> IO (MVar Tempo, [ThreadId])
clocked config callback
  = do s <- O.time
       -- TODO - do something with thread id
       _ <- serverListen config
       (tempoMV, listenTid) <- clientListen config s
       let st = State {ticks = 0,
                       start = s,
                       nowTime = s,
                       nowArc = (P.Arc 0 0)
                      }
       clockTid <- forkIO $ loop tempoMV st
       return (tempoMV, [listenTid, clockTid])
  where loop tempoMV st =
          do -- putStrLn $ show $ nowArc ts
             tempo <- readMVar tempoMV
             let frameTimespan = cFrameTimespan config
             let -- 'now' comes from clock ticks, nothing to do with cycles
                 logicalNow = start st + (fromIntegral $ (ticks st)+1) * frameTimespan
                 -- the tempo is just used to convert logical time to cycles
                 s = P.stop $ nowArc st
                 e = timeToCycles tempo logicalNow
                 st' = st {ticks = (ticks st) + 1, nowArc = P.Arc s e}
             t <- O.time
             when (t < logicalNow) $ threadDelay (floor $ (logicalNow - t) * 1000000)
             callback tempoMV st'
             loop tempoMV st'

clientListen :: Config -> O.Time -> IO (MVar Tempo, ThreadId)
clientListen config s =
  do -- Listen on random port
     local <- O.udpServer "127.0.0.1" 0
     let hostname = cTempoAddr config
         port = cTempoPort config
     (remote_addr:_) <- N.getAddrInfo Nothing (Just hostname) Nothing
     let (N.SockAddrInet _ a) = N.addrAddress remote_addr
         remote = N.SockAddrInet (fromIntegral port) (a)
         t = defaultTempo s local remote
     -- Send to clock port from same port that's listened to
     O.sendTo local (O.p_message "/hello" []) remote
     -- Make tempo mvar
     tempoMV <- newMVar t
     -- Listen to tempo changes
     tempoChild <- (forkIO $ listenTempo local tempoMV)
     return (tempoMV, tempoChild)

sendTempo :: Tempo -> IO ()
sendTempo tempo = O.sendTo (localUDP tempo) (O.p_bundle (atTime tempo) [m]) (remoteAddr tempo)
  where m = O.Message "/transmit/cps/cycle" [O.Float $ fromRational $ atCycle tempo,
                                             O.Float $ realToFrac $ cps tempo,
                                             O.Int32 $ if (paused tempo) then 1 else 0
                                            ]
 
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
          do tempo <- takeMVar tempoMV
             putMVar tempoMV $ tempo {atTime = ts,
                                      atCycle = realToFrac atCycle',
                                      cps = realToFrac cps',
                                      paused = (paused' == 1)
                                     }
        act _ pkt = putStrLn $ "Unknown packet: " ++ show pkt

serverListen :: Config -> IO (Maybe ThreadId)
serverListen config = catchAny (run) (\_ -> do putStrLn $ "Tempo listener failed (is one already running?)"
                                               return Nothing
                                     )
  where run = do let port = cTempoPort config
                 -- iNADDR_ANY deprecated - what's the right way to do this?
                 udp <- O.udpServer "0.0.0.0" port
                 tid <- forkIO $ loop udp []
                 return $ Just tid
        loop udp cs = do (pkt,c) <- O.recvFrom udp
                         cs' <- act udp c Nothing cs pkt
                         loop udp cs'
        act :: O.UDP -> N.SockAddr -> Maybe O.Time -> [N.SockAddr] -> O.Packet -> IO [N.SockAddr]
        act udp c _ cs (O.Packet_Bundle (O.Bundle ts ms)) = foldM (act udp c (Just ts)) cs $ map (O.Packet_Message) ms
        act _ c _ cs (O.Packet_Message (O.Message "/hello" []))
          = return $ nub $ c:cs
        act udp _ (Just ts) cs (O.Packet_Message (O.Message path params))
          | isPrefixOf "/transmit" path =
              do let path' = drop 9 path
                     msg = O.Message path' params
                 mapM_ (O.sendTo udp $ O.p_bundle ts [msg]) cs
                 return cs
        act _ _ _ cs pkt = do putStrLn $ "Unknown packet: " ++ show pkt
                              return cs
        catchAny :: IO a -> (E.SomeException -> IO a) -> IO a
        catchAny = E.catch

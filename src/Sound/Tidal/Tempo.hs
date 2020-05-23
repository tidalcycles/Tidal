{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}

module Sound.Tidal.Tempo where

import Control.Concurrent.MVar
import qualified Sound.Tidal.Pattern as P
import qualified Sound.OSC.FD as O
import qualified Network.Socket as N
import Control.Concurrent (forkIO, ThreadId, threadDelay)
import Control.Monad (forever, when, foldM)
import Data.List (nub)
import qualified Control.Exception as E
import Sound.Tidal.Config
import Sound.Tidal.Utils (writeError)

instance Show O.UDP where
  show _ = "-unshowable-"

data Tempo = Tempo {atTime  :: O.Time,
                    atCycle :: Rational,
                    cps     :: O.Time,
                    paused  :: Bool,
                    nudged  :: Double,
                    localUDP   :: O.UDP,
                    remoteAddr :: N.SockAddr,
                    synched :: Bool
                   }
  deriving Show

data State = State {ticks   :: Int,
                    start   :: O.Time,
                    nowTimespan :: (O.Time, O.Time),
                    nowArc  :: P.Arc,
                    starting :: Bool
                   }
  deriving Show

changeTempo :: MVar Tempo -> (O.Time -> Tempo -> Tempo) -> IO Tempo
changeTempo tempoMV f = do t <- O.time
                           tempo <- takeMVar tempoMV
                           let tempo' = f t $ tempo
                           sendTempo tempo'
                           putMVar tempoMV tempo'
                           return tempo'

changeTempo' :: Tempo -> O.Time -> Rational -> Tempo
changeTempo' tempo newCps cyc = tempo {atTime = cyclesToTime tempo cyc,
                                       cps = newCps,
                                       atCycle = cyc
                                      }

resetCycles :: MVar Tempo -> IO Tempo
resetCycles tempoMV = changeTempo tempoMV (\t tempo -> tempo {atTime = t, atCycle = 0})

setCps :: MVar Tempo -> O.Time -> IO Tempo
setCps tempoMV newCps = changeTempo tempoMV (\t tempo -> tempo {atTime = t,
                                                                atCycle = timeToCycles tempo t,
                                                                cps = newCps
                                                               })

defaultCps :: O.Time
defaultCps = 0.5625

defaultTempo :: O.Time -> O.UDP -> N.SockAddr -> Tempo
defaultTempo t local remote = Tempo {atTime   = t,
                                     atCycle  = 0,
                                     cps      = defaultCps,
                                     paused   = False,
                                     nudged   = 0,
                                     localUDP   = local,
                                     remoteAddr = remote,
                                     synched = False
                                    }

-- | Returns the given time in terms of
-- cycles relative to metrical grid of a given Tempo
timeToCycles :: Tempo -> O.Time -> Rational
timeToCycles tempo t = atCycle tempo + toRational cycleDelta
  where delta = t - atTime tempo
        cycleDelta = realToFrac (cps tempo) * delta

cyclesToTime :: Tempo -> Rational -> O.Time
cyclesToTime tempo cyc = atTime tempo + (fromRational timeDelta)
  where cycleDelta = cyc - atCycle tempo
        timeDelta = cycleDelta / (toRational $ cps tempo)

{-
getCurrentCycle :: MVar Tempo -> IO Rational
getCurrentCycle t = (readMVar t) >>= (cyclesNow) >>= (return . toRational)
-}

clocked :: Config -> MVar Tempo -> (State -> IO ()) -> IO [ThreadId]
clocked config tempoMV callback
  = do s <- O.time
       -- TODO - do something with thread id
       _ <- serverListen config
       listenTid <- clientListen config tempoMV s
       let st = State {ticks = 0,
                       start = s,
                       nowTimespan = (s, s + frameTimespan),
                       nowArc = P.Arc 0 0,
                       starting = True
                      }
       clockTid <- forkIO $ loop st
       return [listenTid, clockTid]
  where frameTimespan :: Double
        frameTimespan = cFrameTimespan config
        loop st =
          do -- putStrLn $ show $ nowArc ts
             tempo <- readMVar tempoMV               
             t <- O.time
             let logicalT ticks' = start st + fromIntegral ticks' *  frameTimespan
                 logicalNow = logicalT $ ticks st + 1
                 -- Wait maximum of two frames
                 delta = min (frameTimespan * 2) (logicalNow - t)
                 e = timeToCycles tempo logicalNow
                 s = if starting st && synched tempo
                     then timeToCycles tempo (logicalT $ ticks st)
                     else P.stop $ nowArc st
             when (t < logicalNow) $ threadDelay (floor $ delta * 1000000)
             t' <- O.time
             let actualTick = floor $ (t' - start st) / frameTimespan
                 -- reset ticks if ahead/behind by skipTicks or more
                 ahead = (abs $ actualTick - ticks st) > (cSkipTicks config)
                 newTick | ahead = actualTick
                         | otherwise = (ticks st) + 1
                 st' = st {ticks = newTick,
                           nowArc = P.Arc s e,
                           nowTimespan = (logicalNow,  logicalNow + frameTimespan),
                           starting = not (synched tempo)
                          }
             when ahead $ writeError $ "skip: " ++ show (actualTick - ticks st)
             callback st'
             {-putStrLn ("actual tick: " ++ show actualTick
                       ++ " old tick: " ++ show (ticks st)
                       ++ " new tick: " ++ show newTick
                      )-}
             loop st'

clientListen :: Config -> MVar Tempo -> O.Time -> IO (ThreadId)
clientListen config tempoMV s =
  do -- Listen on random port
     let tempoClientPort = cTempoClientPort config
         hostname = cTempoAddr config
         port = cTempoPort config
     (remote_addr:_) <- N.getAddrInfo Nothing (Just hostname) Nothing
     local <- O.udpServer "0.0.0.0" tempoClientPort
     let (N.SockAddrInet _ a) = N.addrAddress remote_addr
         remote = N.SockAddrInet (fromIntegral port) a
         t = defaultTempo s local remote
     putMVar tempoMV t
     -- Send to clock port from same port that's listened to
     O.sendTo local (O.p_message "/hello" []) remote
     -- Make tempo mvar
     -- Listen to tempo changes
     tempoChild <- forkIO $ listenTempo local tempoMV
     return tempoChild

sendTempo :: Tempo -> IO ()
sendTempo tempo = O.sendTo (localUDP tempo) (O.p_bundle (atTime tempo) [m]) (remoteAddr tempo)
  where m = O.Message "/transmit/cps/cycle" [O.Float $ fromRational $ atCycle tempo,
                                             O.Float $ realToFrac $ cps tempo,
                                             O.Int32 $ if paused tempo then 1 else 0
                                            ]
  
listenTempo :: O.UDP -> MVar Tempo -> IO ()
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
                                      paused = paused' == 1,
                                      synched = True
                                     }
        act _ pkt = writeError $ "Unknown packet (client): " ++ show pkt

serverListen :: Config -> IO (Maybe ThreadId)
serverListen config = catchAny run (\_ -> return Nothing) -- probably just already running)
  where run = do let port = cTempoPort config
                 -- iNADDR_ANY deprecated - what's the right way to do this?
                 udp <- O.udpServer "0.0.0.0" port
                 cpsMessage <- defaultCpsMessage
                 tid <- forkIO $ loop udp ([], cpsMessage)
                 return $ Just tid
        loop udp (cs, msg) = do (pkt,c) <- O.recvFrom udp
                                (cs', msg') <- act udp c Nothing (cs,msg) pkt
                                loop udp (cs', msg')
        act :: O.UDP -> N.SockAddr -> Maybe O.Time -> ([N.SockAddr], O.Packet) -> O.Packet -> IO ([N.SockAddr], O.Packet)
        act udp c _ (cs,msg) (O.Packet_Bundle (O.Bundle ts ms)) = foldM (act udp c (Just ts)) (cs,msg) $ map O.Packet_Message ms
        act udp c _ (cs,msg) (O.Packet_Message (O.Message "/hello" []))
          = do O.sendTo udp msg c 
               return (nub (c:cs),msg)
        act udp _ (Just ts) (cs,_) (O.Packet_Message (O.Message "/transmit/cps/cycle" params)) =
          do let path' = "/cps/cycle"
                 msg' = O.p_bundle ts [O.Message path' params]
             mapM_ (O.sendTo udp msg') cs
             return (cs, msg')
        act _ x _ (cs,msg) pkt = do writeError $ "Unknown packet (serv): " ++ show pkt ++ " / " ++ (show x)
                                    return (cs,msg)
        catchAny :: IO a -> (E.SomeException -> IO a) -> IO a
        catchAny = E.catch
        defaultCpsMessage = do ts <- O.time
                               return $ O.p_bundle ts [O.Message "/cps/cycle" [O.Float $ 0,
                                                                               O.Float $ realToFrac $ defaultCps,
                                                                               O.Int32 0
                                                                              ]
                                                    ]



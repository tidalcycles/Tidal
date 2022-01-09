{-# LANGUAGE ConstraintKinds, GeneralizedNewtypeDeriving, FlexibleContexts, ScopedTypeVariables, BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns -fno-warn-orphans #-}


module Sound.Tidal.Tempo where

import Control.Concurrent.MVar
-- import Control.Concurrent.Chan
import qualified Sound.Tidal.Pattern as P
import qualified Sound.OSC.FD as O
-- import qualified Network.Socket as N
import Control.Concurrent (forkIO, ThreadId, threadDelay)
import Control.Monad (when)
import qualified Data.Map.Strict as Map
-- import Control.Monad (forever, when, foldM)
-- import Data.List (nub)
import qualified Control.Exception as E
import Sound.Tidal.ID
import Sound.Tidal.Config
import Sound.Tidal.Utils (writeError)
import Foreign.Ptr
import qualified Sound.Tidal.Link as Link
import Foreign.C.Types (CDouble(..))
import Data.Coerce (coerce)
import System.IO (hPutStrLn, stderr)

{-
    Tempo.hs - Tidal's scheduler
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

instance Show O.UDP where
  show _ = "-unshowable-"

-- atTime is at what time tempo what changed.
-- In case of change via pattern, time of event start is used.
-- In case of resetCycle, time of processing the cycle reset is used.
data Tempo = Tempo {atTime  :: O.Time,
                    atCycle :: Rational,
                    cps     :: O.Time,
                    paused  :: Bool,
                    nudged  :: Double,
                    -- localUDP   :: O.UDP,
                    -- remoteAddr :: N.SockAddr,
                    synched :: Bool
                   }
  deriving Show

instance Eq Tempo where
  (==) t t' = and [(atTime t)  == (atTime t'),
                   (atCycle t) == (atCycle t'),
                   (cps t)     == (cps t'),
                   (paused t)  == (paused t'),
                   (nudged t)  == (nudged t')
                  ]

data TempoAction =
  ResetCycles
  | SingleTick P.ControlPattern
  | SetNudge Double
  | StreamReplace ID P.ControlPattern

data State = State {ticks   :: Int,
                    start   :: O.Time,
                    nowTimespan :: (O.Time, O.Time),
                    nowArc  :: P.Arc,
                    starting :: Bool
                   }
  deriving Show

data ActionHandler =
  ActionHandler {
    onTick :: State -> Tempo -> P.ValueMap -> IO (Tempo, P.ValueMap),
    onSingleTick :: State -> Tempo -> P.ValueMap -> P.ControlPattern -> IO (Tempo, P.ValueMap),
    updatePattern :: ID -> P.ControlPattern -> IO ()
  }

changeTempo' :: Tempo -> O.Time -> Rational -> Tempo
changeTempo' tempo newCps cyc = tempo {atTime = cyclesToTime tempo cyc,
                                       cps = newCps,
                                       atCycle = cyc
                                      }

resetCycles :: MVar [TempoAction] -> IO ()
resetCycles actionsMV = modifyMVar_ actionsMV (\actions -> return $ ResetCycles : actions)

setNudge :: MVar [TempoAction] -> Double -> IO ()
setNudge actionsMV nudge = modifyMVar_ actionsMV (\actions -> return $ SetNudge nudge : actions)

defaultCps :: O.Time
defaultCps = 0.5625

{-
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
-}

defaultTempo :: O.Time -> Tempo
defaultTempo t = Tempo {atTime   = t,
                                     atCycle  = 0,
                                     cps      = defaultCps,
                                     paused   = False,
                                     nudged   = 0,
                                     synched = False
                                    }

-- | Returns the given time in terms of
-- cycles relative to metrical grid of a given Tempo
timeToCycles :: Tempo -> O.Time -> Rational
timeToCycles tempo t = atCycle tempo + toRational cycleDelta
  where delta = t - atTime tempo
        cycleDelta = realToFrac (cps tempo) * delta

cyclesToTime :: Tempo -> Rational -> O.Time
cyclesToTime tempo cyc = atTime tempo + fromRational timeDelta
  where cycleDelta = cyc - atCycle tempo
        timeDelta = cycleDelta / toRational (cps tempo)

-- clocked assumes tempoMV is empty
clocked :: Config -> MVar Tempo -> MVar P.ValueMap -> MVar [TempoAction] -> ActionHandler -> IO [ThreadId]
clocked config tempoMV stateMV actionsMV ac
  = do s <- O.time
       -- TODO - do something with thread id
       -- _ <- serverListen config
       -- clientListen assumes tempoMV is empty
       -- listenTid <- clientListen config tempoMV s
       let st = State {ticks = 0,
                       start = s,
                       nowTimespan = (s, s + frameTimespan),
                       nowArc = P.Arc 0 0,
                       starting = True
                      }
       let t = defaultTempo s
       clockTid <- forkIO $ loopInit st t
       -- return [listenTid, clockTid]
       return [clockTid]
  where frameTimespan :: Double
        frameTimespan = cFrameTimespan config
        -- create startloop function and create the link wrapper there
        loopInit :: State -> Tempo -> IO a
        loopInit st t =
          do
            print "Creating wrapper"
            let bpm = coerce $ (cps t) * 60
            al <- Link.create bpm
            print "Wrapper created. Enabling link."
            was_enabled <- Link.enable al
            sessionState <- Link.createSessionState
            Link.captureAppSessionState al sessionState
            now <- Link.clock al
            
            Link.destroySessionState sessionState
            putMVar actionsMV []
            putMVar tempoMV t
            loop st al t
        -- Time is processed at a fixed rate according to configuration
        -- logicalTime gives the time when a tick starts based on when
        -- processing first started.
        logicalTime :: O.Time -> Int -> O.Time
        logicalTime startTime ticks' = startTime + fromIntegral ticks' * frameTimespan
        loop :: State -> Link.AbletonLink -> Tempo -> IO a 
        loop st lw tempo =
          do
            t <- O.time
            let logicalStart = logicalTime (start st) $ ticks st
                logicalEnd   = logicalTime (start st) $ ticks st + 1
                -- Wait maximum of two frames
                delta = min (frameTimespan * 2) (logicalEnd - t)
                e = timeToCycles tempo logicalEnd
                s = if starting st && synched tempo
                    then timeToCycles tempo logicalStart
                    else P.stop $ nowArc st
            when (t < logicalEnd) $ threadDelay (floor $ delta * 1000000)
            t' <- O.time
            let actualTick = floor $ (t' - start st) / frameTimespan
                -- reset ticks if ahead/behind by skipTicks or more
                ahead = abs (actualTick - ticks st) > cSkipTicks config
                newTick | ahead = actualTick
                        | otherwise = ticks st + 1
                st' = st {ticks = newTick,
                          nowArc = P.Arc s e,
                          nowTimespan = (logicalEnd,  logicalEnd + frameTimespan),
                          starting = not (synched tempo)
                        }
            when ahead $ writeError $ "skip: " ++ (show (actualTick - ticks st))
            streamState <- takeMVar stateMV
            actions <- swapMVar actionsMV [] 
            -- tempo <- takeMVar tempoMV
            (st'', tempo', streamState') <- handleActions st' t' actions (tempo, streamState)
            (tempo'', streamState'') <- (onTick ac) st'' tempo' streamState'
            {-when (tempo /= tempo') $ do
              sendTempo tempo'
              when (cps tempo /= cps tempo') $ do
                let newBpm = coerce $ (cps tempo) * 60
                set_tempo_at_beat lw newBpm 0-}
            when (cps tempo /= cps tempo'') $ do
              let newBpm = coerce $ (cps tempo'') * 60
              setTempoNow lw newBpm
            putMVar stateMV streamState''
            _ <- swapMVar tempoMV tempo''
            {- putStrLn ("actual tick: " ++ show actualTick
                       ++ " old tick: " ++ show (ticks st)
                       ++ " new tick: " ++ show newTick
                      )-}
            loop st'' lw tempo''
        setTempoNow :: Link.AbletonLink -> Link.BPM -> IO ()
        setTempoNow al bpm = do
          sessionState <- Link.createSessionState
          Link.captureAppSessionState al sessionState
          now <- Link.clock al
          Link.setTempo sessionState bpm now
          Link.commitAppSessionState al sessionState
          Link.destroySessionState sessionState
        handleActions :: State -> O.Time -> [TempoAction] -> (Tempo, P.ValueMap) -> IO (State, Tempo, P.ValueMap)
        handleActions st _ [] (tempo, streamState) = return (st, tempo, streamState)
        handleActions st t (ResetCycles : otherActions) ts =
          do
            (st', tempo', streamState') <- handleActions st t otherActions ts
            let tempo'' = tempo' { atTime = t, atCycle = 0 }
            let logicalEnd   = logicalTime (start st') $ ticks st' + 1
                e = timeToCycles tempo'' logicalEnd
                st'' = st' {
                          nowArc = P.Arc 0 e,
                          nowTimespan = (logicalEnd,  logicalEnd + frameTimespan),
                          starting = not (synched tempo'')
                        }
            return (st'', tempo'', streamState')
        handleActions st t (SingleTick pat : otherActions) ts =
          do
            (st', tempo', streamState') <- handleActions st t otherActions ts
            (tempo'', streamState'') <- (onSingleTick ac) st' tempo' streamState' pat
            return (st', tempo'', streamState'')
        handleActions st t (SetNudge nudge : otherActions) ts =
          do
            (st', tempo', streamState') <- handleActions st t otherActions ts
            let tempo'' = tempo' {nudged = nudge}
            return (st', tempo'', streamState')
        handleActions st t (StreamReplace k pat : otherActions) ts =
          do
            (st', tempo', streamState') <- handleActions st t otherActions ts
            E.catch (
              do
                -- put pattern id and change time in control input
                let cyc = timeToCycles tempo' t
                let streamState'' = Map.insert ("_t_all") (P.VR cyc) $ Map.insert ("_t_" ++ fromID k) (P.VR cyc) streamState'
                (updatePattern ac) k pat
                return (st', tempo', streamState'')
              )
              (\(e :: E.SomeException) -> do
                hPutStrLn stderr $ "Error in pattern: " ++ show e
                return (st', tempo', streamState')
              )

{-
-- clientListen assumes tempoMV is empty
clientListen :: Config -> MVar Tempo -> O.Time -> IO ThreadId
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
     print "Write clientListen"
     putMVar tempoMV t
     -- Send to clock port from same port that's listened to
     O.sendTo local (O.p_message "/hello" []) remote
     -- Make tempo mvar
     -- Listen to tempo changes
     tid <- forkIO $ listenTempo local tempoMV

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
        act _ x _ (cs,msg) pkt = do writeError $ "Unknown packet (serv): " ++ show pkt ++ " / " ++ show x
                                    return (cs,msg)
        catchAny :: IO a -> (E.SomeException -> IO a) -> IO a
        catchAny = E.catch
        defaultCpsMessage = do ts <- O.time
                               return $ O.p_bundle ts [O.Message "/cps/cycle" [O.Float 0,
                                                                               O.Float $ realToFrac defaultCps,
                                                                               O.Int32 0
                                                                              ]
                                                    ]


-}
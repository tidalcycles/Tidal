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
import qualified Sound.Tidal.Link as Link
import Foreign.C.Types (CDouble(..))
import Data.Coerce (coerce)
import System.IO (hPutStrLn, stderr)
import Data.Int(Int64)
import Debug.Trace (trace)

import Sound.Tidal.StreamTypes
import Sound.Tidal.Core (silence)

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

type TransitionMapper = P.Time -> [P.ControlPattern] -> P.ControlPattern

data TempoAction =
  ResetCycles
  | SingleTick P.ControlPattern
  | SetNudge Double
  | StreamReplace ID P.ControlPattern
  | Transition Bool TransitionMapper ID P.ControlPattern

data State = State {ticks    :: Int64,
                    start    :: Link.Micros,
                    nowEnd   :: Link.Micros,
                    nowArc   :: P.Arc,
                    nudged   :: Double,
                    al       :: Link.AbletonLink
                   }
  deriving Show

data ActionHandler =
  ActionHandler {
    onTick :: State -> LinkOperations -> P.ValueMap -> IO P.ValueMap,
    onSingleTick :: Link.Micros -> LinkOperations -> P.ValueMap -> P.ControlPattern -> IO P.ValueMap,
    updatePattern :: ID -> P.ControlPattern -> IO ()
  }

data LinkOperations =
  LinkOperations {
    timeAtBeat :: Link.Beat -> Link.Quantum -> IO Link.Micros,
    beatAtTime :: Link.Micros -> Link.Quantum -> IO Link.Beat,
    getTempo :: IO Link.BPM,
    setTempo :: Link.BPM -> Link.Micros -> IO (),
    linkToOscTime :: Link.Micros -> O.Time
  }

resetCycles :: MVar [TempoAction] -> IO ()
resetCycles actionsMV = modifyMVar_ actionsMV (\actions -> return $ ResetCycles : actions)

setNudge :: MVar [TempoAction] -> Double -> IO ()
setNudge actionsMV nudge = modifyMVar_ actionsMV (\actions -> return $ SetNudge nudge : actions)

defaultCps :: O.Time
defaultCps = 0.5625

timeToCycles :: LinkOperations -> Link.Micros -> IO P.Time
timeToCycles ops time = do
  beat <- (beatAtTime ops) time bpc
  return $! (toRational beat) / (toRational bpc)

timeToCycles' :: Link.SessionState -> Link.Micros -> IO P.Time
timeToCycles' ss time = do
  beat <- Link.beatAtTime ss time bpc
  return $! (toRational beat) / (toRational bpc)

cyclesToTime :: Link.SessionState -> P.Time -> IO Link.Micros
cyclesToTime ss cyc = do
  let beat = (fromRational cyc) * bpc
  Link.timeAtBeat ss beat bpc

addMicrosToOsc :: Link.Micros -> O.Time -> O.Time
addMicrosToOsc m t = ((fromIntegral m) / 1000000) + t

-- clocked assumes tempoMV is empty
clocked :: Config -> MVar P.ValueMap -> MVar PlayMap -> MVar [TempoAction] -> ActionHandler -> IO [ThreadId]
clocked config stateMV mapMV actionsMV ac
  = do -- TODO - do something with thread id
       -- _ <- serverListen config
       -- clientListen assumes tempoMV is empty
       -- listenTid <- clientListen config tempoMV s
      clockTid <- forkIO $ loopInit
       -- return [listenTid, clockTid]
      return $! [clockTid]
  where frameTimespan :: Link.Micros
        frameTimespan = cFrameTimespan config
        -- create startloop function and create the link wrapper there
        loopInit :: IO a
        loopInit =
          do
            let bpm = (coerce defaultCps) * 60 * bpc
            abletonLink <- Link.create bpm
            Link.enable abletonLink
            sessionState <- Link.createAndCaptureAppSessionState abletonLink
            now <- Link.clock abletonLink
            let startAt = now + processAhead
            Link.requestBeatAtTime sessionState 0 startAt bpc
            Link.commitAppSessionState abletonLink sessionState
            putMVar actionsMV []
            let st = State {ticks = 0,
                       start = now,
                       nowEnd = logicalTime now 1,
                       nowArc = P.Arc 0 0,
                       nudged = 0,
                       al = abletonLink
                      }
            checkArc $! st
        -- Time is processed at a fixed rate according to configuration
        -- logicalTime gives the time when a tick starts based on when
        -- processing first started.
        logicalTime :: Link.Micros -> Int64 -> Link.Micros
        logicalTime startTime ticks' = startTime + ticks' * frameTimespan
        -- tick moves the logical time forward or recalculates the ticks in case
        -- the logical time is out of sync with Link time.
        -- tick delays the thread when logical time is ahead of Link time.
        tick :: State -> IO a
        tick st = do
          now <- Link.clock (al st)
          let preferredNewTick = ticks st + 1
              logicalNow = logicalTime (start st) preferredNewTick
              aheadOfNow = now - processAhead
              actualTick = (aheadOfNow - start st) `div` frameTimespan
              drifted    = abs (actualTick - preferredNewTick) > cSkipTicks config
              newTick | drifted   = actualTick
                      | otherwise = preferredNewTick
              st' = st {ticks = newTick}
              delta = min frameTimespan (logicalNow - aheadOfNow)
          if drifted
            then writeError $ "skip: " ++ (show (actualTick - ticks st))
            else when (delta > 0) $ threadDelay $ fromIntegral delta
          checkArc st'
        -- The reference time Link uses,
        -- is the time the audio for a certain beat hits the speaker.
        -- Processing of the nowArc should happen early enough for
        -- all events in the nowArc to hit the speaker, but not too early.
        -- Processing thus needs to happen a short while before the start
        -- of nowArc. For now, this short while = frameTimespan.
        -- The nowArc that will be used starts at the current end and ends
        -- at the cycle where we predict it will end at.
        -- checkArc should therefore look at the end of the currently stored
        -- nowArc rather than at the start of it.
        -- If processing is late, we anyway keep processing at the same rate.
        -- Ticks are only skipped if processing gets very out of sync.
        -- When ticks are skipped, the events are still processed,
        -- but multiple frames are merged.
        -- It is possible for the nowArc to be several ticks in the future.
        -- This likely only happens when we are starting up or have reset
        -- the cycles, but perhaps it can also happen if bpm is changed
        -- drastically?
        -- When it happens, we just wait for time to pass until the nowArc
        -- is close enough in time.
        processAhead :: Link.Micros
        processAhead = cProcessAhead config
        checkArc :: State -> IO a
        checkArc st = do
          actions <- swapMVar actionsMV [] 
          st' <- processActions st actions
          let logicalEnd = logicalTime (start st') $ ticks st' + 1
              nextArcStartCycle = P.stop $ nowArc st'
          ss <- Link.createAndCaptureAppSessionState (al st')
          arcStartTime <- cyclesToTime ss nextArcStartCycle
          Link.destroySessionState ss
          if (arcStartTime < logicalEnd)
            then processArc st'
            else tick st'
        processArc :: State -> IO a 
        processArc st =
          do
            streamState <- takeMVar stateMV
            let logicalEnd   = logicalTime (start st) $ ticks st + 1
                startCycle = P.stop $ nowArc st
            sessionState <- Link.createAndCaptureAppSessionState (al st)
            endCycle <- timeToCycles' sessionState logicalEnd
            let st' = st {nowArc = P.Arc startCycle endCycle,
                          nowEnd = logicalEnd
                        }
            nowOsc <- O.time
            nowLink <- Link.clock (al st)
            let ops = LinkOperations {
              timeAtBeat = Link.timeAtBeat sessionState,
              beatAtTime = Link.beatAtTime sessionState,
              getTempo = Link.getTempo sessionState,
              setTempo = Link.setTempo sessionState,
              linkToOscTime = \lt -> addMicrosToOsc (nowLink - lt) nowOsc
            }
            streamState' <- (onTick ac) st' ops streamState
            Link.commitAndDestroyAppSessionState (al st) sessionState
            putMVar stateMV streamState'
            tick st'
        processActions :: State -> [TempoAction] -> IO State
        processActions st [] = return $! st
        processActions st actions = do
          streamState <- takeMVar stateMV
          (st', streamState') <- handleActions st actions streamState
          putMVar stateMV streamState'
          return $! st'
        handleActions :: State -> [TempoAction] -> P.ValueMap -> IO (State, P.ValueMap)
        handleActions st [] streamState = return (st, streamState)
        handleActions st (ResetCycles : otherActions) streamState =
          do
            (st', streamState') <- handleActions st otherActions streamState
            sessionState <- Link.createAndCaptureAppSessionState (al st)

            let logicalEnd   = logicalTime (start st') $ ticks st' + 1
                -- e = timeToCycles' tempo'' logicalEnd
                st'' = st' {
                          nowArc = P.Arc 0 0,
                          nowEnd = logicalEnd + frameTimespan
                        }
            now <- Link.clock (al st)
            Link.requestBeatAtTime sessionState 0 now bpc
            Link.commitAndDestroyAppSessionState (al st) sessionState
            return (st'', streamState')
        handleActions st (SingleTick pat : otherActions) streamState =
          do
            (st', streamState') <- handleActions st otherActions streamState
            -- onSingleTick assumes it runs at beat 0.
            -- The best way to achieve that is to use forceBeatAtTime.
            -- But using forceBeatAtTime means we can not commit its session state.
            -- Another session state, which we will commit,
            -- is introduced to keep track of tempo changes.
            sessionState <- Link.createAndCaptureAppSessionState (al st)
            zeroedSessionState <- Link.createAndCaptureAppSessionState (al st)
            nowOsc <- O.time
            nowLink <- Link.clock (al st)
            Link.forceBeatAtTime zeroedSessionState 0 nowLink bpc
            let ops = LinkOperations {
              timeAtBeat = Link.timeAtBeat zeroedSessionState,
              beatAtTime = Link.beatAtTime zeroedSessionState,
              getTempo = Link.getTempo zeroedSessionState,
              setTempo = \bpm micros ->
                            Link.setTempo zeroedSessionState bpm micros >>
                            Link.setTempo sessionState bpm micros,
              linkToOscTime = \lt -> addMicrosToOsc (nowLink - lt) nowOsc
            }
            streamState'' <- (onSingleTick ac) nowLink ops streamState' pat
            Link.commitAndDestroyAppSessionState (al st) sessionState
            Link.destroySessionState zeroedSessionState
            return (st', streamState'')
        handleActions st (SetNudge nudge : otherActions) streamState =
          do
            (st', streamState') <- handleActions st otherActions streamState
            let st'' = st' {nudged = nudge}
            return (st'', streamState')
        handleActions st (StreamReplace k pat : otherActions) streamState =
          do
            (st', streamState') <- handleActions st otherActions streamState
            E.catch (
              do
                now <- Link.clock (al st')
                sessionState <- Link.createAndCaptureAppSessionState (al st')
                beat <- Link.beatAtTime sessionState now bpc
                Link.destroySessionState sessionState
                let cyc = beat / bpc
                -- put pattern id and change time in control input
                let streamState'' = Map.insert ("_t_all") (P.VR $! toRational cyc) $ Map.insert ("_t_" ++ fromID k) (P.VR $! toRational cyc) streamState'
                (updatePattern ac) k pat
                return (st', streamState'')
              )
              (\(e :: E.SomeException) -> do
                hPutStrLn stderr $ "Error in pattern: " ++ show e
                return (st', streamState')
              )
        handleActions st (Transition historyFlag f patId pat : otherActions) streamState =
          do
            (st', streamState') <- handleActions st otherActions streamState
            let
              appendPat flag = if flag then (pat:) else id
              updatePS (Just playState) = playState {history = (appendPat historyFlag) (history playState)}
              updatePS Nothing = PlayState {pattern = silence,
                                            mute = False,
                                            solo = False,
                                            history = (appendPat historyFlag) (silence:[])
                                          }
              transition' pat' = do now <- Link.clock (al st')
                                    ss <- Link.createAndCaptureAppSessionState (al st')
                                    c <- timeToCycles' ss now
                                    return $! f c pat'
            pMap <- readMVar mapMV
            let playState = updatePS $ Map.lookup (fromID patId) pMap
            pat' <- transition' $ appendPat (not historyFlag) (history playState)
            let pMap' = Map.insert (fromID patId) (playState {pattern = pat'}) pMap
            _ <- swapMVar mapMV pMap'
            return (st', streamState')

bpc :: Link.Quantum
bpc = 4

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
{-# LANGUAGE ConstraintKinds, GeneralizedNewtypeDeriving, FlexibleContexts, ScopedTypeVariables, BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns -fno-warn-orphans #-}


module Sound.Tidal.Tempo where

import Control.Concurrent.MVar
import qualified Sound.Tidal.Pattern as P
import qualified Sound.OSC.FD as O
import Control.Concurrent (forkIO, ThreadId, threadDelay)
import Control.Monad (when)
import qualified Data.Map.Strict as Map
import qualified Control.Exception as E
import Sound.Tidal.ID
import Sound.Tidal.Config
import Sound.Tidal.Utils (writeError)
import qualified Sound.Tidal.Link as Link
import Foreign.C.Types (CDouble(..))
import System.IO (hPutStrLn, stderr)
import Data.Int(Int64)

import Sound.Tidal.StreamTypes

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
  SetCycle P.Time
  | SingleTick P.ControlPattern
  | SetNudge Double
  | StreamReplace ID P.ControlPattern
  | Transition Bool TransitionMapper ID P.ControlPattern

data State = State {ticks    :: Int64,
                    start    :: Link.Micros,
                    nowArc   :: P.Arc,
                    nudged   :: Double
                   }
  deriving Show

data ActionHandler =
  ActionHandler {
    onTick :: TickState -> LinkOperations -> P.ValueMap -> IO P.ValueMap,
    onSingleTick :: LinkOperations -> P.ValueMap -> P.ControlPattern -> IO P.ValueMap,
    updatePattern :: ID -> P.Time -> P.ControlPattern -> IO ()
  }

data LinkOperations =
  LinkOperations {
    timeAtBeat :: Link.Beat -> IO Link.Micros,
    timeToCycles :: Link.Micros -> IO P.Time,
    getTempo :: IO Link.BPM,
    setTempo :: Link.BPM -> Link.Micros -> IO (),
    linkToOscTime :: Link.Micros -> O.Time,
    beatToCycles :: CDouble -> CDouble,
    cyclesToBeat :: CDouble -> CDouble
  }

setCycle :: P.Time -> MVar [TempoAction] -> IO ()
setCycle cyc actionsMV = modifyMVar_ actionsMV (\actions -> return $ SetCycle cyc : actions)

setNudge :: MVar [TempoAction] -> Double -> IO ()
setNudge actionsMV nudge = modifyMVar_ actionsMV (\actions -> return $ SetNudge nudge : actions)

timeToCycles' :: Config -> Link.SessionState -> Link.Micros -> IO P.Time
timeToCycles' config ss time = do
  beat <- Link.beatAtTime ss time (cQuantum config)
  return $! (toRational beat) / (toRational (cBeatsPerCycle config))

-- At what time does the cycle occur according to Link?
cyclesToTime :: Config -> Link.SessionState -> P.Time -> IO Link.Micros
cyclesToTime config ss cyc = do
  let beat = (fromRational cyc) * (cBeatsPerCycle config)
  Link.timeAtBeat ss beat (cQuantum config)

addMicrosToOsc :: Link.Micros -> O.Time -> O.Time
addMicrosToOsc m t = ((fromIntegral m) / 1000000) + t

-- clocked assumes tempoMV is empty
clocked :: Config -> MVar P.ValueMap -> MVar PlayMap -> MVar [TempoAction] -> ActionHandler -> Link.AbletonLink -> IO [ThreadId]
clocked config stateMV mapMV actionsMV ac abletonLink
  = do -- TODO - do something with thread id
      clockTid <- forkIO $ loopInit
      return $! [clockTid]
  where frameTimespan :: Link.Micros
        frameTimespan = round $ (cFrameTimespan config) * 1000000
        quantum :: CDouble
        quantum = cQuantum config
        beatsPerCycle :: CDouble
        beatsPerCycle = cBeatsPerCycle config
        loopInit :: IO a
        loopInit =
          do
            when (cEnableLink config) $ Link.enable abletonLink
            sessionState <- Link.createAndCaptureAppSessionState abletonLink
            now <- Link.clock abletonLink
            let startAt = now + processAhead
            Link.requestBeatAtTime sessionState 0 startAt quantum
            Link.commitAndDestroyAppSessionState abletonLink sessionState
            putMVar actionsMV []
            let st = State {ticks = 0,
                       start = now,
                       nowArc = P.Arc 0 0,
                       nudged = 0
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
          now <- Link.clock abletonLink
          let preferredNewTick = ticks st + 1
              logicalNow = logicalTime (start st) preferredNewTick
              aheadOfNow = now + processAhead
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
        -- of nowArc. How far ahead is controlled by cProcessAhead.
        processAhead :: Link.Micros
        processAhead = round $ (cProcessAhead config) * 1000000
        checkArc :: State -> IO a
        checkArc st = do
          actions <- swapMVar actionsMV [] 
          st' <- processActions st actions
          let logicalEnd = logicalTime (start st') $ ticks st' + 1
              nextArcStartCycle = P.stop $ nowArc st'
          ss <- Link.createAndCaptureAppSessionState abletonLink
          arcStartTime <- cyclesToTime config ss nextArcStartCycle
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
            sessionState <- Link.createAndCaptureAppSessionState abletonLink
            endCycle <- timeToCycles' config sessionState logicalEnd
            let st' = st {nowArc = P.Arc startCycle endCycle}
            nowOsc <- O.time
            nowLink <- Link.clock abletonLink
            let ops = LinkOperations {
              timeAtBeat = \beat -> Link.timeAtBeat sessionState beat quantum ,
              timeToCycles = timeToCycles' config sessionState,
              getTempo = Link.getTempo sessionState,
              setTempo = Link.setTempo sessionState,
              linkToOscTime = \lt -> addMicrosToOsc (lt - nowLink) nowOsc,
              beatToCycles = btc,
              cyclesToBeat = ctb
            }
            let state = TickState {
                tickArc   = nowArc st',
                tickNudge = nudged st'
            }
            streamState' <- (onTick ac) state ops streamState
            Link.commitAndDestroyAppSessionState abletonLink sessionState
            putMVar stateMV streamState'
            tick st'
        btc :: CDouble -> CDouble
        btc beat = beat / beatsPerCycle
        ctb :: CDouble -> CDouble
        ctb cyc =  cyc * beatsPerCycle
        processActions :: State -> [TempoAction] -> IO State
        processActions st [] = return $! st
        processActions st actions = do
          streamState <- takeMVar stateMV
          (st', streamState') <- handleActions st actions streamState
          putMVar stateMV streamState'
          return $! st'
        handleActions :: State -> [TempoAction] -> P.ValueMap -> IO (State, P.ValueMap)
        handleActions st [] streamState = return (st, streamState)
        handleActions st (SetCycle cyc : otherActions) streamState =
          do
            (st', streamState') <- handleActions st otherActions streamState
            sessionState <- Link.createAndCaptureAppSessionState abletonLink

            now <- Link.clock abletonLink
            let startAt = now + processAhead
                beat = (fromRational cyc) * (cBeatsPerCycle config)
            Link.requestBeatAtTime sessionState beat startAt quantum
            Link.commitAndDestroyAppSessionState abletonLink sessionState

                  
            let st'' = st' {
                  ticks = 0,
                  start = now,
                  nowArc = P.Arc cyc cyc
                  }

            return (st'', streamState')
        handleActions st (SingleTick pat : otherActions) streamState =
          do
            (st', streamState') <- handleActions st otherActions streamState
            -- onSingleTick assumes it runs at beat 0.
            -- The best way to achieve that is to use forceBeatAtTime.
            -- But using forceBeatAtTime means we can not commit its session state.
            -- Another session state, which we will commit,
            -- is introduced to keep track of tempo changes.
            sessionState <- Link.createAndCaptureAppSessionState abletonLink
            zeroedSessionState <- Link.createAndCaptureAppSessionState abletonLink
            nowOsc <- O.time
            nowLink <- Link.clock abletonLink
            Link.forceBeatAtTime zeroedSessionState 0 (nowLink + processAhead) quantum
            let ops = LinkOperations {
              timeAtBeat = \beat -> Link.timeAtBeat zeroedSessionState beat quantum,
              timeToCycles = timeToCycles' config zeroedSessionState,
              getTempo = Link.getTempo zeroedSessionState,
              setTempo = \bpm micros ->
                            Link.setTempo zeroedSessionState bpm micros >>
                            Link.setTempo sessionState bpm micros,
              linkToOscTime = \lt -> addMicrosToOsc (lt - nowLink) nowOsc,
              beatToCycles = btc,
              cyclesToBeat = ctb
            }
            streamState'' <- (onSingleTick ac) ops streamState' pat
            Link.commitAndDestroyAppSessionState abletonLink sessionState
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
                now <- Link.clock abletonLink
                sessionState <- Link.createAndCaptureAppSessionState abletonLink
                cyc <- timeToCycles' config sessionState now
                Link.destroySessionState sessionState
                (updatePattern ac) k cyc pat
                return (st', streamState')
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
              updatePS Nothing = PlayState {pattern = P.silence,
                                            mute = False,
                                            solo = False,
                                            history = (appendPat historyFlag) (P.silence:[])
                                          }
              transition' pat' = do now <- Link.clock abletonLink
                                    ss <- Link.createAndCaptureAppSessionState abletonLink
                                    c <- timeToCycles' config ss now
                                    return $! f c pat'
            pMap <- readMVar mapMV
            let playState = updatePS $ Map.lookup (fromID patId) pMap
            pat' <- transition' $ appendPat (not historyFlag) (history playState)
            let pMap' = Map.insert (fromID patId) (playState {pattern = pat'}) pMap
            _ <- swapMVar mapMV pMap'
            return (st', streamState')

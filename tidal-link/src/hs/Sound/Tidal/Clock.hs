module Sound.Tidal.Clock where

import qualified Sound.Tidal.Link as Link
import qualified Sound.Osc.Fd as O

import Control.Concurrent     (forkIO, threadDelay)
import Control.Concurrent.STM (TVar, atomically, readTVar, newTVar, modifyTVar', swapTVar, retry)
import Control.Monad          (when)
import Control.Monad.Reader   (ReaderT, runReaderT, ask)
import Control.Monad.State    (StateT, liftIO, evalStateT, get, put, modify)

import Foreign.C.Types        (CDouble (..))
import Data.Int               (Int64)
import Data.Coerce            (coerce)
import System.IO              (hPutStrLn, stderr)

type Time = Rational

-- | representation of a tick based clock
type Clock
  = ReaderT ClockMemory (StateT ClockState IO)

-- | internal read-only memory of the clock
data ClockMemory
  = ClockMemory
  {clockConfig :: ClockConfig
  ,clockRef :: ClockRef
  ,clockAction :: TickAction
  }

-- | internal mutable state of the clock
data ClockState
  = ClockState
  {ticks  :: Int64
  ,start  :: Link.Micros
  ,nowArc :: (Time, Time)
  ,nudged :: Double
  } deriving Show

-- | reference to interact with the clock, while it is running
data ClockRef
  = ClockRef
  {rAction :: TVar ClockAction
  ,rAbletonLink :: Link.AbletonLink
  }

-- | configuration of the clock
data ClockConfig
  = ClockConfig
  {cQuantum :: CDouble
  ,cBeatsPerCycle :: CDouble
  ,cFrameTimespan :: Double
  ,cEnableLink :: Bool
  ,cSkipTicks :: Int64
  ,cProcessAhead :: Double
  }

-- | action to be executed on a tick,
-- | given the current timespan, nudge and reference to the clock
type TickAction
  = (Time,Time) -> Double -> ClockConfig -> ClockRef -> (Link.SessionState, Link.SessionState) -> IO ()

-- | possible actions for interacting with the clock
data ClockAction
  = NoAction
  | SetCycle Time
  | SetTempo Time
  | SetNudge Double

defaultCps :: Double
defaultCps = 0.575

defaultConfig :: ClockConfig
defaultConfig = ClockConfig
              {cFrameTimespan = 1/20
              ,cEnableLink = False
              ,cProcessAhead = 3/10
              ,cSkipTicks = 10
              ,cQuantum = 4
              ,cBeatsPerCycle = 4
              }

-- | creates a clock according to the config and runs it
-- | in a seperate thread
clocked :: ClockConfig -> TickAction -> IO ClockRef
clocked config ac = runClock config ac clockCheck

-- | runs the clock on the initial state and memory as given
-- | by initClock, hands the ClockRef for interaction from outside
runClock :: ClockConfig -> TickAction -> Clock () -> IO ClockRef
runClock config ac clock = do
              (mem, st) <- initClock config ac
              _ <- forkIO $ evalStateT (runReaderT clock mem) st
              return (clockRef mem)

-- | creates a ableton link instance and an MVar for interacting
-- | with the clock from outside and computes the initial clock state
initClock :: ClockConfig -> TickAction -> IO (ClockMemory, ClockState)
initClock config ac = do
      abletonLink <- Link.create bpm
      when (cEnableLink config) $ Link.enable abletonLink
      sessionState <- Link.createAndCaptureAppSessionState abletonLink
      now <- Link.clock abletonLink
      let startAt = now + processAhead
      Link.requestBeatAtTime sessionState 0 startAt (cQuantum config)
      Link.commitAndDestroyAppSessionState abletonLink sessionState
      clockMV <- atomically $ newTVar NoAction
      let st = ClockState {ticks = 0,
                 start = now,
                 nowArc = (0,0),
                 nudged = 0
                }
      return (ClockMemory config (ClockRef clockMV abletonLink) ac, st)
  where processAhead = round $ (cProcessAhead config) * 1000000
        bpm = (coerce defaultCps) * 60 * (cBeatsPerCycle config)


-- The reference time Link uses,
-- is the time the audio for a certain beat hits the speaker.
-- Processing of the nowArc should happen early enough for
-- all events in the nowArc to hit the speaker, but not too early.
-- Processing thus needs to happen a short while before the start
-- of nowArc. How far ahead is controlled by cProcessAhead.

-- previously called checkArc
clockCheck :: Clock ()
clockCheck = do
    (ClockMemory config (ClockRef clockMV abletonLink) _) <- ask

    action <- liftIO $ atomically $ swapTVar clockMV NoAction
    processAction action

    st <- get

    let logicalEnd = logicalTime config (start st) $ ticks st + 1
        nextArcStartCycle = arcEnd $ nowArc st

    ss <- liftIO $ Link.createAndCaptureAppSessionState abletonLink
    arcStartTime <- liftIO $ cyclesToTime config ss nextArcStartCycle
    liftIO $ Link.destroySessionState ss

    if (arcStartTime < logicalEnd)
      then clockProcess
      else tick

-- tick moves the logical time forward or recalculates the ticks in case
-- the logical time is out of sync with Link time.
-- tick delays the thread when logical time is ahead of Link time.
tick :: Clock ()
tick = do
  (ClockMemory config (ClockRef _ abletonLink) _) <- ask
  st <- get
  now <- liftIO $ Link.clock abletonLink
  let processAhead = round $ (cProcessAhead config) * 1000000
      frameTimespan = round $ (cFrameTimespan config) * 1000000
      preferredNewTick = ticks st + 1
      logicalNow = logicalTime config (start st) preferredNewTick
      aheadOfNow = now + processAhead
      actualTick = (aheadOfNow - start st) `div` frameTimespan
      drifted    = abs (actualTick - preferredNewTick) > (cSkipTicks config)
      newTick | drifted   = actualTick
              | otherwise = preferredNewTick
      delta = min frameTimespan (logicalNow - aheadOfNow)

  put $ st {ticks = newTick}

  if drifted
    then liftIO $ hPutStrLn stderr $ "skip: " ++ (show (actualTick - ticks st))
    else when (delta > 0) $ liftIO $ threadDelay $ fromIntegral delta

  clockCheck

-- previously called processArc
-- hands the current link operations to the TickAction
clockProcess :: Clock ()
clockProcess = do
    (ClockMemory config ref@(ClockRef _ abletonLink) action) <- ask
    st <- get
    let logicalEnd = logicalTime config (start st) $ ticks st + 1
        startCycle = arcEnd $ nowArc st

    sessionState <- liftIO $ Link.createAndCaptureAppSessionState abletonLink
    endCycle <- liftIO $ timeToCycles config sessionState logicalEnd

    liftIO $ action (startCycle,endCycle) (nudged st) config ref (sessionState, sessionState)

    liftIO $ Link.commitAndDestroyAppSessionState abletonLink sessionState

    put (st {nowArc = (startCycle,endCycle)})
    tick

processAction :: ClockAction -> Clock ()
processAction NoAction = return ()
processAction (SetNudge n) = modify (\st  -> st {nudged = n})
processAction (SetTempo bpm) = do
            (ClockMemory _ (ClockRef _ abletonLink) _) <- ask
            sessionState <- liftIO $ Link.createAndCaptureAppSessionState abletonLink
            now <- liftIO $ Link.clock abletonLink
            liftIO $ Link.setTempo sessionState (fromRational bpm) now
            liftIO $ Link.commitAndDestroyAppSessionState abletonLink sessionState
processAction (SetCycle cyc) = do
            (ClockMemory config (ClockRef _ abletonLink) _) <- ask
            sessionState <- liftIO $ Link.createAndCaptureAppSessionState abletonLink

            now <- liftIO $ Link.clock abletonLink
            let processAhead = round $ (cProcessAhead config) * 1000000
                startAt = now + processAhead
                beat = (fromRational cyc) * (cBeatsPerCycle config)
            liftIO $ Link.requestBeatAtTime sessionState beat startAt (cQuantum config)
            liftIO $ Link.commitAndDestroyAppSessionState abletonLink sessionState

            modify (\st -> st {ticks = 0, start = now, nowArc = (cyc,cyc)})

---------------------------------------------------------------
----------- functions representing link operations ------------
---------------------------------------------------------------

arcStart :: (Time, Time) -> Time
arcStart = fst

arcEnd :: (Time, Time) -> Time
arcEnd = snd

beatToCycles :: ClockConfig -> Double -> Double
beatToCycles config beat = beat / (coerce $ cBeatsPerCycle config)

cyclesToBeat :: ClockConfig -> Double -> Double
cyclesToBeat config cyc = cyc * (coerce $ cBeatsPerCycle config)

getSessionState :: ClockRef -> IO Link.SessionState
getSessionState (ClockRef _ abletonLink) = Link.createAndCaptureAppSessionState abletonLink

-- onSingleTick assumes it runs at beat 0.
-- The best way to achieve that is to use forceBeatAtTime.
-- But using forceBeatAtTime means we can not commit its session state.
getZeroedSessionState :: ClockConfig -> ClockRef -> IO Link.SessionState
getZeroedSessionState config (ClockRef _ abletonLink) = do
                            ss <- Link.createAndCaptureAppSessionState abletonLink
                            nowLink <- liftIO $ Link.clock abletonLink
                            Link.forceBeatAtTime ss 0 (nowLink + processAhead) (cQuantum config)
                            return ss
                        where processAhead = round $ (cProcessAhead config) * 1000000

getTempo :: Link.SessionState -> IO Time
getTempo ss = fmap toRational $ Link.getTempo ss

setTempoCPS :: Time -> Link.Micros -> ClockConfig -> Link.SessionState -> IO ()
setTempoCPS cps now conf ss = Link.setTempo ss (coerce $ cyclesToBeat conf ((fromRational cps) * 60)) now

timeAtBeat :: ClockConfig -> Link.SessionState  -> Double -> IO Link.Micros
timeAtBeat config ss beat = Link.timeAtBeat ss (coerce beat) (cQuantum config)

timeToCycles :: ClockConfig -> Link.SessionState -> Link.Micros -> IO Time
timeToCycles config ss time = do
  beat <- Link.beatAtTime ss time (cQuantum config)
  return $! (toRational beat) / (toRational (cBeatsPerCycle config))

-- At what time does the cycle occur according to Link?
cyclesToTime :: ClockConfig -> Link.SessionState -> Time -> IO Link.Micros
cyclesToTime config ss cyc = do
  let beat = (fromRational cyc) * (cBeatsPerCycle config)
  Link.timeAtBeat ss beat (cQuantum config)

linkToOscTime :: ClockRef -> Link.Micros -> IO O.Time
linkToOscTime (ClockRef _ abletonLink) lt = do
        nowOsc <- O.time
        nowLink <- liftIO $ Link.clock abletonLink
        return $ addMicrosToOsc (lt - nowLink) nowOsc

addMicrosToOsc :: Link.Micros -> O.Time -> O.Time
addMicrosToOsc m t = ((fromIntegral m) / 1000000) + t

-- Time is processed at a fixed rate according to configuration
-- logicalTime gives the time when a tick starts based on when
-- processing first started.
logicalTime :: ClockConfig -> Link.Micros -> Int64 -> Link.Micros
logicalTime config startTime ticks' = startTime + ticks' * frameTimespan
                              where frameTimespan = round $ (cFrameTimespan config) * 1000000

---------------------------------------------------------------
----------- functions for interacting with the clock ----------
---------------------------------------------------------------

getBPM :: ClockRef -> IO Time
getBPM (ClockRef _ abletonLink) = do
                            ss <- Link.createAndCaptureAppSessionState abletonLink
                            bpm <- Link.getTempo ss
                            Link.destroySessionState ss
                            return $! toRational bpm

getCPS :: ClockConfig -> ClockRef -> IO Time
getCPS config ref = fmap (\bpm -> bpm / (toRational $ cBeatsPerCycle config) / 60) (getBPM ref)

getCycleTime :: ClockConfig -> ClockRef -> IO Time
getCycleTime config (ClockRef _ abletonLink) = do
                            now <- Link.clock abletonLink
                            ss <- Link.createAndCaptureAppSessionState abletonLink
                            c <- timeToCycles config ss now
                            Link.destroySessionState ss
                            return $! c

resetClock :: ClockRef -> IO ()
resetClock clock = setClock clock 0

setClock :: ClockRef -> Time -> IO ()
setClock (ClockRef clock _) t = atomically $ do
                                      action <- readTVar clock
                                      case action of
                                        NoAction -> modifyTVar' clock (const $ SetCycle t)
                                        _ -> retry

setBPM :: ClockRef -> Time -> IO ()
setBPM (ClockRef clock _) t = atomically $ do
                                      action <- readTVar clock
                                      case action of
                                        NoAction -> modifyTVar' clock (const $ SetTempo t)
                                        _ -> retry

setCPS :: ClockConfig -> ClockRef -> Time -> IO ()
setCPS config ref cps = setBPM ref bpm
                       where bpm = cps * 60 * (toRational $ cBeatsPerCycle config)

setNudge :: ClockRef -> Double -> IO ()
setNudge (ClockRef clock _) n = atomically $ do
                                      action <- readTVar clock
                                      case action of
                                        NoAction -> modifyTVar' clock (const $ SetNudge n)
                                        _ -> retry

-- Used for Tempo callback
-- Tempo changes will be applied.
-- However, since the full arc is processed at once and since Link does not support
-- scheduling, tempo change may affect scheduling of events that happen earlier
-- in the normal stream (the one handled by onTick).
clockOnce :: TickAction -> ClockConfig -> ClockRef -> IO ()
clockOnce action config ref@(ClockRef _ abletonLink) = do
        ss <- getZeroedSessionState config ref
        temposs <- Link.createAndCaptureAppSessionState abletonLink
        -- The nowArc is a full cycle
        action (0,1) 0 config ref (ss, temposs)
        Link.destroySessionState ss
        Link.commitAndDestroyAppSessionState abletonLink temposs

disableLink :: ClockRef -> IO ()
disableLink (ClockRef _ abletonLink) = Link.disable abletonLink

enableLink :: ClockRef -> IO ()
enableLink (ClockRef _ abletonLink) = Link.enable abletonLink

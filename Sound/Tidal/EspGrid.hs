module Sound.Tidal.EspGrid where

import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad (forever)
import Control.Monad.Loops (iterateM_)
import Sound.OSC.FD
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Calendar (fromGregorian)

import Sound.Tidal.Tempo
import Sound.Tidal.Time as T
import Sound.Tidal.Stream
import Sound.Tidal.Dirt
import Sound.Tidal.Transition (transition)
import Sound.Tidal.Pattern (silence)

parseEspTempo :: [Datum] -> Maybe Tempo
parseEspTempo d = do
  on <- datum_integral (d!!0)
  bpm <- datum_floating (d!!1)
  t1 <- datum_integral (d!!2)
  t2 <- datum_integral (d!!3)
  n <- datum_integral (d!!4)
  let nanos = (t1*1000000000) + t2
  let utc = posixSecondsToUTCTime ((realToFrac nanos)/1000000000)
  return (Tempo utc (fromIntegral n) (bpm/60) (on==0) 0.04)

changeTempo :: MVar Tempo -> Packet -> IO ()
changeTempo mvar (Packet_Message msg) = do
    case parseEspTempo (messageDatum msg) of
      Just t -> tryTakeMVar mvar >> putMVar mvar t
      Nothing -> putStrLn "Unable to parse message as Tempo"
changeTempo _ _ = putStrLn "Can only process Packet_Message"

getTempo :: MVar Tempo -> IO Tempo
getTempo = readMVar

runClientEsp :: IO (MVar Tempo,MVar Double)
runClientEsp = do
  mTempo <- newEmptyMVar
  mCps <- newEmptyMVar
  socket <- openUDP "127.0.0.1" 5510
  forkIO $ forever $ do
    sendOSC socket $ Message "/esp/tempo/q" []
    response <- waitAddress socket "/esp/tempo/r"
    changeTempo mTempo response
    threadDelay 100000
  return (mTempo, mCps)

sendEspTempo :: Real t => t -> IO ()
sendEspTempo t = do
  socket <- openUDP "127.0.0.1" 5510
  sendOSC socket $ Message "/esp/beat/tempo" [float (t*60)]

cpsUtilsEsp :: IO (Double -> IO (), IO Rational, IO Tempo)
cpsUtilsEsp = do
  (mTempo,mCps) <- runClientEsp
  return (sendEspTempo,getCurrentBeat mTempo,getTempo mTempo)

clockedTickEsp :: Int -> (Tempo -> Int -> IO ()) -> IO ()
clockedTickEsp tpb callback = do
  (mTempo, _) <- runClientEsp
  nowBeat <- getCurrentBeat mTempo
  let nextTick = ceiling (nowBeat * (fromIntegral tpb))
  iterateM_ (clockedTickLoopEsp tpb callback mTempo) nextTick

clockedTickLoopEsp :: Int -> (Tempo -> Int -> IO ()) -> MVar Tempo -> Int -> IO Int
clockedTickLoopEsp tpb callback mTempo tick = do
  tempo <- readMVar mTempo
  if (paused tempo)
    then do  -- TODO - do this via blocking read on the mvar somehow rather than polling
      let pause = 0.01
      threadDelay $ floor (pause * 1000000)
      return $ if cps tempo < 0 then 0 else tick  -- reset tick to 0 if cps is negative
    else do
      now <- getCurrentTime
      let beatsFromAtToTick = fromIntegral tick / fromIntegral tpb - beat tempo
          delayUntilTick = beatsFromAtToTick / cps tempo - realToFrac (diffUTCTime now (at tempo))
      threadDelay $ floor (delayUntilTick * 1000000)
      callback tempo tick
      return $ tick + 1

streamEsp :: Backend a -> Shape -> IO (ParamPattern -> IO ())
streamEsp backend shape = do
  patternM <- newMVar silence
  forkIO $ clockedTickEsp ticksPerCycle (onTick backend shape patternM)
  return $ \p -> do swapMVar patternM p
                    return ()

dirtStreamEsp :: IO (ParamPattern -> IO ())
dirtStreamEsp = do
  backend <- dirtBackend
  streamEsp backend dirt

stateEsp :: Backend a -> Shape -> IO (MVar (ParamPattern, [ParamPattern]))
stateEsp backend shape = do
  patternsM <- newMVar (silence, [])
  let ot = (onTick' backend shape patternsM) :: Tempo -> Int -> IO ()
  forkIO $ clockedTickEsp ticksPerCycle ot
  return patternsM

dirtSettersEsp :: IO T.Time -> IO (ParamPattern -> IO (), (T.Time -> [ParamPattern] -> ParamPattern) -> ParamPattern -> IO ())
dirtSettersEsp getNow = do
  backend <- dirtBackend
  ds <- stateEsp backend dirt
  return (setter ds, transition getNow ds)

superDirtSettersEsp :: IO T.Time -> IO (ParamPattern -> IO (), (T.Time -> [ParamPattern] -> ParamPattern) -> ParamPattern -> IO ())
superDirtSettersEsp getNow = do
  backend <- superDirtBackend 57120
  ds <- stateEsp backend dirt
  return (setter ds, transition getNow ds)

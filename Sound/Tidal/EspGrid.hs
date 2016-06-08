module Sound.Tidal.EspGrid where

import Sound.Tidal.Tempo
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad (forever)
import Sound.OSC.FD
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Calendar (fromGregorian)

-- test = do
--  (cps,getNow) <- cpsUtilsEsp
--  (c1,ct1) <- dirtSetters getNow
--  c1 $ s (p "bd cp")
--  getLine
--  c1 silence

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
    let t = parseEspTempo (messageDatum msg)
    case t of
      Just t' -> do
        tryTakeMVar mvar
        putMVar mvar t'
--        putStrLn ("changeTempo: " ++ (show t'))
        return ()
      Nothing -> error "Unable to parse message as Tempo"
changeTempo _ _ = error "Can only process Packet_Message"

tempoQuery :: MVar Tempo -> IO ()
tempoQuery mvar = do
  socket <- openUDP "127.0.0.1" 5510
  sendOSC socket $ Message "/esp/tempo/q" []
  response <- waitAddress socket "/esp/tempo/r"
  changeTempo mvar response

clientEsp :: MVar Tempo -> MVar Double -> IO ()
clientEsp t cps = forever $ do
  tempoQuery t
  threadDelay 500000

-- this should be moved to Tempo.hs and incorporated in other places
getCurrentBeat :: MVar Tempo -> IO Rational
getCurrentBeat t = (readMVar t) >>= (beatNow) >>= (return . toRational)

runClientEsp :: IO (MVar Tempo,MVar Double)
runClientEsp = do
  mTempo <- newEmptyMVar
  mCps <- newEmptyMVar
  forkIO $ clientEsp mTempo mCps
  return (mTempo, mCps)

cpsUtilsEsp :: IO (Double -> IO (), IO Rational)
cpsUtilsEsp = do
  (mTempo,mCps) <- runClientEsp
  return (putMVar mCps,getCurrentBeat mTempo)

clockedTickEsp :: Int -> (Tempo -> Int -> IO ()) -> IO ()
clockedTickEsp tpb callback =
    do (mTempo, mCps) <- runClientEsp
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
--               putStrLn ("clockedTickEsp: " ++ (show tempo))
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
                   tickDelta = (fromIntegral tick) - actualTick
                   delay = tickDelta / tps
               --putStrLn ("Delay: " ++ show delay ++ "s Beat: " ++ show (beat tempo))
               threadDelay $ floor (delay * 1000000)
               callback tempo tick
               return $ tick + 1

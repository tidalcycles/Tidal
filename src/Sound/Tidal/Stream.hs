module Sound.Tidal.Stream where

import Sound.Tidal.Pattern
import Sound.Tidal.Tempo as T
import qualified Sound.OSC.FD as O
import Sound.OSC.Datum as O
import Control.Concurrent.MVar
import Control.Concurrent
import qualified Data.Map.Strict as Map

data TimeStamp = BundleStamp | MessageStamp | NoStamp
 deriving Eq

data OSCTarget = OSCTarget {address :: String,
                            port :: Int,
                            path :: String,
                            shape :: Maybe [(String, Maybe Value)],
                            latency :: Double,
                            preamble :: [O.Datum],
                            timestamp :: TimeStamp
                           }

superdirtTarget = OSCTarget {address = "127.0.0.1",
                             port = 57120,
                             path = "/play2",
                             shape = Nothing,
                             latency = 0.2,
                             preamble = [],
                             timestamp = BundleStamp
                            }

stream :: OSCTarget -> IO (ControlPattern -> IO (ControlPattern))
stream target = do u <- O.openUDP (address target) (port target)
                   mp <- newMVar silence
                   forkIO $ clocked $ onTick mp target u
                   return $ swapMVar mp

toDatum :: Value -> O.Datum
toDatum (VF x) = float x
toDatum (VI x) = int32 x
toDatum (VS x) = string x

toData :: Event ControlMap -> [O.Datum]
toData e = concatMap (\(n,v) -> [string n, toDatum v]) $ Map.toList $ eventValue e

onTick :: MVar ControlPattern -> OSCTarget -> O.UDP -> Tempo -> T.State -> IO ()
onTick mp target u t st =
  do p <- readMVar mp
     let es = filter eventHasOnset $ queryArc p (nowArc st)
         messages = map (\e -> (sched $ fst $ eventWhole e, toMessage e)) es
         toMessage e = O.Message (path target) $ preamble target ++ toData e
     mapM_ send messages
     return ()
  where send (time, m) = O.sendOSC u $ O.Bundle (time + (latency target)) [m]
        sched :: Rational -> Double
        sched c = ((fromRational $ c - (atCycle t)) / cps t) + (nowTime st)

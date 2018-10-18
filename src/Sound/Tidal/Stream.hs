module Sound.Tidal.Stream where

import Sound.Tidal.Pattern
import Sound.Tidal.UI (stack, silence)
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

superdirtTarget :: OSCTarget
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
                   mp <- newMVar empty
                   _ <- ($) forkIO $ clocked $ onTick mp target u
                   return $ \p -> swapMVar mp p >> return p

type PatId = Int

data PlayState = PlayState {pattern :: ControlPattern,
                            mute :: Bool,
                            solo :: Bool
                           }

type PlayMap = Map.Map PatId PlayState

stream2 :: OSCTarget -> IO (PatId -> ControlPattern -> IO (), -- swap
                            IO ()) -- hush
                             -- IO (Int -> IO ()), -- toggle mute
                             -- IO (Int -> IO ()), -- solo
                             -- IO (IO ()), -- unsolo
                             -- IO ([Int, True]) -- list patterns and whether they're muted
                             -- ]
stream2 target = do set <- stream target
                    pMapMV <- newMVar (Map.empty :: Map.Map PatId PlayState)
                    return (swap set pMapMV,
                            hush set pMapMV
                            {- --toggle set pMapMV,
                                solo set pMapMV,
                                unsolo set pMapMV,
                                list set pMapMV -}
                           )
  where
        swap :: (ControlPattern -> IO ControlPattern) -> MVar PlayMap -> PatId -> ControlPattern -> IO ()
        swap set pMapMV k p = do pMap <- takeMVar pMapMV
                                 let pMap' = Map.insert k (PlayState p False False) pMap
                                 update set pMap'
                                 putMVar pMapMV pMap'
                                 return ()
        update :: (ControlPattern -> IO ControlPattern) -> PlayMap -> IO ()
        update set pMap = do set $ stack $ map pattern $ filter (\pState -> if hasSolo pMap then solo pState else not (mute pState)) (Map.elems pMap)
                             return ()
        hasSolo = (>= 1) . length . filter solo . Map.elems
        hush set pMapMV = do set silence
                             swapMVar pMapMV Map.empty
                             return ()

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

module Sound.Tidal.Stream where

import Sound.Tidal.Pattern
import Sound.Tidal.UI (stack, silence)
import qualified Sound.Tidal.Tempo as T
import qualified Sound.OSC.FD as O
import Sound.OSC.Datum as O
import Control.Concurrent.MVar
import Control.Concurrent
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)

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

stream :: MVar ControlMap -> OSCTarget -> IO (ControlPattern -> IO (ControlPattern))
stream cMapMV target = do u <- O.openUDP (address target) (port target)
                          mp <- newMVar empty
                          _ <- ($) forkIO $ T.clocked $ onTick cMapMV mp target u
                          return $ \p -> swapMVar mp p >> return p

type PatId = String

data PlayState = PlayState {pattern :: ControlPattern,
                            mute :: Bool,
                            solo :: Bool
                           }

type PlayMap = Map.Map PatId PlayState

listenCMap cMapMV = do sock <- O.udpServer "127.0.0.1" (6011)
                       _ <- forkIO $ loop sock
                       return ()
  where loop sock =
          do ms <- O.recvMessages sock
             mapM_ r ms
             loop sock
        r (O.Message path (O.ASCII_String k:v@(O.Float _):[])) = add cMapMV (ascii_to_string k) (VF $ fromJust $ datum_floating v)
        r (O.Message path (O.ASCII_String k:O.ASCII_String v:[])) = add cMapMV (ascii_to_string k) (VS $ ascii_to_string v)
        r (O.Message path (O.ASCII_String k:O.Int32 v:[]))  = add cMapMV (ascii_to_string k) (VI $ fromIntegral v)
        add :: MVar ControlMap -> String -> Value -> IO ()
        add cMapMV k v = do cMap <- takeMVar cMapMV
                            putMVar cMapMV $ Map.insert k v cMap
                            return ()

stream3 :: OSCTarget -> IO (MVar ControlMap,
                            PatId -> ControlPattern -> IO (), -- swap
                            IO (), -- hush
                            IO () -- list
                           )
                             -- IO (Int -> IO ()), -- toggle mute
                             -- IO (Int -> IO ()), -- solo
                             -- IO (IO ()), -- unsolo
                             -- IO ([Int, True]) -- list patterns and whether they're muted
                             -- ]
stream3 target = do pMapMV <- newMVar (Map.empty :: Map.Map PatId PlayState)
                    cMapMV <- newMVar (Map.empty :: ControlMap)
                    listenCMap cMapMV
                    set <- stream cMapMV target
                    return (cMapMV,
                            swap set pMapMV,
                            hush set pMapMV,
                            list pMapMV
                            {- --toggle set pMapMV,
                                solo set pMapMV,
                                unsolo set pMapMV,
                                list set pMapMV -}
                           )
  where
        swap :: (ControlPattern -> IO ControlPattern) -> MVar PlayMap -> PatId -> ControlPattern -> IO ()
        swap set pMapMV k p
          = do pMap <- takeMVar pMapMV
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
        list :: MVar PlayMap -> IO ()
        list pMapMV = do pMap <- readMVar pMapMV
                         let hs = hasSolo pMap
                         putStrLn $ concatMap (showKV hs) $ Map.toList pMap
          where showKV :: Bool -> (PatId, PlayState) -> String
                showKV True  (k, (PlayState _  _ True)) = k ++ " - solo\n"
                showKV True  (k, _) = "(" ++ k ++ ")\n"
                showKV False (k, (PlayState _ False _)) = k ++ "\n"
                showKV False (k, _) = "(" ++ k ++ ") - muted\n"

toDatum :: Value -> O.Datum
toDatum (VF x) = float x
toDatum (VI x) = int32 x
toDatum (VS x) = string x

toData :: Event ControlMap -> [O.Datum]
toData e = concatMap (\(n,v) -> [string n, toDatum v]) $ Map.toList $ eventValue e

onTick :: MVar ControlMap -> MVar ControlPattern -> OSCTarget -> O.UDP -> T.Tempo -> T.State -> IO ()
onTick cMapMV pMV target u t st =
  do p <- readMVar pMV
     cMap <- readMVar cMapMV
     let es = filter eventHasOnset $ query p (State {arc = T.nowArc st, controls = cMap})
         messages = map (\e -> (sched $ fst $ eventWhole e, toMessage e)) es
         toMessage e = O.Message (path target) $ preamble target ++ toData e
     mapM_ send messages
     return ()
  where send (time, m) = O.sendOSC u $ O.Bundle (time + (latency target)) [m]
        sched :: Rational -> Double
        sched c = ((fromRational $ c - (T.atCycle t)) / T.cps t) + (T.nowTime st)

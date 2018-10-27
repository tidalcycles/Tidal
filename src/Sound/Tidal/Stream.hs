module Sound.Tidal.Stream where

import Sound.Tidal.Pattern
import Sound.Tidal.Core (stack, silence)

import qualified Sound.Tidal.Tempo as T
import qualified Sound.OSC.FD as O
import Sound.OSC.Datum as O
import Control.Concurrent.MVar
import Control.Concurrent
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)

data TimeStamp = BundleStamp | MessageStamp | NoStamp
 deriving Eq

data OSCTarget = OSCTarget {oAddress :: String,
                            oPort :: Int,
                            oPath :: String,
                            oShape :: Maybe [(String, Maybe Value)],
                            oLatency :: Double,
                            oPreamble :: [O.Datum],
                            oTimestamp :: TimeStamp
                           }

superdirtTarget :: OSCTarget
superdirtTarget = OSCTarget {oAddress = "127.0.0.1",
                             oPort = 57120,
                             oPath = "/play2",
                             oShape = Nothing,
                             oLatency = 0.02,
                             oPreamble = [],
                             oTimestamp = BundleStamp
                            }

stream :: MVar ControlMap -> OSCTarget -> IO (ControlPattern -> IO (ControlPattern), MVar T.Tempo)
stream cMapMV target = do u <- O.openUDP (oAddress target) (oPort target)
                          mp <- newMVar empty
                          (tempoMV, _) <- T.clocked $ onTick cMapMV mp target u
                          return $ (\p -> swapMVar mp p >> return p, tempoMV)

type PatId = String

data PlayState = PlayState {pattern :: ControlPattern,
                            mute :: Bool,
                            solo :: Bool
                           }

type PlayMap = Map.Map PatId PlayState

listenCMap :: MVar ControlMap -> IO ()
listenCMap cMapMV = do sock <- O.udpServer "127.0.0.1" (6011)
                       _ <- forkIO $ loop sock
                       return ()
  where loop sock =
          do ms <- O.recvMessages sock
             mapM_ readMessage ms
             loop sock
        readMessage (O.Message _ (O.ASCII_String k:v@(O.Float _):[])) = add (ascii_to_string k) (VF $ fromJust $ datum_floating v)
        readMessage (O.Message _ (O.ASCII_String k:O.ASCII_String v:[])) = add (ascii_to_string k) (VS $ ascii_to_string v)
        readMessage (O.Message _ (O.ASCII_String k:O.Int32 v:[]))  = add (ascii_to_string k) (VI $ fromIntegral v)
        readMessage _ = return ()
        add :: String -> Value -> IO ()
        add k v = do cMap <- takeMVar cMapMV
                     putMVar cMapMV $ Map.insert k v cMap
                     return ()

stream5 :: OSCTarget -> IO (MVar T.Tempo,
                            MVar ControlMap,
                            PatId -> ControlPattern -> IO (), -- swap
                            IO (), -- hush
                            IO () -- list
                           )
                             -- IO (Int -> IO ()), -- toggle mute
                             -- IO (Int -> IO ()), -- solo
                             -- IO (IO ()), -- unsolo
                             -- IO ([Int, True]) -- list patterns and whether they're muted
                             -- ]
stream5 target = do pMapMV <- newMVar (Map.empty :: Map.Map PatId PlayState)
                    cMapMV <- newMVar (Map.empty :: ControlMap)
                    listenCMap cMapMV
                    (set, tempoMV) <- stream cMapMV target
                    return (tempoMV,
                            cMapMV,
                            swap set pMapMV,
                            hush set pMapMV,
                            list pMapMV
                            -- once set pMapMV
                            {- --toggle set pMapMV,
                                solo set pMapMV,
                                unsolo set pMapMV,
                                list set pMapMV -}
                           )

toDatum :: Value -> O.Datum
toDatum (VF x) = float x
toDatum (VI x) = int32 x
toDatum (VS x) = string x

toData :: Event ControlMap -> [O.Datum]
toData e = concatMap (\(n,v) -> [string n, toDatum v]) $ Map.toList $ eventValue e

onTick :: MVar ControlMap -> MVar ControlPattern -> OSCTarget -> O.UDP -> MVar T.Tempo -> T.State -> IO ()
onTick cMapMV pMV target u tempoMV st =
  do p <- readMVar pMV
     cMap <- readMVar cMapMV
     tempo <- readMVar tempoMV
     now <- O.time
     let es = filter eventHasOnset $ query p (State {arc = T.nowArc st, controls = cMap})
         at e = sched tempo $ fst $ eventWhole e
         messages = map (\e -> (at e, toMessage e)) es
         cpsChanges = map (\e -> (at e - now, Map.lookup "cps" $ eventValue e)) es
         toMessage e = O.Message (oPath target) $ oPreamble target ++ toData e
     mapM_ send messages
     mapM_ doCps cpsChanges
     return ()
  where send (time, m) = O.sendOSC u $ O.Bundle (time + (oLatency target)) [m]
        sched :: T.Tempo -> Rational -> Double
        sched tempo c = ((fromRational $ c - (T.atCycle tempo)) / T.cps tempo) + (T.atTime tempo)
        doCps (d, Just (VF cps)) = do _ <- forkIO $ do threadDelay $ floor $ d * 1000000
                                                       _ <- T.setCps tempoMV cps
                                                       return ()
                                      return ()
        doCps _ = return ()


-- Interaction

hasSolo :: Map.Map k PlayState -> Bool
hasSolo = (>= 1) . length . filter solo . Map.elems

hush :: (Pattern a1 -> IO a2) -> MVar (Map.Map k a3) -> IO ()
hush set pMapMV = do _ <- set silence
                     _ <- swapMVar pMapMV Map.empty
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

swap :: (ControlPattern -> IO ControlPattern) -> MVar PlayMap -> PatId -> ControlPattern -> IO ()
swap set pMapMV k p
  = do pMap <- takeMVar pMapMV
       let pMap' = Map.insert k (PlayState p False False) pMap
       update set pMap'
       putMVar pMapMV pMap'
       return ()

update :: (ControlPattern -> IO ControlPattern) -> PlayMap -> IO ()
update set pMap = do _ <- set $ stack $ map pattern $ filter (\pState -> if hasSolo pMap then solo pState else not (mute pState)) (Map.elems pMap)
                     return ()


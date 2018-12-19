{-# LANGUAGE ConstraintKinds, GeneralizedNewtypeDeriving, FlexibleContexts, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Sound.Tidal.Stream where

import           Control.Concurrent.MVar
import           Control.Concurrent
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust, fromMaybe)
import qualified Control.Exception as E
-- import Control.Monad.Reader
-- import Control.Monad.Except
-- import qualified Data.Bifunctor as BF
-- import qualified Data.Bool as B
-- import qualified Data.Char as C

import qualified Sound.OSC.FD as O

import           Sound.Tidal.Config
import           Sound.Tidal.Core (stack, silence)
import           Sound.Tidal.Pattern
import qualified Sound.Tidal.Tempo as T
-- import qualified Sound.OSC.Datum as O

data TimeStamp = BundleStamp | MessageStamp | NoStamp
 deriving Eq

data Stream = Stream {sConfig :: Config,
                      sInput :: MVar ControlMap,
                      sOutput :: MVar ControlPattern,
                      sListenTid :: Maybe ThreadId,
                      sPMapMV :: MVar PlayMap,
                      sTempoMV :: MVar T.Tempo,
                      sTarget :: OSCTarget,
                      sUDP :: O.UDP
                     }

type PatId = String

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

startStream :: Config -> MVar ControlMap -> OSCTarget -> IO (MVar ControlPattern, MVar T.Tempo, O.UDP)
startStream config cMapMV target
  = do u <- O.openUDP (oAddress target) (oPort target)
       pMV <- newMVar empty
       (tempoMV, _) <- T.clocked config $ onTick config cMapMV pMV target u
       return $ (pMV, tempoMV, u)


data PlayState = PlayState {pattern :: ControlPattern,
                            mute :: Bool,
                            solo :: Bool,
                            history :: [ControlPattern]
                           }
               deriving Show

type PlayMap = Map.Map PatId PlayState


toDatum :: Value -> O.Datum
toDatum (VF x) = O.float x
toDatum (VI x) = O.int32 x
toDatum (VS x) = O.string x

toData :: Event ControlMap -> [O.Datum]
toData e = concatMap (\(n,v) -> [O.string n, toDatum v]) $ Map.toList $ value e

toMessage :: OSCTarget -> T.Tempo -> Event (Map.Map String Value) -> O.Message
toMessage target tempo e = O.Message (oPath target) $ oPreamble target ++ toData addCps
  where on = sched tempo $ start $ whole e
        off = sched tempo $ stop $ whole e
        delta = off - on
        -- If there is already cps in the event, the union will preserve that.
        addCps = (\v -> (Map.union v $ Map.fromList [("cps", (VF $ T.cps tempo)),
                                                     ("delta", VF delta),
                                                     ("cycle", VF (fromRational $ start $ whole e))
                                                    ])) <$> e

doCps :: MVar T.Tempo -> (Double, Maybe Value) -> IO ()
doCps tempoMV (d, Just (VF cps)) = do _ <- forkIO $ do threadDelay $ floor $ d * 1000000
                                                       -- hack to stop things from stopping !
                                                       _ <- T.setCps tempoMV (max 0.00001 cps)
                                                       return ()
                                      return ()
doCps _ _ = return ()

onTick :: Config -> MVar ControlMap -> MVar ControlPattern -> OSCTarget -> O.UDP -> MVar T.Tempo -> T.State -> IO ()
onTick config cMapMV pMV target u tempoMV st =
  do p <- readMVar pMV
     cMap <- readMVar cMapMV
     tempo <- readMVar tempoMV
     now <- O.time
     let es = filter eventHasOnset $ query p (State {arc = T.nowArc st, controls = cMap})
         on e = (sched tempo $ start $ whole e) + eventNudge e
         eventNudge e = fromJust $ getF $ fromMaybe (VF 0) $ Map.lookup "nudge" $ value e
         messages = map (\e -> (on e, toMessage target tempo e)) es
         cpsChanges = map (\e -> (on e - now, Map.lookup "cps" $ value e)) es
         latency = oLatency target + cFrameTimespan config + T.nudged tempo
     E.catch (mapM_ (send latency u) messages)
       (\(_ ::E.SomeException)
        -> putStrLn $ "Failed to send. Is the target (probably superdirt) running?")
                -- ++ show (msg :: E.SomeException))
     mapM_ (doCps tempoMV) cpsChanges
     return ()

send :: O.Transport t => Double -> t -> (Double, O.Message) -> IO ()
send latency u (time, m) = O.sendBundle u $ O.Bundle (time + latency) [m]

sched :: T.Tempo -> Rational -> Double
sched tempo c = ((fromRational $ c - (T.atCycle tempo)) / T.cps tempo) + (T.atTime tempo)

-- Interaction

streamNudgeAll :: Stream -> Double -> IO ()
streamNudgeAll s nudge = do tempo <- takeMVar $ sTempoMV s
                            putMVar (sTempoMV s) $ tempo {T.nudged = nudge}

streamResetCycles :: Stream -> IO ()
streamResetCycles s = do _ <- T.resetCycles (sTempoMV s)
                         return ()

hasSolo :: Map.Map k PlayState -> Bool
hasSolo = (>= 1) . length . filter solo . Map.elems

streamList :: Stream -> IO ()
streamList s = do pMap <- readMVar (sPMapMV s)
                  let hs = hasSolo pMap
                  putStrLn $ concatMap (showKV hs) $ Map.toList pMap
  where showKV :: Bool -> (PatId, PlayState) -> String
        showKV True  (k, (PlayState _  _ True _)) = k ++ " - solo\n"
        showKV True  (k, _) = "(" ++ k ++ ")\n"
        showKV False (k, (PlayState _ False _ _)) = k ++ "\n"
        showKV False (k, _) = "(" ++ k ++ ") - muted\n"

-- Evaluation of pat is forced so exceptions are picked up here, before replacing the existing pattern.
streamReplace :: Show a => Stream -> a -> ControlPattern -> IO ()
streamReplace s k pat
  = E.catch (do let x = queryArc pat (Arc 0 0)
                pMap <- seq x $ takeMVar $ sPMapMV s
                let playState = updatePS $ Map.lookup (show k) pMap
                putMVar (sPMapMV s) $ Map.insert (show k) playState pMap
                calcOutput s
                return ()
          )
    (\(e :: E.SomeException) -> putStrLn $ "Error in pattern: " ++ show e
    )
  where updatePS (Just playState) = do playState {pattern = pat, history = pat:(history playState)}
        updatePS Nothing = PlayState pat False False []

streamMute :: Show a => Stream -> a -> IO ()
streamMute s k = withPatId s (show k) (\x -> x {mute = True})

streamMutes :: Show a => Stream -> [a] -> IO ()
streamMutes s ks = withPatIds s (map show ks) (\x -> x {mute = True})

streamUnmute :: Show a => Stream -> a -> IO ()
streamUnmute s k = withPatId s (show k) (\x -> x {mute = False})

streamSolo :: Show a => Stream -> a -> IO ()
streamSolo s k = withPatId s (show k) (\x -> x {solo = True})

streamUnsolo :: Show a => Stream -> a -> IO ()
streamUnsolo s k = withPatId s (show k) (\x -> x {solo = False})

streamOnce :: Stream -> Bool -> ControlPattern -> IO ()
streamOnce st asap p
  = do cMap <- readMVar (sInput st)
       tempo <- readMVar (sTempoMV st)
       now <- O.time
       let target = if asap
                    then (sTarget st) {oLatency = 0}
                    else sTarget st
           fakeTempo = T.Tempo {T.cps = T.cps tempo,
                                T.atCycle = 0,
                                T.atTime = now,
                                T.paused = False,
                                T.nudged = 0
                               }
           es = filter eventHasOnset $ query p (State {arc = (Arc 0 1),
                                                       controls = cMap
                                                      }
                                               )
           at e = sched fakeTempo $ start $ whole e
           on e = sched tempo $ start $ whole e
           cpsChanges = map (\e -> (on e - now, Map.lookup "cps" $ value e)) es
           messages = map (\e -> (at e, toMessage target fakeTempo e)) es
       E.catch (mapM_ (send (oLatency target) (sUDP st)) messages)
         (\(msg ::E.SomeException)
          -> putStrLn $ "Failed to send. Is the target (probably superdirt) running? " ++ show (msg :: E.SomeException))
       mapM_ (doCps $ sTempoMV st) cpsChanges
       return ()

withPatId :: Stream -> PatId -> (PlayState -> PlayState) -> IO ()
withPatId s k f = withPatIds s [k] f

withPatIds :: Stream -> [PatId] -> (PlayState -> PlayState) -> IO ()
withPatIds s ks f
  = do playMap <- takeMVar $ sPMapMV s
       let pMap' = foldr (Map.update (\x -> Just $ f x)) playMap ks
       putMVar (sPMapMV s) pMap'
       calcOutput s
       return ()

-- TODO - is there a race condition here?
streamMuteAll :: Stream -> IO ()
streamMuteAll s = do modifyMVar_ (sOutput s) $ return . const silence
                     modifyMVar_ (sPMapMV s) $ return . fmap (\x -> x {mute = True})

streamHush :: Stream -> IO ()
streamHush s = do modifyMVar_ (sOutput s) $ return . const silence
                  modifyMVar_ (sPMapMV s) $ return . fmap (\x -> x {pattern = silence})

streamUnmuteAll :: Stream -> IO ()
streamUnmuteAll s = do modifyMVar_ (sPMapMV s) $ return . fmap (\x -> x {mute = False})
                       calcOutput s

calcOutput :: Stream -> IO ()
calcOutput s = do pMap <- readMVar $ sPMapMV s
                  _ <- swapMVar (sOutput s) $ toPat pMap
                  return ()
  where toPat pMap =
          stack $ map pattern $ filter (\pState -> if hasSolo pMap
                                                   then solo pState
                                                   else not (mute pState)
                                       ) (Map.elems pMap)

startTidal :: OSCTarget -> Config -> IO Stream
startTidal target config =
  do cMapMV <- newMVar (Map.empty :: ControlMap)
     listenTid <- ctrlListen cMapMV config
     (pMV, tempoMV, u) <- startStream config cMapMV target
     pMapMV <- newMVar Map.empty
     return $ Stream {sConfig = config,
                      sInput = cMapMV,
                      sListenTid = listenTid,
                      sOutput = pMV,
                      sPMapMV = pMapMV,
                      sTempoMV = tempoMV,
                      sTarget = target,
                      sUDP = u
                     }
ctrlListen :: MVar ControlMap -> Config -> IO (Maybe ThreadId)
ctrlListen cMapMV c
  | cCtrlListen c = do putStrLn $ "Listening for controls on " ++ cCtrlAddr c ++ ":" ++ show (cCtrlPort c)
                       catchAny run (\_ -> do putStrLn $ "Control listen failed. Perhaps there's already another tidal instance listening on that port?"
                                              return Nothing
                                    )
  | otherwise  = return Nothing
  where
        run = do sock <- O.udpServer (cCtrlAddr c) (cCtrlPort c)
                 tid <- forkIO $ loop sock
                 return $ Just tid
        loop sock = do ms <- O.recvMessages sock
                       mapM_ act ms
                       loop sock
        act (O.Message x (O.Int32 k:v:[]))
          = act (O.Message x [O.string $ show k,v])
        act (O.Message _ (O.ASCII_String k:v@(O.Float _):[]))
          = add (O.ascii_to_string k) (VF $ fromJust $ O.datum_floating v)
        act (O.Message _ (O.ASCII_String k:O.ASCII_String v:[]))
          = add (O.ascii_to_string k) (VS $ O.ascii_to_string v)
        act (O.Message _ (O.ASCII_String k:O.Int32 v:[]))
          = add (O.ascii_to_string k) (VI $ fromIntegral v)
        act m = putStrLn $ "Unhandled OSC: " ++ show m
        add :: String -> Value -> IO ()
        add k v = do cMap <- takeMVar cMapMV
                     putMVar cMapMV $ Map.insert k v cMap
                     return ()
        catchAny :: IO a -> (E.SomeException -> IO a) -> IO a
        catchAny = E.catch

{-
listenCMap :: MVar ControlMap -> IO ()
listenCMap cMapMV = do sock <- O.udpServer "127.0.0.1" (6011)
                       _ <- forkIO $ loop sock
                       return ()
  where loop sock =
          do ms <- O.recvMessages sock
             mapM_ readMessage ms
             loop sock
        readMessage (O.Message _ (O.ASCII_String k:v@(O.Float _):[])) = add (O.ascii_to_string k) (VF $ fromJust $ O.datum_floating v)
        readMessage (O.Message _ (O.ASCII_String k:O.ASCII_String v:[])) = add (O.ascii_to_string k) (VS $ O.ascii_to_string v)
        readMessage (O.Message _ (O.ASCII_String k:O.Int32 v:[]))  = add (O.ascii_to_string k) (VI $ fromIntegral v)
        readMessage _ = return ()
        add :: String -> Value -> IO ()
        add k v = do cMap <- takeMVar cMapMV
                     putMVar cMapMV $ Map.insert k v cMap
                     return ()
-}

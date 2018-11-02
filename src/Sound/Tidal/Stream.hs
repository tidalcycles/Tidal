
{-# LANGUAGE ConstraintKinds, GeneralizedNewtypeDeriving, FlexibleContexts, ScopedTypeVariables #-}

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

import qualified Control.Exception as E
-- import Control.Monad.Reader
-- import Control.Monad.Except
-- import qualified Data.Bifunctor as BF
-- import qualified Data.Bool as B
-- import qualified Data.Char as C

data TimeStamp = BundleStamp | MessageStamp | NoStamp
 deriving Eq


data Config = Config {cCtrlListen :: Bool,
                      cCtrlAddr :: String,
                      cCtrlPort :: Int
                     }

data Stream = Stream {sConfig :: Config,
                      sInput :: MVar ControlMap,
                      sOutput :: MVar ControlPattern,
                      sListenTid :: Maybe ThreadId,
                      sPMapMV :: MVar PlayMap,
                      sTempoMV :: MVar T.Tempo,
                      sTarget :: OSCTarget,
                      sUDP :: O.UDP
                     }

defaultConfig :: Config
defaultConfig = Config {cCtrlListen = True,
                        cCtrlAddr ="127.0.0.1",
                        cCtrlPort = 6010
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
                             oLatency = 0.01,
                             oPreamble = [],
                             oTimestamp = BundleStamp
                            }

startStream :: MVar ControlMap -> OSCTarget -> IO (MVar ControlPattern, MVar T.Tempo, O.UDP)
startStream cMapMV target = do u <- O.openUDP (oAddress target) (oPort target)
                               pMV <- newMVar empty
                               (tempoMV, _) <- T.clocked $ onTick cMapMV pMV target u
                               return $ (pMV, tempoMV, u)


data PlayState = PlayState {pattern :: ControlPattern,
                            mute :: Bool,
                            solo :: Bool,
                            history :: [ControlPattern]
                           }
               deriving Show

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

     E.catch (mapM_ (send target u) messages)
       (\(_ ::E.SomeException)
        -> putStrLn $ "Failed to send. Is the target (probably superdirt) running?")
                -- ++ show (msg :: E.SomeException))
     mapM_ doCps cpsChanges
     return ()
  where doCps (d, Just (VF cps)) = do _ <- forkIO $ do threadDelay $ floor $ d * 1000000
                                                       _ <- T.setCps tempoMV cps
                                                       return ()
                                      return ()
        doCps _ = return ()

send :: O.Transport t => OSCTarget -> t -> (Double, O.Message) -> IO ()
send target u (time, m) = O.sendOSC u $ O.Bundle (time + (oLatency target)) [m]

sched :: T.Tempo -> Rational -> Double
sched tempo c = ((fromRational $ c - (T.atCycle tempo)) / T.cps tempo) + (T.atTime tempo)

-- Interaction

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

streamReplace :: Show a => Stream -> a -> ControlPattern -> IO ()
streamReplace s k pat
  = do pMap <- takeMVar $ sPMapMV s
       let playState = updatePS $ Map.lookup (show k) pMap
       putMVar (sPMapMV s) $ Map.insert (show k) playState pMap
       calcOutput s
       return ()
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
           es = filter eventHasOnset $ query p (State {arc = (0,1),
                                                       controls = cMap
                                                      }
                                               )
           at e = sched fakeTempo $ fst $ eventWhole e
           messages = map (\e -> (at e, toMessage e)) es
           toMessage e = O.Message (oPath target) $ oPreamble target ++ toData e
       E.catch (mapM_ (send target (sUDP st)) messages)
         (\(msg ::E.SomeException)
          -> putStrLn $ "Failed to send. Is the target (probably superdirt) running? " ++ show (msg :: E.SomeException))
                    
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
streamHush :: Stream -> IO ()
streamHush s = do modifyMVar_ (sOutput s) $ return . const silence
                  modifyMVar_ (sPMapMV s) $ return . fmap (\x -> x {mute = True})

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
startTidal target c = do cMapMV <- newMVar (Map.empty :: ControlMap)
                         listenTid <- ctrlListen cMapMV c
                         (pMV, tempoMV, u) <- startStream cMapMV target
                         pMapMV <- newMVar Map.empty
                         return $ Stream {sConfig = defaultConfig,
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
  | cCtrlListen c = do x <- O.udpServer (cCtrlAddr c) (cCtrlPort c)
                       tid <- forkIO $ loop x
                       return $ Just tid
  | otherwise  = return Nothing
  where
        loop x = do m <- O.recvMessage x
                    act m
                    loop x
        act (Just (O.Message "/ctrl" [O.Float v, O.Float n, O.Float chan])) = 
          do putStrLn $ "got '" ++ show n ++ ":" ++ show v ++ ":" ++ show chan
             let name = "cc" ++ show (floor n :: Int)
                 value = v/127
             ctrl <- takeMVar cMapMV
             putMVar cMapMV $ Map.insert name (VF $ realToFrac value) ctrl
             return ()
        act _ = return ()

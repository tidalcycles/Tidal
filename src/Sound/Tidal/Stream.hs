{-# LANGUAGE ConstraintKinds, GeneralizedNewtypeDeriving, FlexibleContexts, ScopedTypeVariables, BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Sound.Tidal.Stream where

import           Control.Applicative ((<|>))
import           Control.Concurrent.MVar
import           Control.Concurrent
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust, fromMaybe, isJust, catMaybes)
import qualified Control.Exception as E
-- import Control.Monad.Reader
-- import Control.Monad.Except
-- import qualified Data.Bifunctor as BF
-- import qualified Data.Bool as B
-- import qualified Data.Char as C
import           System.IO (hPutStrLn, stderr)

import qualified Sound.OSC.FD as O

import           Sound.Tidal.Config
import           Sound.Tidal.Core (stack, silence)
import           Sound.Tidal.Pattern
import qualified Sound.Tidal.Tempo as T
-- import qualified Sound.OSC.Datum as O
import           Data.List (sortOn)
import           System.Random (getStdRandom, randomR)


data TimeStamp = BundleStamp | MessageStamp | NoStamp
 deriving (Eq, Show)

data Stream = Stream {sConfig :: Config,
                      sInput :: MVar StateMap,
                      sOutput :: MVar ControlPattern,
                      sListenTid :: Maybe ThreadId,
                      sPMapMV :: MVar PlayMap,
                      sTempoMV :: MVar T.Tempo,
                      sGlobalFMV :: MVar (ControlPattern -> ControlPattern),
                      sCxs :: [Cx]
                     }

type PatId = String

data Cx = Cx {cxTarget :: OSCTarget,
              cxUDP :: O.UDP
             }

data OSCTarget = OSCTarget {oName :: String,
                            oAddress :: String,
                            oPort :: Int,
                            oPath :: String,
                            oShape :: Maybe [(String, Maybe Value)],
                            oLatency :: Double,
                            oPreamble :: [O.Datum],
                            oTimestamp :: TimeStamp
                           }
                 deriving Show

superdirtTarget :: OSCTarget
superdirtTarget = OSCTarget {oName = "SuperDirt",
                             oAddress = "127.0.0.1",
                             oPort = 57120,
                             oPath = "/play2",
                             oShape = Nothing,
                             oLatency = 0.02,
                             oPreamble = [],
                             oTimestamp = BundleStamp
                            }

dirtTarget :: OSCTarget
dirtTarget = OSCTarget {oName = "Dirt",
                        oAddress = "127.0.0.1",
                        oPort = 7771,
                        oPath = "/play",
                        oShape = Just [("sec", Just $ VI 0),
                                       ("usec", Just $ VI 0),
                                       ("cps", Just $ VF 0),
                                       ("s", Nothing),
                                       ("offset", Just $ VF 0),
                                       ("begin", Just $ VF 0),
                                       ("end", Just $ VF 1),
                                       ("speed", Just $ VF 1),
                                       ("pan", Just $ VF 0.5),
                                       ("velocity", Just $ VF 0.5),
                                       ("vowel", Just $ VS ""),
                                       ("cutoff", Just $ VF 0),
                                       ("resonance", Just $ VF 0),
                                       ("accelerate", Just $ VF 0),
                                       ("shape", Just $ VF 0),
                                       ("kriole", Just $ VI 0),
                                       ("gain", Just $ VF 1),
                                       ("cut", Just $ VI 0),
                                       ("delay", Just $ VF 0),
                                       ("delaytime", Just $ VF (-1)),
                                       ("delayfeedback", Just $ VF (-1)),
                                       ("crush", Just $ VF 0),
                                       ("coarse", Just $ VI 0),
                                       ("hcutoff", Just $ VF 0),
                                       ("hresonance", Just $ VF 0),
                                       ("bandf", Just $ VF 0),
                                       ("bandq", Just $ VF 0),
                                       ("unit", Just $ VS "rate"),
                                       ("loop", Just $ VF 0),
                                       ("n", Just $ VF 0),
                                       ("attack", Just $ VF (-1)),
                                       ("hold", Just $ VF 0),
                                       ("release", Just $ VF (-1)),
                                       ("orbit", Just $ VI 0),
                                       ("id", Just $ VI 0)
                                      ],
                         oLatency = 0.02,
                         oPreamble = [],
                         oTimestamp = MessageStamp
                       }

startStream :: Config -> MVar StateMap -> [OSCTarget] -> IO (MVar ControlPattern, MVar T.Tempo, [Cx])
startStream config sMapMV targets
  = do cxs <- mapM (\target -> do u <- O.openUDP (oAddress target) (oPort target)
                                  return $ Cx {cxUDP = u,
                                               cxTarget = target
                                              }
                   ) targets
       pMV <- newMVar empty
       (tempoMV, _) <- T.clocked config $ onTick config sMapMV pMV cxs
       return $ (pMV, tempoMV, cxs)


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
toDatum (VR x) = O.float $ ((fromRational x) :: Double)
toDatum (VB True) = O.int32 (1 :: Int)
toDatum (VB False) = O.int32 (0 :: Int)

toData :: OSCTarget -> Event ControlMap -> Maybe [O.Datum]
toData target e
  | isJust (oShape target) = fmap (fmap toDatum) $ sequence $ map (\(n,v) -> Map.lookup n (value e) <|> v) (fromJust $ oShape target)
  | otherwise = Just $ concatMap (\(n,v) -> [O.string n, toDatum v]) $ Map.toList $ value e

toMessage :: Config -> Double -> OSCTarget -> T.Tempo -> Event (Map.Map String Value) -> Maybe O.Message
toMessage config t target tempo e = do vs <- toData target addExtra
                                       return $ O.Message (oPath target) $ oPreamble target ++ vs
  where on = sched tempo $ start $ wholeOrPart e
        off = sched tempo $ stop $ wholeOrPart e
        identifier = ((if (start $ wholeOrPart e) == (start $ part e) then "X" else ">")
                      ++ show (start $ wholeOrPart e)
                      ++ "-"
                      ++ show (stop $ wholeOrPart e)
                      ++ "-"
                      ++ getString "n"
                      ++ "-"
                      ++ getString "note"
                      ++ "-"
                      ++ getString "s"
                     )
        getString s = fromMaybe "" $ do v <- Map.lookup s $ value e
                                        return $ simpleShow v
        simpleShow (VS s) = s
        simpleShow (VI i) = show i
        simpleShow (VF f) = show f
        simpleShow (VR r) = show r
        simpleShow (VB b) = show b
        delta = off - on
        messageStamp = oTimestamp target == MessageStamp
        -- If there is already cps in the event, the union will preserve that.
        addExtra = (\v -> (Map.union v $ Map.fromList (extra messageStamp)
                          )) <$> e
        addIdentifier | cSendParts config = (("id", VS identifier):)
                      | otherwise = id
        extra False = addIdentifier [("cps", (VF $ T.cps tempo)),
                                     ("delta", VF delta),
                                     ("cycle", VF (fromRational $ start $ wholeOrPart e))
                                    ]
        extra True = timestamp ++ (extra False)
        timestamp = [("sec", VI sec),
                     ("usec", VI usec)
                    ]
        ut = O.ntpr_to_ut t
        sec = floor ut
        usec = floor $ 1000000 * (ut - (fromIntegral sec))

doCps :: MVar T.Tempo -> (Double, Maybe Value) -> IO ()
doCps tempoMV (d, Just (VF cps)) = do _ <- forkIO $ do threadDelay $ floor $ d * 1000000
                                                       -- hack to stop things from stopping !
                                                       _ <- T.setCps tempoMV (max 0.00001 cps)
                                                       return ()
                                      return ()
doCps _ _ = return ()

onTick :: Config -> MVar StateMap -> MVar ControlPattern -> [Cx] -> MVar T.Tempo -> T.State -> IO ()
onTick config sMapMV pMV cxs tempoMV st =
  do p <- readMVar pMV
     sMap <- readMVar sMapMV
     tempo <- takeMVar tempoMV
     let frameEnd = snd $ T.nowTimespan st
         sMap' = Map.insert "_cps" (pure $ VF $ T.cps tempo) sMap
         es = sortOn (start . part) $ filterOns $ query p (State {arc = T.nowArc st, controls = sMap'})
         filterOns | cSendParts config = id
                   | otherwise = filter eventHasOnset
           -- there should always be a whole (due to the eventHasOnset filter)
         on e tempo' = (sched tempo' $ start $ wholeOrPart e)
         eventNudge e = fromJust $ getF $ fromMaybe (VF 0) $ Map.lookup "nudge" $ value e
         processCps :: T.Tempo -> [Event ControlMap] -> ([(T.Tempo, Event ControlMap)], T.Tempo)
         processCps tempo [] = ([], tempo)
         processCps tempo (e:es) = (((tempo', e):es'), tempo'')
           where cps' = do x <- Map.lookup "cps" $ value e
                           getF x
                 tempo' = (maybe tempo (\newCps -> T.changeTempo' tempo newCps (eventPartStart e)) cps')
                 (es', tempo'') = processCps tempo' es
         latency target = oLatency target + cFrameTimespan config + T.nudged tempo
         (tes, tempo') = processCps tempo es
     mapM_ (\(Cx target udp) -> (do let ms = catMaybes $ map (\(t, e) -> do let nudge = eventNudge e
                                                                            let onset = on e t
                                                                            m <- toMessage config (onset + nudge + latency target) target tempo e
                                                                            -- drop events that have gone out of frame (due to tempo
                                                                            -- changes during the frame)
                                                                            if (onset < frameEnd)
                                                                              then Just (onset + nudge, m)
                                                                              else  Nothing
                                                             ) tes
                                    E.catch (mapM_ (send target (latency target) udp) ms)
                                )
                       (\(e ::E.SomeException)
                        -> putStrLn $ "Failed to send. Is the '" ++ oName target ++ "' target running? " ++ show e
                       )
           ) cxs
     putMVar tempoMV tempo'
     return ()

send :: O.Transport t => OSCTarget -> Double -> t -> (Double, O.Message) -> IO ()
send target latency u (time, m)
  | oTimestamp target == BundleStamp = O.sendBundle u $ O.Bundle (time + latency) [m]
  | oTimestamp target == MessageStamp = O.sendMessage u m
  | otherwise = do _ <- forkIO $ do now <- O.time
                                    threadDelay $ floor $ ((time+latency) - now) * 1000000
                                    O.sendMessage u m
                   return ()

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
streamReplace s k !pat
  = E.catch (do let x = queryArc pat (Arc 0 0)
                tempo <- readMVar $ sTempoMV s
                input <- takeMVar $ sInput s
                -- put change time in control input
                now <- O.time
                let cyc = T.timeToCycles tempo now
                putMVar (sInput s) $
                  Map.insert ("_t_all") (pure $ VR cyc) $ Map.insert ("_t_" ++ show k) (pure $ VR cyc) input
                -- update the pattern itself
                pMap <- seq x $ takeMVar $ sPMapMV s
                let playState = updatePS $ Map.lookup (show k) pMap
                putMVar (sPMapMV s) $ Map.insert (show k) playState pMap
                calcOutput s
                return ()
          )
    (\(e :: E.SomeException) -> hPutStrLn stderr $ "Error in pattern: " ++ show e
    )
  where updatePS (Just playState) = do playState {pattern = pat, history = pat:(history playState)}
        updatePS Nothing = PlayState pat False False [pat]

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

streamOnce :: Stream -> ControlPattern -> IO ()
streamOnce st p = do i <- getStdRandom $ randomR (0, 8192)
                     streamFirst st $ rotL (toRational (i :: Int)) p

streamFirst :: Stream -> ControlPattern -> IO ()
streamFirst st p
  = do sMap <- readMVar (sInput st)
       tempo <- readMVar (sTempoMV st)
       now <- O.time
       let fakeTempo = T.Tempo {T.cps = T.cps tempo,
                                T.atCycle = 0,
                                T.atTime = now,
                                T.paused = False,
                                T.nudged = 0
                               }
           sMap' = Map.insert "_cps" (pure $ VF $ T.cps tempo) sMap
           es = filter eventHasOnset $ query p (State {arc = (Arc 0 1),
                                                       controls = sMap'
                                                      }
                                               )
           -- there should always be a whole (due to the eventHasOnset filter)
           at e = sched fakeTempo $ start $ wholeOrPart e
           -- there should always be a whole (due to the eventHasOnset filter)
           on e = sched tempo $ start $ wholeOrPart e
           cpsChanges = map (\e -> (on e - now, Map.lookup "cps" $ value e)) es
           config = sConfig st
           messages target =
             catMaybes $ map (\e -> do m <- toMessage config (at e + (oLatency target)) target fakeTempo e
                                       return $ (at e, m)
                             ) es
       mapM_ (\(Cx target udp) ->
                 E.catch (mapM_ (send target (oLatency target) udp) (messages target))
                 (\(e ::E.SomeException)
                   -> putStrLn $ "Failed to send. Is the '" ++ oName target ++ "' target running? " ++ show e
                 )
             ) (sCxs st)
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
                  modifyMVar_ (sPMapMV s) $ return . fmap (\x -> x {pattern = silence, history = silence:history x})

streamUnmuteAll :: Stream -> IO ()
streamUnmuteAll s = do modifyMVar_ (sPMapMV s) $ return . fmap (\x -> x {mute = False})
                       calcOutput s


streamAll :: Stream -> (ControlPattern -> ControlPattern) -> IO ()
streamAll s f = do _ <- swapMVar (sGlobalFMV s) f
                   calcOutput s

streamSet :: Valuable a => Stream -> String -> Pattern a -> IO ()
streamSet s k pat = do sMap <- takeMVar $ sInput s
                       let pat' = toValue <$> pat
                           sMap' = Map.insert k pat' sMap
                       putMVar (sInput s) $ sMap'

streamSetI :: Stream -> String -> Pattern Int -> IO ()
streamSetI = streamSet

streamSetF :: Stream -> String -> Pattern Double -> IO ()
streamSetF = streamSet

streamSetS :: Stream -> String -> Pattern String -> IO ()
streamSetS = streamSet

streamSetB :: Stream -> String -> Pattern Bool -> IO ()
streamSetB = streamSet

streamSetR :: Stream -> String -> Pattern Rational -> IO ()
streamSetR = streamSet

calcOutput :: Stream -> IO ()
calcOutput s = do pMap <- readMVar $ sPMapMV s
                  globalF <- (readMVar $ sGlobalFMV s)
                  _ <- swapMVar (sOutput s) $ globalF $ toPat $ pMap
                  return ()
  where toPat pMap =
          stack $ map pattern $ filter (\pState -> if hasSolo pMap
                                                   then solo pState
                                                   else not (mute pState)
                                       ) (Map.elems pMap)

startTidal :: OSCTarget -> Config -> IO Stream
startTidal target config = startMulti [target] config

startMulti :: [OSCTarget] -> Config -> IO Stream
startMulti targets config =
  do sMapMV <- newMVar (Map.empty :: StateMap)
     listenTid <- ctrlListen sMapMV config
     (pMV, tempoMV, cxs) <- startStream config sMapMV targets
     pMapMV <- newMVar Map.empty
     globalFMV <- newMVar id
     return $ Stream {sConfig = config,
                      sInput = sMapMV,
                      sListenTid = listenTid,
                      sOutput = pMV,
                      sPMapMV = pMapMV,
                      sTempoMV = tempoMV,
                      sGlobalFMV = globalFMV,
                      sCxs = cxs
                     }

ctrlListen :: MVar StateMap -> Config -> IO (Maybe ThreadId)
ctrlListen sMapMV c
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
        add k v = do sMap <- takeMVar sMapMV
                     putMVar sMapMV $ Map.insert k (pure v) sMap
                     return ()
        catchAny :: IO a -> (E.SomeException -> IO a) -> IO a
        catchAny = E.catch





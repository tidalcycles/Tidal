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
import           Sound.Tidal.Show ()

data Stream = Stream {sConfig :: Config,
                      sInput :: MVar StateMap,
                      -- sOutput :: MVar ControlPattern,
                      sListenTid :: Maybe ThreadId,
                      sPMapMV :: MVar PlayMap,
                      sTempoMV :: MVar T.Tempo,
                      sGlobalFMV :: MVar (ControlPattern -> ControlPattern),
                      sCxs :: [Cx]
                     }

type PatId = String

data Cx = Cx {cxTarget :: Target,
              cxUDP :: O.UDP,
              cxOSCs :: [OSC]
             }
  deriving (Show)

data StampStyle = BundleStamp
                | MessageStamp
  deriving (Eq, Show)


data Schedule = Pre StampStyle
              | Live
  deriving (Eq, Show)

data Target = Target {oName :: String,
                      oAddress :: String,
                      oPort :: Int,
                      oLatency :: Double,
                      oWindow :: Maybe Arc,
                      oSchedule :: Schedule
                     }
                 deriving Show

data Args = Named {required :: [String]}
          | ArgList [(String, Maybe Value)]
         deriving Show

data OSC = OSC {path :: String,
                args :: Args
               }
         deriving Show

data PlayState = PlayState {pattern :: ControlPattern,
                            mute :: Bool,
                            solo :: Bool,
                            history :: [ControlPattern]
                           }
               deriving Show

type PlayMap = Map.Map PatId PlayState

superdirtTarget :: Target
superdirtTarget = Target {oName = "SuperDirt",
                          oAddress = "127.0.0.1",
                          oPort = 57120,
                          oLatency = 0.2,
                          oWindow = Nothing,
                          oSchedule = Pre BundleStamp
                         }

superdirtShape :: OSC
superdirtShape = OSC "/play2" $ Named {required = []}

dirtTarget :: Target
dirtTarget = Target {oName = "Dirt",
                     oAddress = "127.0.0.1",
                     oPort = 7771,
                     oLatency = 0.02,
                     oWindow = Nothing,
                     oSchedule = Pre MessageStamp
                    }

dirtShape :: OSC
dirtShape = OSC "/play" $ ArgList [("sec", Just $ VI 0),
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
                                  ]

startStream :: Config -> [(Target, [OSC])] -> IO Stream
startStream config oscmap 
  = do cxs <- mapM (\(target, os) -> do u <- O.openUDP (oAddress target) (oPort target)
                                        return $ Cx {cxUDP = u, cxTarget = target, cxOSCs = os}
                   ) oscmap

       sMapMV <- newMVar Map.empty
       pMapMV <- newMVar Map.empty
       globalFMV <- newMVar id
       listenTid <- ctrlListen sMapMV config
       tempoMV <- newEmptyMVar
       let stream = Stream {sConfig = config,
                            sInput = sMapMV,
                            sListenTid = listenTid,
                            sPMapMV = pMapMV,
                            sTempoMV = tempoMV,
                            sGlobalFMV = globalFMV,
                            sCxs = cxs
                           }
       T.clocked config tempoMV $ onTick stream
       return stream

startTidal :: Target -> Config -> IO Stream
startTidal target config = startStream config [(target, [superdirtShape])]

startMulti :: [a] -> Config -> IO ()
startMulti _ _ = putStrLn $ "startMulti has been removed, please check the latest documentation on tidalcycles.org"

toDatum :: Value -> O.Datum
toDatum (VF x) = O.float x
toDatum (VI x) = O.int32 x
toDatum (VS x) = O.string x
toDatum (VR x) = O.float $ ((fromRational x) :: Double)
toDatum (VB True) = O.int32 (1 :: Int)
toDatum (VB False) = O.int32 (0 :: Int)
toDatum (VX xs) = O.Blob $ O.blob_pack xs

toData :: OSC -> Event ControlMap -> Maybe [O.Datum]
toData (OSC {args = ArgList as}) e = fmap (fmap toDatum) $ sequence $ map (\(n,v) -> Map.lookup n (value e) <|> v) as
toData (OSC {args = Named rqrd}) e
  | hasRequired rqrd = Just $ concatMap (\(n,v) -> [O.string n, toDatum v]) $ Map.toList $ value e
  | otherwise = Nothing
  where hasRequired [] = True
        hasRequired xs = null $ filter (not . (`elem` ks)) xs
        ks = Map.keys (value e)

substitutePath :: String -> ControlMap -> String
substitutePath path cm = parse path
  where parse [] = []
        parse ('{':xs) = parseWord xs
        parse (x:xs) = x:(parse xs)
        parseWord xs | b == [] = getString cm a
                     | otherwise = getString cm a ++ parse (tail b)
          where (a,b) = break (== '}') xs

getString :: ControlMap -> String -> String
getString cm s = fromMaybe "" $ do v <- Map.lookup s cm
                                   return $ simpleShow v
                                    where simpleShow :: Value -> String
                                          simpleShow (VS str) = str
                                          simpleShow (VI i) = show i
                                          simpleShow (VF f) = show f
                                          simpleShow (VR r) = show r
                                          simpleShow (VB b) = show b
                                          simpleShow (VX xs) = show xs


playStack :: PlayMap -> ControlPattern
playStack pMap = stack $ map pattern active
  where active = filter (\pState -> if hasSolo pMap
                                    then solo pState
                                    else not (mute pState)
                        ) $ Map.elems pMap

toOSC :: Double -> Event ControlMap -> T.Tempo -> OSC -> Maybe (Double, O.Message)
toOSC latency e tempo osc = do vs <- toData osc addExtra
                               return (ts, O.Message (substitutePath (path osc) (value e)) vs)
       where on = sched tempo $ start $ wholeOrPart e
             off = sched tempo $ stop $ wholeOrPart e
             delta = off - on
             -- If there is already cps in the event, the union will preserve that.
             addExtra = (\v -> (Map.union v extra)) <$> e
             extra = Map.fromList [("cps", (VF $ T.cps tempo)),
                                   ("delta", VF delta),
                                   ("cycle", VF (fromRational $ start $ wholeOrPart e))
                                  ]
             ts = on + nudge + latency
             nudge = fromJust $ getF $ fromMaybe (VF 0) $ Map.lookup "nudge" $ value e

doCps :: MVar T.Tempo -> (Double, Maybe Value) -> IO ()
doCps tempoMV (d, Just (VF cps)) = do _ <- forkIO $ do threadDelay $ floor $ d * 1000000
                                                       -- hack to stop things from stopping !
                                                       _ <- T.setCps tempoMV (max 0.00001 cps)
                                                       return ()
                                      return ()
doCps _ _ = return ()

onTick :: Stream -> T.State -> IO ()
onTick stream st
  = do doTick False stream st

processCps :: T.Tempo -> [Event ControlMap] -> ([(T.Tempo, Event ControlMap)], T.Tempo)
processCps t [] = ([], t)
-- If an event has a tempo change, that affects the following events..
processCps t (e:evs) = (((t', e):es'), t'')
  where cps' = do x <- Map.lookup "cps" $ value e
                  getF x
        t' = (maybe t (\newCps -> T.changeTempo' t newCps (eventPartStart e)) cps')
        (es', t'') = processCps t' evs

streamOnce :: Stream -> ControlPattern -> IO ()
streamOnce st p = do i <- getStdRandom $ randomR (0, 8192)
                     streamFirst st $ rotL (toRational (i :: Int)) p

streamFirst :: Stream -> ControlPattern -> IO ()
streamFirst stream pat = do now <- O.time
                            tempo <- readMVar (sTempoMV stream)
                            pMapMV <- newMVar $ Map.singleton "fake"
                                      (PlayState {pattern = pat,
                                                  mute = False,
                                                  solo = False,
                                                  history = []
                                                 }
                                      )
                            let cps = T.cps tempo
                                state = T.State {T.ticks = 0,
                                                 T.start = now,
                                                 T.nowTimespan = (now, now + (1/cps)),
                                                 T.nowArc = (Arc 0 1)
                                                }
                            doTick True (stream {sPMapMV = pMapMV}) state

doTick :: Bool -> Stream -> T.State -> IO ()
doTick first stream st =
  do tempo <- takeMVar (sTempoMV stream)
     pMap <- readMVar (sPMapMV stream)
     sMap <- readMVar (sInput stream)
     -- putStrLn $ show st
     let config = sConfig stream
         cxs = sCxs stream
         pat = playStack pMap
         frameEnd = snd $ T.nowTimespan st
         -- add cps to state
         sMap' = Map.insert "_cps" (pure $ VF $ T.cps tempo) sMap
         filterOns = filter eventHasOnset
         --filterOns | cSendParts config = id
         --          | otherwise = filter eventHasOnset
         es = sortOn (start . part) $ filterOns $ query pat (State {arc = T.nowArc st,
                                                                    controls = sMap'
                                                                   }
                                                            )
         schedtempo | first = tempo {T.atCycle = 0,
                                     T.atTime = T.start st
                                    }
                    | otherwise = tempo
         -- TODO onset is calculated in toOSC as well..
         on e tempo'' = (sched tempo'' $ start $ wholeOrPart e)
         (tes, tempo') = processCps tempo es
     mapM_ (\cx@(Cx target _ oscs) ->
              (do let latency = oLatency target + cFrameTimespan config + T.nudged tempo
                      ms = concatMap (\(t, e) ->
                                        if (first || (on e t) < frameEnd)
                                        then catMaybes $ map (toOSC latency e schedtempo) oscs
                                        else []
                                     ) tes
                  E.catch (mapM_ (send cx) ms)
              )
              (\(e ::E.SomeException)
                -> putStrLn $ "Failed to send. Is the '" ++ oName target ++ "' target running? " ++ show e
              )
           ) cxs
     putMVar (sTempoMV stream) tempo'
     return ()

send :: Cx -> (Double, O.Message) -> IO ()
send cx (time, m)
  | oSchedule target == Pre BundleStamp = O.sendBundle u $ O.Bundle time [m]
  | oSchedule target == Pre MessageStamp = O.sendMessage u $ addtime m
  | otherwise = do _ <- forkIO $ do now <- O.time
                                    threadDelay $ floor $ (time - now) * 1000000
                                    O.sendMessage u m
                   return ()
    where addtime (O.Message path params) = O.Message path ((O.int32 sec):((O.int32 usec):params))
          ut = O.ntpr_to_ut time
          sec = floor ut
          usec = floor $ 1000000 * (ut - (fromIntegral sec))
          u = cxUDP cx
          target = cxTarget cx
         -- latency target = oLatency target + cFrameTimespan config + T.nudged tempo

sched :: T.Tempo -> Rational -> Double
sched tempo c = ((fromRational $ c - (T.atCycle tempo)) / T.cps tempo)
                + (T.atTime tempo)

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
        showKV True  (k, (PlayState {solo = True})) = k ++ " - solo\n"
        showKV True  (k, _) = "(" ++ k ++ ")\n"
        showKV False (k, (PlayState {solo = False})) = k ++ "\n"
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

withPatId :: Stream -> PatId -> (PlayState -> PlayState) -> IO ()
withPatId s k f = withPatIds s [k] f

withPatIds :: Stream -> [PatId] -> (PlayState -> PlayState) -> IO ()
withPatIds s ks f
  = do playMap <- takeMVar $ sPMapMV s
       let pMap' = foldr (Map.update (\x -> Just $ f x)) playMap ks
       putMVar (sPMapMV s) pMap'
       return ()

-- TODO - is there a race condition here?
streamMuteAll :: Stream -> IO ()
streamMuteAll s = modifyMVar_ (sPMapMV s) $ return . fmap (\x -> x {mute = True})

streamHush :: Stream -> IO ()
streamHush s = modifyMVar_ (sPMapMV s) $ return . fmap (\x -> x {pattern = silence, history = silence:history x})

streamUnmuteAll :: Stream -> IO ()
streamUnmuteAll s = modifyMVar_ (sPMapMV s) $ return . fmap (\x -> x {mute = False})

streamAll :: Stream -> (ControlPattern -> ControlPattern) -> IO ()
streamAll s f = do _ <- swapMVar (sGlobalFMV s) f
                   return ()

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





{-# LANGUAGE ConstraintKinds, GeneralizedNewtypeDeriving, FlexibleContexts, ScopedTypeVariables, BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# language DeriveGeneric, StandaloneDeriving #-}

module Sound.Tidal.Stream where

{-
    Stream.hs - Tidal's thingie for turning patterns into OSC streams
    Copyright (C) 2020, Alex McLean and contributors

    This library is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this library.  If not, see <http://www.gnu.org/licenses/>.
-}

import           Control.Applicative ((<|>))
import           Control.Concurrent.MVar
import           Control.Concurrent
import           Control.Monad (forM_, when)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust, fromMaybe, catMaybes, isJust)
import qualified Control.Exception as E
import           System.IO (hPutStrLn, stderr)

import qualified Sound.OSC.FD as O
import qualified Network.Socket          as N

import           Sound.Tidal.Config
import           Sound.Tidal.Core (stack, silence, (#))
import           Sound.Tidal.ID
import           Sound.Tidal.Params (pS)
import           Sound.Tidal.Pattern
import qualified Sound.Tidal.Tempo as T
import           Sound.Tidal.Utils ((!!!))
-- import qualified Sound.OSC.Datum as O
import           Data.List (sortOn)
import           System.Random (getStdRandom, randomR)
import           Sound.Tidal.Show ()
import           Data.Word (Word8)

import           Sound.Tidal.Version

data Stream = Stream {sConfig :: Config,
                      sBusses :: MVar [Int],
                      sStateMV :: MVar ValueMap,
                      -- sOutput :: MVar ControlPattern,
                      sListen :: Maybe O.UDP,
                      sPMapMV :: MVar PlayMap,
                      sTempoMV :: MVar T.Tempo,
                      sGlobalFMV :: MVar (ControlPattern -> ControlPattern),
                      sCxs :: [Cx]
                     }

type PatId = String

data Cx = Cx {cxTarget :: Target,
              cxUDP :: O.UDP,
              cxOSCs :: [OSC],
              cxAddr :: N.AddrInfo,
              cxBusAddr :: Maybe N.AddrInfo
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
                      oBusPort :: Maybe Int,
                      oLatency :: Double,
                      oWindow :: Maybe Arc,
                      oSchedule :: Schedule,
                      oHandshake :: Bool
                     }
                 deriving Show

data Args = Named {requiredArgs :: [String]}
          | ArgList [(String, Maybe Value)]
         deriving Show

data OSC = OSC {path :: String,
                args :: Args
               }
         | OSCContext {path :: String}
         deriving Show

data PlayState = PlayState {pattern :: ControlPattern,
                            mute :: Bool,
                            solo :: Bool,
                            history :: [ControlPattern]
                           }
               deriving Show

type PlayMap = Map.Map PatId PlayState


sDefault :: String -> Maybe Value
sDefault x = Just $ VS x
fDefault :: Double -> Maybe Value
fDefault x = Just $ VF x
rDefault :: Rational -> Maybe Value
rDefault x = Just $ VR x
iDefault :: Int -> Maybe Value
iDefault x = Just $ VI x
bDefault :: Bool -> Maybe Value
bDefault x = Just $ VB x
xDefault :: [Word8] -> Maybe Value
xDefault x = Just $ VX x

required :: Maybe Value
required = Nothing

superdirtTarget :: Target
superdirtTarget = Target {oName = "SuperDirt",
                          oAddress = "127.0.0.1",
                          oPort = 57120,
                          oBusPort = Just 57110,
                          oLatency = 0.2,
                          oWindow = Nothing,
                          oSchedule = Pre BundleStamp,
                          oHandshake = True
                         }

superdirtShape :: OSC
superdirtShape = OSC "/dirt/play" $ Named {requiredArgs = ["s"]}

dirtTarget :: Target
dirtTarget = Target {oName = "Dirt",
                     oAddress = "127.0.0.1",
                     oPort = 7771,
                     oBusPort = Nothing,
                     oLatency = 0.02,
                     oWindow = Nothing,
                     oSchedule = Pre MessageStamp,
                     oHandshake = False
                    }

dirtShape :: OSC
dirtShape = OSC "/play" $ ArgList [("cps", fDefault 0),
                                   ("s", required),
                                   ("offset", fDefault 0),
                                   ("begin", fDefault 0),
                                   ("end", fDefault 1),
                                   ("speed", fDefault 1),
                                   ("pan", fDefault 0.5),
                                   ("velocity", fDefault 0.5),
                                   ("vowel", sDefault ""),
                                   ("cutoff", fDefault 0),
                                   ("resonance", fDefault 0),
                                   ("accelerate", fDefault 0),
                                   ("shape", fDefault 0),
                                   ("kriole", iDefault 0),
                                   ("gain", fDefault 1),
                                   ("cut", iDefault 0),
                                   ("delay", fDefault 0),
                                   ("delaytime", fDefault (-1)),
                                   ("delayfeedback", fDefault (-1)),
                                   ("crush", fDefault 0),
                                   ("coarse", iDefault 0),
                                   ("hcutoff", fDefault 0),
                                   ("hresonance", fDefault 0),
                                   ("bandf", fDefault 0),
                                   ("bandq", fDefault 0),
                                   ("unit", sDefault "rate"),
                                   ("loop", fDefault 0),
                                   ("n", fDefault 0),
                                   ("attack", fDefault (-1)),
                                   ("hold", fDefault 0),
                                   ("release", fDefault (-1)),
                                   ("orbit", iDefault 0) -- ,
                                   -- ("id", iDefault 0)
                                  ]

startStream :: Config -> [(Target, [OSC])] -> IO Stream
startStream config oscmap 
  = do sMapMV <- newMVar Map.empty
       pMapMV <- newMVar Map.empty
       bussesMV <- newMVar []
       globalFMV <- newMVar id
       tempoMV <- newEmptyMVar

       tidal_status_string >>= verbose config
       verbose config $ "Listening for external controls on " ++ cCtrlAddr config ++ ":" ++ show (cCtrlPort config)
       listen <- openListener config

       cxs <- mapM (\(target, os) -> do remote_addr <- resolve (oAddress target) (show $ oPort target)
                                        remote_bus_addr <- if isJust $ oBusPort target
                                                           then Just <$> resolve (oAddress target) (show $ fromJust $ oBusPort target)
                                                           else return Nothing
                                        --u <- O.openUDP (oAddress target) (oPort target)
                                        u <- O.udp_socket (\sock sockaddr -> do N.setSocketOption sock N.Broadcast 1
                                                                                N.connect sock sockaddr
                                                          ) (oAddress target) (oPort target)
                                        return $ Cx {cxUDP = u, cxAddr = remote_addr, cxBusAddr = remote_bus_addr, cxTarget = target, cxOSCs = os}                                        
                   ) oscmap
       let stream = Stream {sConfig = config,
                            sBusses = bussesMV,
                            sStateMV  = sMapMV,
                            sListen = listen,
                            sPMapMV = pMapMV,
                            sTempoMV = tempoMV,
                            sGlobalFMV = globalFMV,
                            sCxs = cxs
                           }
       sendHandshakes stream
       _ <- T.clocked config tempoMV $ onTick stream
       _ <- forkIO $ ctrlResponder 0 config stream
       return stream

-- It only really works to handshake with one target at the moment..
sendHandshakes :: Stream -> IO ()
sendHandshakes stream = mapM_ sendHandshake $ filter (oHandshake . cxTarget) (sCxs stream)
  where sendHandshake cx = if (isJust $ sListen stream)
                           then                                            
                             do -- send it _from_ the udp socket we're listening to, so the
                                -- replies go back there
                                sendO False (sListen stream) cx $ O.Message "/dirt/handshake" []
                           else
                             hPutStrLn stderr "Can't handshake with SuperCollider without control port."

sendO :: Bool -> (Maybe O.UDP) -> Cx -> O.Message -> IO ()
sendO isBusMsg (Just listen) cx msg = O.sendTo listen (O.Packet_Message msg) (N.addrAddress addr)
  where addr | isBusMsg && isJust (cxBusAddr cx) = fromJust $ cxBusAddr cx
             | otherwise = cxAddr cx
sendO _ Nothing cx msg = O.sendMessage (cxUDP cx) msg

sendBndl :: Bool -> (Maybe O.UDP) -> Cx -> O.Bundle -> IO ()
sendBndl isBusMsg (Just listen) cx bndl = O.sendTo listen (O.Packet_Bundle bndl) (N.addrAddress addr)
  where addr | isBusMsg && isJust (cxBusAddr cx) = fromJust $ cxBusAddr cx
             | otherwise = cxAddr cx
sendBndl _ Nothing cx bndl = O.sendBundle (cxUDP cx) bndl

resolve :: String -> String -> IO N.AddrInfo
resolve host port = do let hints = N.defaultHints { N.addrSocketType = N.Stream }
                       addr:_ <- N.getAddrInfo (Just hints) (Just host) (Just port)
                       return addr

startTidal :: Target -> Config -> IO Stream
startTidal target config = startStream config [(target, [superdirtShape])]

startMulti :: [Target] -> Config -> IO ()
startMulti _ _ = hPutStrLn stderr $ "startMulti has been removed, please check the latest documentation on tidalcycles.org"

toDatum :: Value -> O.Datum
toDatum (VF x) = O.float x
toDatum (VN x) = O.float x
toDatum (VI x) = O.int32 x
toDatum (VS x) = O.string x
toDatum (VR x) = O.float $ ((fromRational x) :: Double)
toDatum (VB True) = O.int32 (1 :: Int)
toDatum (VB False) = O.int32 (0 :: Int)
toDatum (VX xs) = O.Blob $ O.blob_pack xs
toDatum _ = error "toDatum: unhandled value"
  
toData :: OSC -> Event ValueMap -> Maybe [O.Datum]
toData (OSC {args = ArgList as}) e = fmap (fmap (toDatum)) $ sequence $ map (\(n,v) -> Map.lookup n (value e) <|> v) as
toData (OSC {args = Named rqrd}) e
  | hasRequired rqrd = Just $ concatMap (\(n,v) -> [O.string n, toDatum v]) $ Map.toList $ value e
  | otherwise = Nothing
  where hasRequired [] = True
        hasRequired xs = null $ filter (not . (`elem` ks)) xs
        ks = Map.keys (value e)
toData _ _ = Nothing

substitutePath :: String -> ValueMap -> Maybe String
substitutePath str cm = parse str
  where parse [] = Just []
        parse ('{':xs) = parseWord xs
        parse (x:xs) = do xs' <- parse xs
                          return (x:xs')
        parseWord xs | b == [] = getString cm a
                     | otherwise = do v <- getString cm a
                                      xs' <- parse (tail b)
                                      return $ v ++ xs'
          where (a,b) = break (== '}') xs

getString :: ValueMap -> String -> Maybe String
getString cm s = (simpleShow <$> Map.lookup param cm) <|> defaultValue dflt
                      where (param, dflt) = break (== '=') s
                            simpleShow :: Value -> String
                            simpleShow (VS str) = str
                            simpleShow (VI i) = show i
                            simpleShow (VF f) = show f
                            simpleShow (VN n) = show n
                            simpleShow (VR r) = show r
                            simpleShow (VB b) = show b
                            simpleShow (VX xs) = show xs
                            simpleShow (VState _) = show "<stateful>"
                            simpleShow (VPattern _) = show "<pattern>"
                            simpleShow (VList _) = show "<list>"
                            defaultValue :: String -> Maybe String
                            defaultValue ('=':dfltVal) = Just dfltVal
                            defaultValue _ = Nothing

playStack :: PlayMap -> ControlPattern
playStack pMap = stack $ map pattern active
  where active = filter (\pState -> if hasSolo pMap
                                    then solo pState
                                    else not (mute pState)
                        ) $ Map.elems pMap

toOSC :: Double -> [Int] -> Event ValueMap -> T.Tempo -> OSC -> [(Double, Bool, O.Message)]
toOSC latency busses e tempo osc@(OSC _ _)
  = catMaybes (playmsg:busmsgs)
       where (playmap, busmap) = Map.partitionWithKey (\k _ -> null k || head k /= '^') $ value e
             -- swap in bus ids where needed
             playmap' = Map.union (Map.mapKeys tail $ Map.map (\(VI i) -> VS ('c':(show $ toBus i))) busmap) playmap
             addExtra = Map.union playmap' extra
             playmsg | eventHasOnset e = do vs <- toData osc (e {value = addExtra})
                                            mungedPath <- substitutePath (path osc) playmap'
                                            return (ts,
                                                    False, -- bus message ?
                                                    O.Message mungedPath vs
                                                   )
                     | otherwise = Nothing
             toBus n | null busses = n
                     | otherwise = busses !!! n
             busmsgs = map
                         (\(('^':k), (VI b)) -> do v <- Map.lookup k playmap
                                                   return $ (tsPart,
                                                             True, -- bus message ?
                                                             O.Message "/c_set" [O.int32 b, toDatum v]
                                                            )
                         )
                         (Map.toList busmap)
             onPart = sched tempo $ start $ part e
             on = sched tempo $ start $ wholeOrPart e
             off = sched tempo $ stop $ wholeOrPart e
             delta = off - on
             -- If there is already cps in the event, the union will preserve that.
             extra = Map.fromList [("cps", (VF (T.cps tempo))),
                                   ("delta", VF delta),
                                   ("cycle", VF (fromRational $ start $ wholeOrPart e)) 
                                 ]
             nudge = fromJust $ getF $ fromMaybe (VF 0) $ Map.lookup "nudge" $ playmap
             ts = on + nudge + latency
             tsPart = onPart + nudge + latency

toOSC latency _ e tempo (OSCContext oscpath)
  = map cToM $ contextPosition $ context e
  where cToM :: ((Int,Int),(Int,Int)) -> (Double, Bool, O.Message)
        cToM ((x, y), (x',y')) = (ts,
                                  False, -- bus message ?
                                  O.Message oscpath $ (O.string ident):(O.float delta):(O.float cyc):(map O.int32 [x,y,x',y'])
                                 )
        on = sched tempo $ start $ wholeOrPart e
        off = sched tempo $ stop $ wholeOrPart e
        delta = off - on
        cyc :: Double
        cyc = fromRational $ start $ wholeOrPart e
        nudge = fromMaybe 0 $ Map.lookup "nudge" (value e) >>= getF
        ident = fromMaybe "unknown" $ Map.lookup "_id_" (value e) >>= getS
        ts = on + nudge + latency

doCps :: MVar T.Tempo -> (Double, Maybe Value) -> IO ()
doCps tempoMV (d, Just (VF cps)) =
  do _ <- forkIO $ do threadDelay $ floor $ d * 1000000
                      -- hack to stop things from stopping !
                      -- TODO is this still needed?
                      _ <- T.setCps tempoMV (max 0.00001 cps)
                      return ()
     return ()
doCps _ _ = return ()

onTick :: Stream -> T.State -> IO ()
onTick stream st
  = do doTick False stream st

processCps :: T.Tempo -> [Event ValueMap] -> ([(T.Tempo, Event ValueMap)], T.Tempo)
processCps t [] = ([], t)
-- If an event has a tempo change, that affects the following events..
processCps t (e:evs) = (((t', e):es'), t'')
  where cps' | eventHasOnset e = do x <- Map.lookup "cps" $ value e
                                    getF x
             | otherwise = Nothing
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
                                                 T.starting = True, -- really?
                                                 T.nowArc = (Arc 0 1)
                                                }
                            doTick True (stream {sPMapMV = pMapMV}) state

-- | Query the current pattern (contained in argument @stream :: Stream@)
-- for the events in the current arc (contained in argument @st :: T.State@),
-- translate them to OSC messages, and send these.
--
-- If an exception occurs during sending,
-- this functions prints a warning and continues, because
-- the likely reason is that the backend (supercollider) isn't running.
-- 
-- If any exception occurs before or outside sending
-- (e.g., while querying the pattern, while computing a message),
-- this function prints a warning and resets the current pattern
-- to the previous one (or to silence if there isn't one) and continues,
-- because the likely reason is that something is wrong with the current pattern.
doTick :: Bool -> Stream -> T.State -> IO ()
doTick fake stream st =
  E.handle (\ (e :: E.SomeException) -> do
    hPutStrLn stderr $ "Failed to Stream.doTick: " ++ show e
    hPutStrLn stderr $ "Return to previous pattern."
    setPreviousPatternOrSilence stream
           ) $
  modifyState $ \(tempo, sMap) -> do
     pMap <- readMVar (sPMapMV stream)
     busses <- readMVar (sBusses stream)
     sGlobalF <- readMVar (sGlobalFMV stream)
     -- putStrLn $ show st
     let config = sConfig stream
         cxs = sCxs stream
         cycleNow = T.timeToCycles tempo $ T.start st
         patstack = sGlobalF $ playStack pMap
         -- If a 'fake' tick, it'll be aligned with cycle zero
         pat | fake = withResultTime (+ cycleNow) patstack
             | otherwise = patstack
         frameEnd = snd $ T.nowTimespan st
         -- add cps to state
         sMap' = Map.insert "_cps" (VF (T.cps tempo)) sMap
         --filterOns = filter eventHasOnset
         extraLatency | fake = 0
                      | otherwise = cFrameTimespan config + T.nudged tempo
         -- First the state is used to query the pattern
         es = sortOn (start . part) $ query pat (State {arc = T.nowArc st,
                                                        controls = sMap'
                                                       }
                                                )
         -- Then it's passed through the events
         (sMap'', es') = resolveState sMap' es
         
         -- TODO onset is calculated in toOSC as well..
         on e tempo'' = (sched tempo'' $ start $ wholeOrPart e)
         (tes, tempo') = processCps tempo $ es'
     forM_ cxs $ \cx@(Cx target _ oscs _ _) -> do
         let latency = oLatency target + extraLatency
             ms = concatMap (\(t, e) ->
                              if (fake || (on e t) < frameEnd)
                              then concatMap (toOSC latency busses e t) oscs
                              else []
                          ) tes
         forM_ ms $ \ m -> send (sListen stream) cx m `E.catch` \ (e :: E.SomeException) -> do
           hPutStrLn stderr $ "Failed to send. Is the '" ++ oName target ++ "' target running? " ++ show e

     when (tempo /= tempo') $ T.sendTempo tempo'

     (tempo', sMap'') `seq` return (tempo', sMap'')
  where modifyState :: ((T.Tempo, ValueMap) -> IO (T.Tempo, ValueMap)) -> IO ()
        modifyState io = E.mask $ \restore -> do
          s <- takeMVar (sStateMV stream)
          t <- takeMVar (sTempoMV stream)
          (t', s') <- restore (io (t, s)) `E.onException` (do {putMVar (sStateMV stream) s; putMVar (sTempoMV stream) t; return ()})
          putMVar (sStateMV stream) s'
          putMVar (sTempoMV stream) t'

setPreviousPatternOrSilence :: Stream -> IO ()
setPreviousPatternOrSilence stream =
  modifyMVar_ (sPMapMV stream) $ return
    . Map.map ( \ pMap -> case history pMap of
      _:p:ps -> pMap { pattern = p, history = p:ps }
      _ -> pMap { pattern = silence, history = [silence] }
              )
      
send :: Maybe O.UDP -> Cx -> (Double, Bool, O.Message) -> IO ()
send listen cx (time, isBusMsg, m)
  | oSchedule target == Pre BundleStamp = sendBndl isBusMsg listen cx $ O.Bundle time [m]
  | oSchedule target == Pre MessageStamp = sendO isBusMsg listen cx $ addtime m
  | otherwise = do _ <- forkIO $ do now <- O.time
                                    threadDelay $ floor $ (time - now) * 1000000
                                    sendO isBusMsg listen cx m
                   return ()
    where addtime (O.Message mpath params) = O.Message mpath ((O.int32 sec):((O.int32 usec):params))
          ut = O.ntpr_to_ut time
          sec :: Int
          sec = floor ut
          usec :: Int
          usec = floor $ 1000000 * (ut - (fromIntegral sec))
          target = cxTarget cx

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

streamReplace :: Stream -> ID -> ControlPattern -> IO ()
streamReplace s k !pat
  = E.catch (do let x = queryArc pat (Arc 0 0)
                tempo <- readMVar $ sTempoMV s
                input <- takeMVar $ sStateMV s
                -- put pattern id and change time in control input
                now <- O.time
                let cyc = T.timeToCycles tempo now
                putMVar (sStateMV s) $
                  Map.insert ("_t_all") (VR cyc) $ Map.insert ("_t_" ++ fromID k) (VR cyc) input
                -- update the pattern itself
                pMap <- seq x $ takeMVar $ sPMapMV s
                let playState = updatePS $ Map.lookup (fromID k) pMap
                putMVar (sPMapMV s) $ Map.insert (fromID k) playState pMap
                return ()
          )
    (\(e :: E.SomeException) -> hPutStrLn stderr $ "Error in pattern: " ++ show e
    )
  where updatePS (Just playState) = do playState {pattern = pat', history = pat:(history playState)}
        updatePS Nothing = PlayState pat' False False [pat']
        pat' = pat # pS "_id_" (pure $ fromID k)

streamMute :: Stream -> ID -> IO ()
streamMute s k = withPatIds s [k] (\x -> x {mute = True})

streamMutes :: Stream -> [ID] -> IO ()
streamMutes s ks = withPatIds s ks (\x -> x {mute = True})

streamUnmute :: Stream -> ID -> IO ()
streamUnmute s k = withPatIds s [k] (\x -> x {mute = False})

streamSolo :: Stream -> ID -> IO ()
streamSolo s k = withPatIds s [k] (\x -> x {solo = True})

streamUnsolo :: Stream -> ID -> IO ()
streamUnsolo s k = withPatIds s [k] (\x -> x {solo = False})

withPatIds :: Stream -> [ID] -> (PlayState -> PlayState) -> IO ()
withPatIds s ks f
  = do playMap <- takeMVar $ sPMapMV s
       let pMap' = foldr (Map.update (\x -> Just $ f x)) playMap (map fromID ks)
       putMVar (sPMapMV s) pMap'
       return ()

-- TODO - is there a race condition here?
streamMuteAll :: Stream -> IO ()
streamMuteAll s = modifyMVar_ (sPMapMV s) $ return . fmap (\x -> x {mute = True})

streamHush :: Stream -> IO ()
streamHush s = modifyMVar_ (sPMapMV s) $ return . fmap (\x -> x {pattern = silence, history = silence:history x})

streamUnmuteAll :: Stream -> IO ()
streamUnmuteAll s = modifyMVar_ (sPMapMV s) $ return . fmap (\x -> x {mute = False})

streamUnsoloAll :: Stream -> IO ()
streamUnsoloAll s = modifyMVar_ (sPMapMV s) $ return . fmap (\x -> x {solo = False})

streamSilence :: Stream -> ID -> IO ()
streamSilence s k = withPatIds s [k] (\x -> x {pattern = silence, history = silence:history x})

streamAll :: Stream -> (ControlPattern -> ControlPattern) -> IO ()
streamAll s f = do _ <- swapMVar (sGlobalFMV s) f
                   return ()

streamGet :: Stream -> String -> IO (Maybe Value)
streamGet s k = Map.lookup k <$> readMVar (sStateMV s)

streamSet :: Valuable a => Stream -> String -> Pattern a -> IO ()
streamSet s k pat = do sMap <- takeMVar $ sStateMV s
                       let pat' = toValue <$> pat
                           sMap' = Map.insert k (VPattern pat') sMap
                       putMVar (sStateMV s) $ sMap'

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

openListener :: Config -> IO (Maybe O.UDP)
openListener c
  | cCtrlListen c = catchAny run (\_ -> do verbose c "That port isn't available, perhaps another Tidal instance is already listening on that port?"
                                           return Nothing
                                 )
  | otherwise  = return Nothing
  where
        run = do sock <- O.udpServer (cCtrlAddr c) (cCtrlPort c)
                 N.setSocketOption (O.udpSocket sock) N.Broadcast 1
                 return $ Just sock
        catchAny :: IO a -> (E.SomeException -> IO a) -> IO a
        catchAny = E.catch

ctrlResponder :: Int -> Config -> Stream -> IO ()
ctrlResponder waits c (stream@(Stream {sListen = Just sock}))
  = do ms <- recvMessagesTimeout 2 sock
       if (null ms)
         then do checkHandshake -- there was a timeout, check handshake
                 ctrlResponder (waits+1) c stream
         else do mapM_ act ms
                 ctrlResponder 0 c stream
     where
        checkHandshake = do busses <- readMVar (sBusses stream)
                            when (null busses) $ do when  (waits == 0) $ verbose c $ "Waiting for SuperDirt (v.1.7.2 or higher).."
                                                    sendHandshakes stream

        act (O.Message "/dirt/hello" _) = sendHandshakes stream
        act (O.Message "/dirt/handshake/reply" xs) = do prev <- swapMVar (sBusses stream) $ bufferIndices xs
                                                        -- Only report the first time..
                                                        when (null prev) $ verbose c $ "Connected to SuperDirt."
                                                        return ()
          where 
            bufferIndices [] = []
            bufferIndices (x:xs') | x == (O.ASCII_String $ O.ascii "&controlBusIndices") = catMaybes $ takeWhile isJust $ map O.datum_integral xs'
                                  | otherwise = bufferIndices xs'
        -- External controller commands
        act (O.Message "/ctrl" (O.Int32 k:v:[]))
          = act (O.Message "/ctrl" [O.string $ show k,v])
        act (O.Message "/ctrl" (O.ASCII_String k:v@(O.Float _):[]))
          = add (O.ascii_to_string k) (VF (fromJust $ O.datum_floating v))
        act (O.Message "/ctrl" (O.ASCII_String k:O.ASCII_String v:[]))
          = add (O.ascii_to_string k) (VS (O.ascii_to_string v))
        act (O.Message "/ctrl" (O.ASCII_String k:O.Int32 v:[]))
          = add (O.ascii_to_string k) (VI (fromIntegral v))
        -- Stream playback commands
        act (O.Message "/mute" (k:[]))
          = withID k $ streamMute stream
        act (O.Message "/unmute" (k:[]))
          = withID k $ streamUnmute stream
        act (O.Message "/solo" (k:[]))
          = withID k $ streamSolo stream
        act (O.Message "/unsolo" (k:[]))
          = withID k $ streamUnsolo stream
        act (O.Message "/muteAll" [])
          = streamMuteAll stream
        act (O.Message "/unmuteAll" [])
          = streamUnmuteAll stream
        act (O.Message "/unsoloAll" [])
          = streamUnsoloAll stream
        act (O.Message "/hush" [])
          = streamHush stream
        act (O.Message "/silence" (k:[]))
          = withID k $ streamSilence stream
        act m = hPutStrLn stderr $ "Unhandled OSC: " ++ show m
        add :: String -> Value -> IO ()
        add k v = do sMap <- takeMVar (sStateMV stream)
                     putMVar (sStateMV stream) $ Map.insert k v sMap
                     return ()
        withID :: O.Datum -> (ID -> IO ()) -> IO ()
        withID (O.ASCII_String k) func = func $ (ID . O.ascii_to_string) k
        withID (O.Int32 k) func = func $ (ID . show) k
        withID _ _ = return ()
ctrlResponder _ _ _ = return ()


verbose :: Config -> String -> IO ()
verbose c s = when (cVerbose c) $ putStrLn s

recvMessagesTimeout :: (O.Transport t) => Double -> t -> IO [O.Message]
recvMessagesTimeout n sock = fmap (maybe [] O.packetMessages) $ O.recvPacketTimeout n sock


streamGetcps :: Stream -> IO O.Time
streamGetcps s = do tempo <- readMVar $ sTempoMV s
                    return $ T.cps tempo

streamGetnow :: Stream -> IO Double
streamGetnow s = do tempo <- readMVar $ sTempoMV s
                    now <- O.time
                    return $ fromRational $ T.timeToCycles tempo now


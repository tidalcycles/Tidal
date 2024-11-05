{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Sound.Tidal.Stream.Process where

{-
    Process.hs - Tidal's thingie for turning patterns into OSC streams
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

import           Control.Applicative       ((<|>))
import           Control.Concurrent.MVar
import qualified Control.Exception         as E
import           Control.Monad             (forM_, when)
import           Data.Coerce               (coerce)
import qualified Data.Map.Strict           as Map
import           Data.Maybe                (catMaybes, fromJust, fromMaybe)
import           System.IO                 (hPutStrLn, stderr)

import qualified Sound.Osc.Fd              as O

import           Data.List                 (sortOn)
import qualified Sound.Tidal.Clock         as Clock
import           Sound.Tidal.Core          (stack, (#))
import           Sound.Tidal.ID
import qualified Sound.Tidal.Link          as Link
import           Sound.Tidal.Params        (pS)
import           Sound.Tidal.Pattern
import           Sound.Tidal.Show          ()
import           Sound.Tidal.Utils         ((!!!))

import           Sound.Tidal.Stream.Target
import           Sound.Tidal.Stream.Types

data ProcessedEvent =
  ProcessedEvent {
    peHasOnset         :: Bool,
    peEvent            :: Event ValueMap,
    peCps              :: Double,
    peDelta            :: Link.Micros,
    peCycle            :: Time,
    peOnWholeOrPart    :: Link.Micros,
    peOnWholeOrPartOsc :: O.Time,
    peOnPart           :: Link.Micros,
    peOnPartOsc        :: O.Time
  }

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

doTick :: MVar ValueMap                           -- pattern state
       -> MVar [Int]                              -- busses
       -> MVar PlayMap                            -- currently playing
       -> MVar (ControlPattern -> ControlPattern) -- current global fx
       -> [Cx]                                    -- target addresses
       -> Maybe O.Udp                             -- network socket
       -> (Time,Time)                             -- current arc
       -> Double                                  -- nudge
       -> Clock.ClockConfig                       -- config of the clock
       -> Clock.ClockRef                          -- reference to the clock
       -> (Link.SessionState, Link.SessionState)  -- second session state is for keeping track of tempo changes
       -> IO ()
doTick stateMV busMV playMV globalFMV cxs listen (st,end) nudge cconf cref (ss, temposs) =
  E.handle handleException $ do
    modifyMVar_ stateMV $ \sMap -> do
      pMap <- readMVar playMV
      busses <- readMVar busMV
      sGlobalF <- readMVar globalFMV
      bpm <- Clock.getTempo ss
      let
        patstack = sGlobalF $ playStack pMap
        cps = ((Clock.beatToCycles cconf) $ fromRational bpm) / 60
        sMap' = Map.insert "_cps" (VF $ coerce cps) sMap
        extraLatency = nudge
        -- First the state is used to query the pattern
        es = sortOn (start . part) $ query patstack (State {arc = Arc st end,
                                                        controls = sMap'
                                                      }
                                                )
         -- Then it's passed through the events
        (sMap'', es') = resolveState sMap' es
      tes <- processCps cconf cref (ss, temposs) es'
      -- For each OSC target
      forM_ cxs $ \cx@(Cx target _ oscs _ _) -> do
              -- Latency is configurable per target.
              -- Latency is only used when sending events live.
              let latency = oLatency target
                  ms = concatMap (\e ->  concatMap (toOSC busses e) oscs) tes
              -- send the events to the OSC target
              forM_ ms $ \m -> (send listen cx latency extraLatency m) `E.catch` \(e :: E.SomeException) ->
                hPutStrLn stderr $ "Failed to send. Is the '" ++ oName target ++ "' target running? " ++ show e
      return sMap''
  where
    handleException :: E.SomeException -> IO ()
    handleException e = do
      hPutStrLn stderr $ "Failed to Stream.doTick: " ++ show e
      hPutStrLn stderr $ "Return to previous pattern."
      setPreviousPatternOrSilence playMV

processCps :: Clock.ClockConfig -> Clock.ClockRef -> (Link.SessionState, Link.SessionState) -> [Event ValueMap] -> IO [ProcessedEvent]
processCps cconf cref (ss, temposs) = mapM processEvent
  where
    processEvent ::  Event ValueMap  -> IO ProcessedEvent
    processEvent e = do
      let wope = wholeOrPart e
          partStartCycle = start $ part e
          partStartBeat = (Clock.cyclesToBeat cconf) (realToFrac partStartCycle)
          onCycle = start wope
          onBeat = (Clock.cyclesToBeat cconf) (realToFrac onCycle)
          offCycle = stop wope
          offBeat = (Clock.cyclesToBeat cconf) (realToFrac offCycle)
      on <- Clock.timeAtBeat cconf ss onBeat
      onPart <- Clock.timeAtBeat cconf ss partStartBeat
      when (eventHasOnset e) (do
        let cps' = Map.lookup "cps" (value e) >>= getF
        maybe (return ()) (\newCps -> Clock.setTempoCPS newCps on cconf temposs) (fmap toRational cps')
        )
      off <- Clock.timeAtBeat cconf ss offBeat
      bpm <- Clock.getTempo ss
      wholeOrPartOsc <- Clock.linkToOscTime cref on
      onPartOsc <- Clock.linkToOscTime cref onPart
      let cps = ((Clock.beatToCycles cconf) $ fromRational bpm) / 60
      let delta = off - on
      return $! ProcessedEvent {
          peHasOnset = eventHasOnset e,
          peEvent = e,
          peCps = cps,
          peDelta = delta,
          peCycle = onCycle,
          peOnWholeOrPart = on,
          peOnWholeOrPartOsc = wholeOrPartOsc,
          peOnPart = onPart,
          peOnPartOsc = onPartOsc
        }


toOSC :: [Int] -> ProcessedEvent -> OSC -> [(Double, Bool, O.Message)]
toOSC busses pe osc@(OSC _ _)
  = catMaybes (playmsg:busmsgs)
      -- playmap is a ValueMap where the keys don't start with ^ and are not ""
      -- busmap is a ValueMap containing the rest of the keys from the event value
      -- The partition is performed in order to have special handling of bus ids.
      where
        (playmap, busmap) = Map.partitionWithKey (\k _ -> null k || head k /= '^') $ val pe
        -- Map in bus ids where needed.
        --
        -- Bus ids are integers
        -- If busses is empty, the ids to send are directly contained in the the values of the busmap.
        -- Otherwise, the ids to send are contained in busses at the indices of the values of the busmap.
        -- Both cases require that the values of the busmap are only ever integers,
        -- that is, they are Values with constructor VI
        -- (but perhaps we should explicitly crash with an error message if it contains something else?).
        -- Map.mapKeys tail is used to remove ^ from the keys.
        -- In case (value e) has the key "", we will get a crash here.
        playmap' = Map.union (Map.mapKeys tail $ Map.map (\v -> VS ('c':(show $ toBus $ fromMaybe 0 $ getI v))) busmap) playmap
        val = value . peEvent
        -- Only events that start within the current nowArc are included
        playmsg | peHasOnset pe = do
                  -- If there is already cps in the event, the union will preserve that.
                  let extra = Map.fromList [("cps", (VF (peCps pe))),
                                          ("delta", VF (Clock.addMicrosToOsc (peDelta pe) 0)),
                                          ("cycle", VF (fromRational (peCycle pe)))
                                        ]
                      addExtra = Map.union playmap' extra
                      ts = (peOnWholeOrPartOsc pe) + nudge -- + latency
                  vs <- toData osc ((peEvent pe) {value = addExtra})
                  mungedPath <- substitutePath (path osc) playmap'
                  return (ts,
                          False, -- bus message ?
                          O.Message mungedPath vs
                          )
                | otherwise = Nothing
        toBus n | null busses = n
                | otherwise = busses !!! n
        busmsgs = map
                    (\(k, b) -> do k' <- if (not $ null k) && head k == '^' then Just (tail k) else Nothing
                                   v <- Map.lookup k' playmap
                                   bi <- getI b
                                   return $ (tsPart,
                                             True, -- bus message ?
                                             O.Message "/c_set" [O.int32 bi, toDatum v]
                                            )
                    )
                    (Map.toList busmap)
          where
            tsPart = (peOnPartOsc pe) + nudge -- + latency
        nudge = fromJust $ getF $ fromMaybe (VF 0) $ Map.lookup "nudge" $ playmap
toOSC _ pe (OSCContext oscpath)
  = map cToM $ contextPosition $ context $ peEvent pe
  where cToM :: ((Int,Int),(Int,Int)) -> (Double, Bool, O.Message)
        cToM ((x, y), (x',y')) = (ts,
                                  False, -- bus message ?
                                  O.Message oscpath $ (O.string ident):(O.float (peDelta pe)):(O.float cyc):(map O.int32 [x,y,x',y'])
                                 )
        cyc :: Double
        cyc = fromRational $ peCycle pe
        nudge = fromMaybe 0 $ Map.lookup "nudge" (value $ peEvent pe) >>= getF
        ident = fromMaybe "unknown" $ Map.lookup "_id_" (value $ peEvent pe) >>= getS
        ts = (peOnWholeOrPartOsc pe) + nudge -- + latency

toData :: OSC -> Event ValueMap -> Maybe [O.Datum]
toData (OSC {args = ArgList as}) e = fmap (fmap (toDatum)) $ sequence $ map (\(n,v) -> Map.lookup n (value e) <|> v) as
toData (OSC {args = Named rqrd}) e
  | hasRequired rqrd = Just $ concatMap (\(n,v) -> [O.string n, toDatum v]) $ Map.toList $ value e
  | otherwise = Nothing
  where hasRequired [] = True
        hasRequired xs = null $ filter (not . (`elem` ks)) xs
        ks = Map.keys (value e)
toData _ _ = Nothing

toDatum :: Value -> O.Datum
toDatum (VF x)     = O.float x
toDatum (VN x)     = O.float x
toDatum (VI x)     = O.int32 x
toDatum (VS x)     = O.string x
toDatum (VR x)     = O.float $ ((fromRational x) :: Double)
toDatum (VB True)  = O.int32 (1 :: Int)
toDatum (VB False) = O.int32 (0 :: Int)
toDatum (VX xs)    = O.Blob $ O.blob_pack xs
toDatum _          = error "toDatum: unhandled value"

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
                            simpleShow (VS str)     = str
                            simpleShow (VI i)       = show i
                            simpleShow (VF f)       = show f
                            simpleShow (VN n)       = show n
                            simpleShow (VR r)       = show r
                            simpleShow (VB b)       = show b
                            simpleShow (VX xs)      = show xs
                            simpleShow (VState _)   = show "<stateful>"
                            simpleShow (VPattern _) = show "<pattern>"
                            simpleShow (VList _)    = show "<list>"
                            defaultValue :: String -> Maybe String
                            defaultValue ('=':dfltVal) = Just dfltVal
                            defaultValue _             = Nothing

playStack :: PlayMap -> ControlPattern
playStack pMap = stack . (map psPattern) . (filter active) . Map.elems $ pMap
  where active pState = if hasSolo pMap
                        then psSolo pState
                        else not (psMute pState)

hasSolo :: Map.Map k PlayState -> Bool
hasSolo = (>= 1) . length . filter psSolo . Map.elems

onSingleTick :: Clock.ClockConfig -> Clock.ClockRef -> MVar ValueMap -> MVar [Int] -> MVar PlayMap -> MVar (ControlPattern -> ControlPattern) -> [Cx] -> Maybe O.Udp -> ControlPattern -> IO ()
onSingleTick clockConfig clockRef stateMV busMV _ globalFMV cxs listen pat = do
  pMapMV <- newMVar $ Map.singleton "fake"
          (PlayState {psPattern = pat,
                      psMute = False,
                      psSolo = False,
                      psHistory = []
                      }
          )
  Clock.clockOnce (doTick stateMV busMV pMapMV globalFMV cxs listen) clockConfig clockRef


-- Used for Tempo callback
updatePattern :: Stream -> ID -> Time -> ControlPattern -> IO ()
updatePattern stream k !t pat = do
  let x = queryArc pat (Arc 0 0)
  pMap <- seq x $ takeMVar (sPMapMV stream)
  let playState = updatePS $ Map.lookup (fromID k) pMap
  putMVar (sPMapMV stream) $ Map.insert (fromID k) playState pMap
  where updatePS (Just playState) = do playState {psPattern = pat', psHistory = pat:(psHistory playState)}
        updatePS Nothing = PlayState pat' False False [pat']
        patControls = Map.singleton patternTimeID (VR t)
        pat' = withQueryControls (Map.union patControls)
                 $ pat # pS "_id_" (pure $ fromID k)

setPreviousPatternOrSilence :: MVar PlayMap -> IO ()
setPreviousPatternOrSilence playMV =
 modifyMVar_ playMV $ return
   . Map.map ( \ pMap -> case psHistory pMap of
     _:p:ps -> pMap { psPattern = p, psHistory = p:ps }
     _      -> pMap { psPattern = silence, psHistory = [silence] }
             )

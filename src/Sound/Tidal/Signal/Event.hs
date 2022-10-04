{-# LANGUAGE DeriveFunctor #-}

module Sound.Tidal.Signal.Event
  (module Sound.Tidal.Time,
   module Sound.Tidal.Arc,
   module Sound.Tidal.Signal.Event
  )
where

import Data.Maybe (fromJust, isJust)
import Data.Monoid

import Sound.Tidal.Arc
import Sound.Tidal.Time
import Sound.Tidal.Types

import qualified Data.Map.Strict as Map
import qualified  Data.List as List

-- ************************************************************ --
-- Event

-- | Event metadata, currently just a list of source code position
-- ranges that an event is tagged with (see Types)

isAnalog :: Event a -> Bool
isAnalog (Event {whole = Nothing}) = True
isAnalog _ = False

isDigital :: Event a -> Bool
isDigital = not . isAnalog

-- | Returns true only if an event starts within given timearc
onsetIn :: Arc -> Event a -> Bool
onsetIn a e = isIn a (wholeBegin e)

wholeOrActive :: Event a -> Arc
wholeOrActive (Event {whole = Just a}) = a
wholeOrActive e = active e

-- | Get the onset of an event's 'whole'
wholeBegin :: Event a -> Time
wholeBegin = aBegin . wholeOrActive

-- | Get the offset of an event's 'whole'
wholeEnd :: Event a -> Time
wholeEnd = aEnd . wholeOrActive

-- | Get the onset of an event's 'whole'
eventActiveBegin :: Event a -> Time
eventActiveBegin = aBegin . active

-- | Get the offset of an event's 'active'
eventActiveEnd :: Event a -> Time
eventActiveEnd = aEnd . active

-- | Get the timearc of an event's 'active'
eventActive :: Event a -> Arc
eventActive = active

eventValue :: Event a -> a
eventValue = value

eventHasOnset :: Event a -> Bool
eventHasOnset e | isAnalog e = False
                | otherwise = aBegin (fromJust $ whole e) == aBegin (active e)

withArc :: (Arc -> Arc) -> Event a -> Event a
withArc f e = e {active = f $ active e,
                 whole  = f <$> whole e
                }

 -- Resolves higher order VState values to plain values, by passing through (and changing) state
resolveState :: ValueMap -> [Event ValueMap] -> (ValueMap, [Event ValueMap])
resolveState sMap [] = (sMap, [])
resolveState sMap (e:es) = (sMap'', (e {value = v'}):es')
  where f sm (VState v) = v sm
        f sm v = (sm, v)
        (sMap', v') | eventHasOnset e = Map.mapAccum f sMap (value e)    -- pass state through VState functions
                    | otherwise = (sMap, Map.filter notVState $ value e) -- filter out VState values without onsets
        (sMap'', es') = resolveState sMap' es
        notVState (VState _) = False
        notVState _ = True

-- | Returns 'True' if the two given events are adjacent parts of the same whole
isAdjacent :: Eq a => Event a -> Event a -> Bool
isAdjacent e e' = (whole e == whole e')
                  && (value e == value e')
                  && ((aEnd (active e) == aBegin (active e'))
                      ||
                      (aEnd (active e') == aBegin (active e))
                     )

-- | Returns a list of events, with any adjacent parts of the same whole combined
defragParts :: Eq a => [Event a] -> [Event a]
defragParts [] = []
defragParts [e] = [e]
defragParts (e:es) | isJust i = defraged : defragParts (List.delete e' es)
                   | otherwise = e : defragParts es
  where i = List.findIndex (isAdjacent e) es
        e' = es !! fromJust i
        defraged = Event (metadata e) (whole e) u (value e)
        u = hull (active e) (active e')

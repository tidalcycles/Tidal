module Sound.Tidal.Event where

import qualified Data.List         as List
import qualified Data.Map.Strict   as Map
import           Data.Maybe        (fromJust, isJust)
import           Sound.Tidal.Span  (hull, isIn)
import           Sound.Tidal.Types

isAnalog :: Event a -> Bool
isAnalog Event {whole = Nothing} = True
isAnalog _                       = False

isDigital :: Event a -> Bool
isDigital = not . isAnalog

-- | Returns true only if an event starts within given timespan
onsetIn :: Span -> Event a -> Bool
onsetIn a e = isIn a (wholeBegin e)

eventWithSpan :: (Span -> Span) -> Event a -> Event a
eventWithSpan f e = e {active = f $ active e,
                       whole  = f <$> whole e
                      }

wholeOrActive :: Event a -> Span
wholeOrActive Event {whole = Just a} = a
wholeOrActive e                      = active e


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

-- | Get the timespan of an event's 'active'
eventActive :: Event a -> Span
eventActive = active

eventValue :: Event a -> a
eventValue = value

eventHasOnset :: Event a -> Bool
eventHasOnset e | isAnalog e = False
                | otherwise = aBegin (fromJust $ whole e) == aBegin (active e)

 -- Resolves higher order VState values to plain values, by passing through (and changing) state
resolveState :: ValueMap -> [Event ValueMap] -> (ValueMap, [Event ValueMap])
resolveState sMap [] = (sMap, [])
resolveState sMap (e:es) = (sMap'', (e {value = v'}):es')
  where f sm (VState v) = v sm
        f sm v          = (sm, v)
        (sMap', v') | eventHasOnset e = Map.mapAccum f sMap (value e)    -- pass state through VState functions
                    | otherwise = (sMap, Map.filter notVState $ value e) -- filter out VState values without onsets
        (sMap'', es') = resolveState sMap' es
        notVState (VState _) = False
        notVState _          = True

-- | Returns 'True' if the two given events are adjacent parts of the same whole
isAdjacent :: Eq a => Event a -> Event a -> Bool
isAdjacent e e' = (whole e == whole e')
                  && (value e == value e')
                  && ((aEnd (active e) == aBegin (active e'))
                      ||
                      (aEnd (active e') == aBegin (active e))
                     )

-- | Returns a list of events, with any adjacent parts of the same whole combined
defragActives :: Eq a => [Event a] -> [Event a]
defragActives [] = []
defragActives [e] = [e]
defragActives (e:es) | isJust i = defraged : defragActives (List.delete e' es)
                     | otherwise = e : defragActives es
  where i = List.findIndex (isAdjacent e) es
        e' = es !! fromJust i
        defraged = Event (eventMetadata e) (whole e) u (value e)
        u = hull (active e) (active e')

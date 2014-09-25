module Sound.Tidal.Time where

import Sound.Tidal.Utils
import Data.Ratio

-- | Time is represented by a rational number. Each natural number
-- represents both the start of the next rhythmic cycle, and the end
-- of the previous one. Rational numbers are used so that subdivisions
-- of each cycle can be accurately represented.
type Time = Rational

-- | @(s,e) :: Arc@ represents a time interval with a start and end value.
-- @ { t : s <= t && t < e } @
type Arc = (Time, Time)

-- | An Event is a value that occurs during the period given by the
-- first @Arc@. The second one indicates the event's "domain of
-- influence". These will often be the same, but many temporal
-- transformations, such as rotation and scaling time, may result in
-- arcs being split or truncated. In such cases, the first arc is
-- preserved, but the second arc reflects the portion of the event
-- which is relevant.
type Event a = (Arc, Arc, a)

-- | The starting point of the current cycle. A cycle occurs from each
-- natural number to the next, so this is equivalent to @floor@.
sam :: Time -> Time
sam = fromIntegral . floor

-- | The end point of the current cycle (and starting point of the next cycle)
nextSam :: Time -> Time
nextSam = (1+) . sam


-- | The position of a time value relative to the start of its cycle.
cyclePos :: Time -> Time
cyclePos t = t - sam t

-- | @isIn a t@ is @True@ iff @t@ is inside 
-- the arc represented by @a@.
isIn :: Arc -> Time -> Bool
isIn (s,e) t = t >= s && t < e

-- | Splits the given @Arc@ into a list of @Arc@s, at cycle boundaries.
arcCycles :: Arc -> [Arc]
arcCycles (s,e) | s >= e = []
                | sam s == sam e = [(s,e)]
                | otherwise = (s, nextSam s) : (arcCycles (nextSam s, e))

-- | Splits the given @Arc@ into a list of @Arc@s, at cycle boundaries, but wrapping the arcs within the same cycle.
arcCycles' :: Arc -> [Arc]
arcCycles' (s,e) | s >= e = []
                 | sam s == sam e = [(s,e)]
                 | otherwise = (s, nextSam s) : (arcCycles' ((nextSam s) - 1, e - 1))


-- | @subArc i j@ is the arc that is the intersection of @i@ and @j@.
subArc :: Arc -> Arc -> Maybe Arc
subArc (s, e) (s',e') | s'' < e'' = Just (s'', e'')
                      | otherwise = Nothing
  where s'' = max s s'
        e'' = min e e'

-- | Map the given function over both the start and end @Time@ values
-- of the given @Arc@.
mapArc :: (Time -> Time) -> Arc -> Arc
mapArc f (s,e) = (f s, f e)

-- | Similar to @mapArc@ but time is relative to the cycle (i.e. the
-- sam of the start of the arc)
mapCycle :: (Time -> Time) -> Arc -> Arc
mapCycle f (s,e) = (sam' + (f $ s - sam'), sam' + (f $ e - sam'))
         where sam' = sam s

-- | Returns the `mirror image' of an @Arc@, used by @Sound.Tidal.Pattern.rev@.
mirrorArc :: Arc -> Arc
mirrorArc (s, e) = (sam s + (nextSam s - e), nextSam s - (s - sam s))

-- | The start time of the given @Event@
eventStart :: Event a -> Time
eventStart = fst . snd'

-- | The original onset of the given @Event@
eventOnset :: Event a -> Time
eventOnset = fst . fst'

-- | The original offset of the given @Event@
eventOffset :: Event a -> Time
eventOffset = snd . fst'

-- | The arc of the given @Event@
eventArc :: Event a -> Arc
eventArc = snd'

-- | The midpoint of an @Arc@
midPoint :: Arc -> Time
midPoint (s,e) = s + ((e - s) / 2)

hasOnset :: Event a -> Bool
hasOnset ((s,_), (s',_), _) = s == s'

hasOffset :: Event a -> Bool
hasOffset ((_,e), (_,e'), _) = e == e'

onsetIn :: Arc -> Event a -> Bool
onsetIn a e = isIn a (eventOnset e)

offsetIn :: Arc -> Event a -> Bool
offsetIn a e = isIn a (eventOffset e)

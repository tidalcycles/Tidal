module Sound.Tidal.TimeSpan where

import           Sound.Tidal.Time
import           Sound.Tidal.Types

-- | Intersection of two arcs
sect :: Span -> Span -> Span
sect (Span b e) (Span b' e') = Span (max b b') (min e e')

-- | Returns the whole cycle arc that the given time is in
timeToCycle :: Time -> Span
timeToCycle t = Span (sam t) (nextSam t)

-- | Splits a timespan at cycle boundaries
splitSpans :: Span -> [Span]
splitSpans (Span s e) | s == e = [Span s e] -- support zero-width arcs
                      | otherwise = splitSpans' (Span s e) -- otherwise, recurse
  where splitSpans' (Span s' e') | e' <= s' = []
                                 | sam s' == sam e' = [Span s' e']
                                 | otherwise = Span s' (nextSam s') : splitSpans' (Span (nextSam s') e')

-- | Similar to 'fmap' but time is relative to the cycle (i.e. the
-- sam of the start of the arc)
mapCycle :: (Time -> Time) -> Span -> Span
mapCycle f (Span s e) = Span (sam' + f (s - sam')) (sam' + f (e - sam'))
         where sam' = sam s

withSpanTime :: (Time -> Time) -> Span -> Span
withSpanTime timef (Span b e) = Span (timef b) (timef e)

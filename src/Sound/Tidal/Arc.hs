module Sound.Tidal.Arc
  (module Sound.Tidal.Arc,
   module Sound.Tidal.Time
  )
where

-- (c) Alex McLean 2022
-- Shared under the terms of the GNU Public License v. 3.0

import Sound.Tidal.Time
import Sound.Tidal.Types

-- | Returns the whole cycle arc that the given arc is in
timeToCycleArc :: Time -> Arc
timeToCycleArc t = Arc (sam t) (nextSam t)

-- | Similar to 'fmap' but time is relative to the cycle (i.e. the
-- sam of the start of the arc)
mapCycle :: (Time -> Time) -> Arc -> Arc
mapCycle f (Arc s e) = Arc (sam' + f (s - sam')) (sam' + f (e - sam'))
         where sam' = sam s

-- | @isIn a t@ is @True@ if @t@ is inside
-- the arc represented by @a@.
isIn :: Arc -> Time -> Bool
isIn (Arc s e) t = t >= s && t < e

-- | Intersection of two timearcs
sect :: Arc -> Arc -> Arc
sect (Arc b e) (Arc b' e') = Arc (max b b') (min e e')

-- | Intersection of two arcs, returns Nothing if they don't intersect
-- The definition is a bit fiddly as results might be zero-width, but
-- not at the end of an non-zero-width arc - e.g. (0,1) and (1,2) do
-- not intersect, but (1,1) (1,1) does.
maybeSect :: Arc -> Arc -> Maybe Arc
maybeSect a@(Arc s e) b@(Arc s' e')
  | and [s'' == e'', s'' == e, s < e] = Nothing
  | and [s'' == e'', s'' == e', s' < e'] = Nothing
  | s'' <= e'' = Just (Arc s'' e'')
  | otherwise = Nothing
  where (Arc s'' e'') = sect a b

-- | convex hull union
hull :: Arc -> Arc -> Arc
hull (Arc s e) (Arc s' e') = Arc (min s s') (max e e')

-- | Splits a timespan at cycle boundaries
splitArcs :: Arc -> [Arc]
splitArcs (Arc s e) | s == e = [Arc s e] -- support zero-width arcs
                    | otherwise = splitArcs' (Arc s e) -- otherwise, recurse
  where splitArcs' (Arc s' e') | e' <= s' = []
                               | sam s' == sam e' = [Arc s' e']
                               | otherwise
          = (Arc s' (nextSam s')):(splitArcs' (Arc (nextSam s') e'))


-- | Shifts a timearc to one of equal duration that starts within cycle zero.
-- (Note that the output timearc probably does not start *at* Time 0 --
-- that only happens when the input Arc starts at an integral Time.)
cycleArc :: Arc -> Arc
cycleArc (Arc b e) = Arc b' e'
  where b' = cyclePos b
        e' = b' + (e - b)

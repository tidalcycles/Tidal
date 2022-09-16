module Sound.Tidal.Arc where

-- (c) Alex McLean 2022
-- Shared under the terms of the GNU Public License v. 3.0

type Time = Rational


-- | The start of the cycle that a given time value is in
sam :: Time -> Time
sam s = toRational $ floor s

-- | The start of the next cycle
nextSam :: Time -> Time
nextSam s = sam s + 1

-- | The position of a time value relative to the start of its cycle.
cyclePos :: Time -> Time
cyclePos t = t - sam t


-- | Time arc (also known as timespan)
data Arc = Arc {begin :: Time, end :: Time}
  deriving (Show)

-- | @isIn a t@ is @True@ if @t@ is inside
-- the arc represented by @a@.
isIn :: Arc -> Time -> Bool
isIn (Arc s e) t = t >= s && t < e

-- | Intersection of two timearcs
sect :: Arc -> Arc -> Arc
sect (Arc b e) (Arc b' e') = Arc (max b b') (min e e')

-- | Intersection of two timearcs, returns Nothing if they don't intersect
maybeSect :: Arc -> Arc -> Maybe Arc
maybeSect a b = check $ sect a b
  where check :: Arc -> Maybe Arc
        check (Arc a b) | b <= a = Nothing
                        | otherwise = Just (Arc a b)

-- | Splits a timearc at cycle boundaries
splitArcs :: Arc -> [Arc]
splitArcs (Arc b e) | e <= b = []
                      | sam b == sam e = [Arc b e]
                      | otherwise
  = (Arc b (nextSam b)):(splitArcs (Arc (nextSam b) e))

-- | Shifts a timearc to one of equal duration that starts within cycle zero.
-- (Note that the output timearc probably does not start *at* Time 0 --
-- that only happens when the input Arc starts at an integral Time.)
cycleArc :: Arc -> Arc
cycleArc (Arc b e) = Arc b' e'
  where b' = cyclePos b
        e' = b' + (e - b)

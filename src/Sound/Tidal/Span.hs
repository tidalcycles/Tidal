module Sound.Tidal.Span where

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


-- | Timespan (called an arc in tidal v1)
data Span = Span {begin :: Time, end :: Time}
  deriving (Show)

-- | @isIn a t@ is @True@ if @t@ is inside
-- the span represented by @a@.
isIn :: Span -> Time -> Bool
isIn (Span s e) t = t >= s && t < e

-- | Intersection of two timespans
sect :: Span -> Span -> Span
sect (Span b e) (Span b' e') = Span (max b b') (min e e')

-- | Intersection of two timespans, returns Nothing if they don't intersect
maybeSect :: Span -> Span -> Maybe Span
maybeSect a b = check $ sect a b
  where check :: Span -> Maybe Span
        check (Span a b) | b <= a = Nothing
                         | otherwise = Just (Span a b)

-- | Splits a timespan at cycle boundaries
splitSpans :: Span -> [Span]
splitSpans (Span b e) | e <= b = []
                      | sam b == sam e = [Span b e]
                      | otherwise
  = (Span b (nextSam b)):(splitSpans (Span (nextSam b) e))

-- | Shifts a timespan to one of equal duration that starts within cycle zero.
-- (Note that the output timespan probably does not start *at* Time 0 --
-- that only happens when the input Arc starts at an integral Time.)
cycleSpan :: Span -> Span
cycleSpan (Span b e) = Span b' e'
  where b' = cyclePos b
        e' = b' + (e - b)

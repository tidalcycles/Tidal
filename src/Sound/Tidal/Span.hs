module Sound.Tidal.Span where

-- (c) Alex McLean 2022
-- Shared under the terms of the GNU Public License v. 3.0

type Time = Rational

-- | Timespan (called an arc in tidal v1)
data Span = Span {start :: Time, stop :: Time}
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

-- | The start of the cycle that a given time value is in
sam :: Time -> Time
sam s = toRational $ floor s

-- | The start of the next cycle
nextSam :: Time -> Time
nextSam s = sam s + 1

-- | Splits a timespan at cycle boundaries
splitSpans :: Span -> [Span]
splitSpans (Span b e) | e <= b = []
                      | sam b == sam e = [Span b e]
                      | otherwise
  = (Span b (nextSam b)):(splitSpans (Span (nextSam b) e))


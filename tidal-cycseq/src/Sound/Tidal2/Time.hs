module Sound.Tidal2.Time where

-- (c) Alex McLean 2022
-- Shared under the terms of the GNU Public License v. 3.0

-- | Timespan (called an arc in tidal v1)
data Span = Span {begin :: Rational, end :: Rational}
  deriving (Show)

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
sam :: Rational -> Rational
sam s = toRational $ floor s

-- | The start of the next cycle
nextSam :: Rational -> Rational
nextSam s = sam s + 1

-- | Splits a timespan at cycle boundaries
splitSpans :: Span -> [Span]
splitSpans (Span b e) | e <= b = []
                      | sam b == sam e = [Span b e]
                      | otherwise
  = (Span b (nextSam b)):(splitSpans (Span (nextSam b) e))

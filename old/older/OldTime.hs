{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}

module Sound.Tidal.Time where

import Control.Applicative
import GHC.Generics
import Control.DeepSeq (NFData)

-- | Time is rational
type Time = Rational

-- | An arc of time, with a start time (or onset) and a stop time (or offset)
data ArcF a = Arc
  { start :: a
  , stop :: a
  } deriving (Eq, Ord, Functor, Show, Generic)

type Arc = ArcF Time

instance Applicative ArcF where
  pure t = Arc t t
  (<*>) (Arc sf ef) (Arc sx ex) = Arc (sf sx) (ef ex)

instance NFData a => NFData (ArcF a)

instance Num a => Num (ArcF a) where
  negate      = fmap negate
  (+)         = liftA2 (+)
  (*)         = liftA2 (*)
  fromInteger = pure . fromInteger
  abs         = fmap abs
  signum      = fmap signum

instance (Fractional a) => Fractional (ArcF a) where
  recip        = fmap recip
  fromRational = pure . fromRational

-- Utility functions - Time

-- | The @sam@ (start of cycle) for the given time value.
-- Cycles have duration 1, so every integer Time value divides two cycles.
sam :: Time -> Time
sam = fromIntegral . (floor :: Time -> Int)

-- | Turns a number into a (rational) time value. An alias for @toRational@.
toTime :: Real a => a -> Rational
toTime = toRational

-- | Turns a (rational) time value into another number. An alias for @fromRational@.
fromTime :: Fractional a => Time -> a
fromTime = fromRational

-- | The end point of the current cycle (and starting point of the next cycle)
nextSam :: Time -> Time
nextSam = (1+) . sam

-- | The position of a time value relative to the start of its cycle.
cyclePos :: Time -> Time
cyclePos t = t - sam t

-- Utility functions - Arc

-- | convex hull union
hull :: Arc -> Arc -> Arc
hull (Arc s e) (Arc s' e') = Arc (min s s') (max e e')

-- | @subArc i j@ is the timespan that is the intersection of @i@ and @j@.
-- intersection
-- The definition is a bit fiddly as results might be zero-width, but
-- not at the end of an non-zero-width arc - e.g. (0,1) and (1,2) do
-- not intersect, but (1,1) (1,1) does.
subArc :: Arc -> Arc -> Maybe Arc
subArc a@(Arc s e) b@(Arc s' e')
  | and [s'' == e'', s'' == e, s < e] = Nothing
  | and [s'' == e'', s'' == e', s' < e'] = Nothing
  | s'' <= e'' = Just (Arc s'' e'')
  | otherwise = Nothing
  where (Arc s'' e'') = sect a b

subMaybeArc :: Maybe Arc -> Maybe Arc -> Maybe (Maybe Arc)
subMaybeArc (Just a) (Just b) = do sa <- subArc a b
                                   return $ Just sa
subMaybeArc _ _ = Just Nothing
-- subMaybeArc = liftA2 subArc -- this typechecks, but doesn't work the same way.. hmm

-- | Simple intersection of two arcs
sect :: Arc -> Arc -> Arc
sect (Arc s e) (Arc s' e') = Arc (max s s') (min e e')

-- | The Arc returned is the cycle that the Time falls within.
--
-- Edge case: If the Time is an integer,
-- the Arc claiming it is the one starting at that Time,
-- not the previous one ending at that Time.
timeToCycleArc :: Time -> Arc
timeToCycleArc t = Arc (sam t) (sam t + 1)

-- | Shifts an Arc to one of equal duration that starts within cycle zero.
-- (Note that the output Arc probably does not start *at* Time 0 --
-- that only happens when the input Arc starts at an integral Time.)
cycleArc :: Arc -> Arc
cycleArc (Arc s e) = Arc (cyclePos s) (cyclePos s + (e-s))

-- | Returns the numbers of the cycles that the input @Arc@ overlaps
-- (excluding the input @Arc@'s endpoint, unless it has duration 0 --
-- see "Edge cases" below).
-- (The "cycle number" of an @Arc@ is equal to its start value.
-- Thus, for instance, @cyclesInArc (Arc 0 1.5) == [0,1]@.)
--
-- Edge cases:
-- >  cyclesInArc $ Arc 0 1.0001 == [0,1]
-- >  cyclesInArc $ Arc 0 1      == [0] -- the endpoint is excluded
-- >  cyclesInArc $ Arc 1 1      == [1] -- unless the Arc has duration 0
--
-- PITFALL: Don't be fooled by the name. The output cycles
-- are not necessarily completely contained in the input @Arc@,
-- but they definitely overlap it,
-- and they include every cycle that overlaps it.
cyclesInArc :: Integral a => Arc -> [a]
cyclesInArc (Arc s e)
  | s > e = []
  | s == e = [floor s]
  | otherwise = [floor s .. ceiling e-1]

-- | This provides exactly the same information as @cyclesInArc@,
-- except that this represents its output as @Arc@s,
-- whereas @cyclesInArc@ represents the same information as integral indices.
-- (The @Arc@ from 0 to 1 corresponds to the index 0,
-- the one from 1 to 2 has index 1, etc.)
cycleArcsInArc :: Arc -> [Arc]
cycleArcsInArc = map (timeToCycleArc . (toTime :: Int -> Time)) . cyclesInArc

-- | Splits the given 'Arc' into a list of 'Arc's, at cycle boundaries.
arcCycles :: Arc -> [Arc]
arcCycles (Arc s e) | s >= e = []
                | sam s == sam e = [Arc s e]
                | otherwise = Arc s (nextSam s) : arcCycles (Arc (nextSam s) e)

-- | Like arcCycles, but returns zero-width arcs
arcCyclesZW :: Arc -> [Arc]
arcCyclesZW (Arc s e) | s == e = [Arc s e]
                  | otherwise = arcCycles (Arc s e)

-- | Similar to 'fmap' but time is relative to the cycle (i.e. the
-- sam of the start of the arc)
mapCycle :: (Time -> Time) -> Arc -> Arc
mapCycle f (Arc s e) = Arc (sam' + f (s - sam')) (sam' + f (e - sam'))
         where sam' = sam s

-- | @isIn a t@ is @True@ if @t@ is inside
-- the arc represented by @a@.
isIn :: Arc -> Time -> Bool
isIn (Arc s e) t = t >= s && t < e

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}

module Sound.Tidal.Time where

import Control.Applicative
import GHC.Generics
import Control.DeepSeq (NFData)
import Data.Maybe (isJust, fromJust, catMaybes, mapMaybe)
import Data.List (delete, findIndex, sort)

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

-- | The 'sam' (start of cycle) for the given time value
sam :: Time -> Time
sam = fromIntegral . (floor :: Time -> Int)

-- | Turns a number into a (rational) time value. An alias for 'toRational'.
toTime :: Real a => a -> Rational
toTime = toRational

-- | Turns a (rational) time value into another number. An alias for 'fromRational'.
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
subMaybeArc = liftA2 subArc

-- | Simple intersection of two arcs
sect :: Arc -> Arc -> Arc
sect (Arc s e) (Arc s' e') = Arc (max s s') (min e e')


-- | The arc of the whole cycle that the given time value falls within
timeToCycleArc :: Time -> Arc
timeToCycleArc t = Arc (sam t) (sam t + 1)

-- | Shifts an arc to the equivalent one that starts during cycle zero
cycleArc :: Arc -> Arc
cycleArc (Arc s e) = Arc (cyclePos s) (cyclePos s + (e-s))

-- | A list of cycle numbers which are included in the given arc
cyclesInArc :: Integral a => Arc -> [a]
cyclesInArc (Arc s e)
  | s > e = []
  | s == e = [floor s]
  | otherwise = [floor s .. ceiling e-1]

-- | A list of arcs of the whole cycles which are included in the given arc
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

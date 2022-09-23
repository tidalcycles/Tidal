module Sound.Tidal.Time where

import Sound.Tidal.Types
import Data.Ratio

-- | The start of the cycle that a given time value is in
sam :: Time -> Time
sam s = toRational $ floor s

-- | The start of the next cycle
nextSam :: Time -> Time
nextSam s = sam s + 1

-- | The position of a time value relative to the start of its cycle.
cyclePos :: Time -> Time
cyclePos t = t - sam t

-- | Lowest common multiple
lcmTime :: Time -> Time -> Time
lcmTime a b = lcm (f a) (f b) % d
  where d = lcm (denominator a) (denominator b)
        f x = numerator x * (d `div` denominator x)

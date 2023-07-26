module Sound.Tidal.Time where

import           Data.Ratio
import           Sound.Tidal.Types

-- | The start of the cycle
sam :: Time -> Time
sam s = toRational (floor s :: Int)

-- | The start of the next cycle
nextSam :: Time -> Time
nextSam = (+1) . sam

-- | Lowest common multiple
lcmTime :: Time -> Time -> Time
lcmTime a b = lcm (f a) (f b) % d
  where d = lcm (denominator a) (denominator b)
        f x = numerator x * (d `div` denominator x)

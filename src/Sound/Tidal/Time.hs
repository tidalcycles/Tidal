module Sound.Tidal.Time where

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

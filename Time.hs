module Time where

type Time = Rational
type Arc = (Time, Time)

cutArc :: (Time, Time) -> (Time, Time) -> (Time, Time)
cutArc (s, d) (s', d') = (s'', d'')
  where s'' = max s s'
        d'' = (min (s+d) (s'+d')) - s''

isIn :: Arc -> Time -> Bool
isIn (s,e) t = t >= s && t < e

sam :: Time -> Time
sam = fromIntegral . floor

nextSam :: Time -> Time
nextSam = (1+) . sam

cyclePos :: Time -> Time
cyclePos t = t - sam t

-- chop arc into arcs within unit cycles
arcCycles :: Arc -> [Arc]
arcCycles (s,e) | s >= e = []
                | sam s == sam e = [(s,e)]
                | otherwise = (s, nextSam s) : (arcCycles (nextSam s, e))

subArc :: Arc -> Arc -> Maybe Arc
subArc (s, e) (s',e') | s'' < e'' = Just (s'', e'')
                      | otherwise = Nothing
  where s'' = max s s'
        e'' = min e e'

mapArc :: (Time -> Time) -> Arc -> Arc
mapArc f (s,e) = (f s, f e)

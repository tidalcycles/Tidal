module Time where

type Time = Rational
type Arc = (Time, Time)
type Event a = (Arc, a)

sam :: Time -> Time
sam = fromIntegral . floor

nextSam :: Time -> Time
nextSam = (1+) . sam

cyclePos :: Time -> Time
cyclePos t = t - sam t

isIn :: Arc -> Time -> Bool
isIn (s,e) t = t >= s && t < e

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

mirrorArc :: Arc -> Arc
mirrorArc (s, e) = (sam s + (nextSam s - e), nextSam s - (s - sam s))

eventStart :: Event a -> Time
eventStart = fst . fst


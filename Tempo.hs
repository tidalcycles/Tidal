module Tempo where

import Data.Time
import Data.Time.Clock.POSIX

data Tempo = Tempo {at :: UTCTime, beat :: Double, bps :: Double}

instance Show Tempo where
  show x = show (at x) ++ "," ++ show (beat x) ++ "," ++ show (bps x)

readTempo :: String -> Tempo
readTempo x = Tempo (read a) (read b) (read c)
  where (a:b:c:_) = wordsBy (== ',') x

wordsBy :: (a -> Bool) -> [a] -> [[a]]
wordsBy p s = case dropWhile p s of
   []      -> []
   s':rest -> (s':w) : wordsBy p (drop 1 s'')
          where (w, s'') = break p rest

logicalTime :: Tempo -> Double -> Double
logicalTime t b = changeT + timeDelta
  where beatDelta = b - (beat t)
        timeDelta = beatDelta / (bps t)
        changeT = realToFrac $ utcTimeToPOSIXSeconds $ at t

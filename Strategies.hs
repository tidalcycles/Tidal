{-# OPTIONS_GHC -XNoMonomorphismRestriction #-}

module Strategies where

import Pattern
import Dirt
import Data.Ratio
import Control.Applicative

import Utils

echo n p = stack [p, n ~> p]
double f p = stack [p, f p]

-- every 4 (smash 4 [1, 2, 3]) $ sound "[odx sn/2 [~ odx] sn/3, [~ hh]*4]"

smash n xs p = cat $ map (\n -> slow n p') xs
  where p' = striate n p

brak = every 2 (((1%4) <~) . (\x -> cat [x, silence]))

-- samples "jvbass [~ latibro] [jvbass [latibro jvbass]]" ((1%2) <~ slow 6 "[1 6 8 7 3]")
samples p p' = pick <$> p <*> p'

spread f xs p = cat $ map (\x -> f x p) xs

spread' :: (a -> Pattern b -> Pattern c) -> Pattern a -> Pattern b -> Pattern c
spread' f timepat pat =
  Pattern $ \r -> concatMap (\(r', x) -> (arc (f x pat) r')) (rs r)
  where rs r = arc (filterOffsets timepat) r

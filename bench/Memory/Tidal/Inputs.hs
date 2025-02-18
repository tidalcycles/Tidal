{-# LANGUAGE OverloadedStrings #-}

module Tidal.Inputs where

import Sound.Tidal.Core
import Sound.Tidal.Params
import Sound.Tidal.Control
import Sound.Tidal.ParseBP ()
import Sound.Tidal.Pattern
import Sound.Tidal.UI
import Weigh

columns :: Weigh ()
columns = setColumns [Case, Allocated, Max, Live, GCs]

{- Pattern inputs -}
xs3 :: [Time]
xs3 = [1 .. 10000]

xs4 :: [Time]
xs4 = [1 .. 100000]

xs5 :: [Time]
xs5 = [1 .. 1000000]

xs6 :: [Time]
xs6 = [1 .. 10000000]

xsA :: [Time]
xsA = [500000 .. 1500000]

catPattSmall :: [Pattern Time]
catPattSmall = pure <$> xs3

catPattMed :: [Pattern Time]
catPattMed = pure <$> xs4

catPattMedB :: [Pattern Time]
catPattMedB = pure <$> xs5

catPattBig :: [Pattern Time]
catPattBig = pure <$> xs6

timeCatMed :: [(Time, Pattern Time)]
timeCatMed = zip xs5 catPattMed

timeCatBig :: [(Time, Pattern Time)]
timeCatBig = zip xs6 catPattBig

appendBig :: [Pattern Time]
appendBig = pure <$> xsA

pattApp1 :: Pattern [Time]
pattApp1 = sequence catPattBig

pattApp2 :: Pattern [Time]
pattApp2 = sequence appendBig

{- Arc Inputs -}
arcFunc :: Arc -> Arc
arcFunc (Arc st en) = Arc (st * 2) (en * 4)

wqaMed :: Pattern Time
wqaMed = fromList xs5

wqaBig :: Pattern Time
wqaBig = fromList xs6

{- fix inputs -}
fixArg1 :: ControlPattern
fixArg1 = pF "cc64" 1

fixArg2 :: ControlPattern
fixArg2 =
  fix (# crush 4) (pF "cc65" 1) $
    fix (echoWith 4 (0.125 / 4) (+ up "1")) (pF "cc66" 1) $
      fix (|*| speed "-1") (pF "cc67" 1) $
        fix ((# delaytime 0.125) . (# delay 0.5)) (pF "cc68" 1) $
          fix (# coarse 12) (pF "cc69" 1) $
            s "[808bd:1(3,8), dr(7,8)]"
              # pF "cc64" (cF 0 "64")
              # pF "cc65" (cF 0 "65")
              # pF "cc66" (cF 0 "66")
              # pF "cc67" (cF 0 "67")
              # pF "cc68" (cF 0 "68")
              # pF "cc69" (cF 0 "69")

{- Euclid inputs -}
ecA1 :: [Pattern Int]
ecA1 = [1, 1000000]

ecA2 :: Pattern String
ecA2 = "x"

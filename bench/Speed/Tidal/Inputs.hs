{-# LANGUAGE OverloadedStrings #-}

module Tidal.Inputs where 

import Sound.Tidal.Pattern
import Sound.Tidal.Core
import Sound.Tidal.ParseBP
import Sound.Tidal.Core
import Sound.Tidal.Params
import Sound.Tidal.Control
import Sound.Tidal.UI

{- Pattern inputs -} 
xs3 = [1..10^3]
xs4 = [1..10^4]
xs5 = [1..10^5]
xs6 = [1..10^6]

xsA = [500000..1500000]

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
arcFunc (Arc s e) = Arc (s * 2) (e * 4) 

wqaMed :: Pattern Time
wqaMed = fromList xs5 

wqaBig :: Pattern Time
wqaBig = fromList xs6

{- fix inputs -} 
fixArg1 :: ControlPattern 
fixArg1 = pF "cc64" 1

fixArg2 :: ControlPattern
fixArg2 =
      fix ( # crush 4 ) (pF "cc65" 1)
    $ fix ( stut' 4 (0.125/4) ( + up "1" )) (pF "cc66" 1)
    $ fix ( |*| speed "-1" ) (pF "cc67" 1)
    $ fix ( (# delaytime 0.125).(# delay 0.5)) (pF "cc68" 1)
    $ fix ( # coarse 12) (pF "cc69" 1)
    $ s "[808bd:1(3,8), dr(7,8)]"
    #  pF "cc64" (cF 0 "64")
    #  pF "cc65" (cF 0 "65")
    #  pF "cc66" (cF 0 "66")
    #  pF "cc67" (cF 0 "67")
    #  pF "cc68" (cF 0 "68")
    #  pF "cc69" (cF 0 "69")

{- Euclid inputs -}
ecA1 :: [Pattern Int] 
ecA1 = [1, 100]

ecA2 :: Pattern String 
ecA2 = "x"

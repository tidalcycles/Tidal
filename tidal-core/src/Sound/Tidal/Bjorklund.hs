module Sound.Tidal.Bjorklund where

{-
    Bjorklund.hs - Euclidean patterns
    Copyright (C) 2006-2020, Rohan Drape and contributors

    This library is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this library.  If not, see <http://www.gnu.org/licenses/>.
-}

-- The below is taken from the hmt library. Tidal used to just include
-- the library but removed for now due to dependency problems.. We
-- could however likely benefit from other parts of the library..

import           Data.Bool           (bool)
import           Data.Ratio          ((%))
import           Sound.Tidal.Pattern
import           Sound.Tidal.Types
import           Sound.Tidal.Utils   (rle)

type STEP a = ((Int,Int),([[a]],[[a]]))

left :: STEP a -> STEP a
left ((i,j),(xs,ys)) =
    let (xs',xs'') = splitAt j xs
    in ((j,i-j),(zipWith (++) xs' ys,xs''))

right :: STEP a -> STEP a
right ((i,j),(xs,ys)) =
    let (ys',ys'') = splitAt i ys
    in ((i,j-i),(zipWith (++) xs ys',ys''))

bjorklund' :: STEP a -> STEP a
bjorklund' (n,x) =
    let (i,j) = n
    in if min i j <= 1
       then (n,x)
       else bjorklund' (if i > j then left (n,x) else right (n,x))

bjorklund :: (Int,Int) -> [Bool]
bjorklund (i,j') =
    let j = j' - i
        x = replicate i [True]
        y = replicate j [False]
        (_,(x',y')) = bjorklund' ((i,j),(x,y))
    in concat x' ++ concat y'

bjorklundNeg :: (Int, Int) -> [Bool]
bjorklundNeg (i,j) | i >= 0 = bjorklund (i,j)
                   | otherwise = not <$> bjorklund (-i,j)

bjorklundOff :: (Int, Int, Int) -> [Bool]
bjorklundOff (i,j,k) = take j $ drop (k `mod` j) $ cycle $ bjorklundNeg (i,j)

_euclid :: Pattern p => Int -> Int -> p Bool
_euclid n k = cat $ map pure $ bjorklundNeg (n,k)

euclid :: Pattern p => p Int -> p Int -> p Bool
euclid = patternify_P_P _euclid

_euclidTo :: Pattern p => Int -> Int -> p a -> p a
_euclidTo n k pat = fastcat $ map (bool pat silence) $ bjorklundNeg (n,k)

euclidTo :: Pattern p => p Int -> p Int -> p a -> p a
euclidTo = patternify_P_P_n _euclidTo

_euclidOff :: Pattern p => Int -> Int -> Int -> p Bool
_euclidOff i j k = cat $ map pure $ bjorklundOff (i,j,k)

euclidOff :: Pattern p => p Int -> p Int -> p Int -> p Bool
euclidOff = patternify_P_P_P _euclidOff

eoff :: Pattern p => p Int -> p Int -> p Int -> p Bool
eoff = euclidOff

_euclidOffTo :: Pattern p => Int -> Int -> Int -> p a -> p a
_euclidOffTo i j k pat = fastcat $ map (bool pat silence) $ bjorklundOff (i,j,k)

euclidOffTo :: Pattern p => p Int -> p Int -> p Int -> p a -> p a
euclidOffTo = patternify_P_P_P_n _euclidOffTo

_euclidOffToBool :: Pattern p => Int -> Int -> Int -> p Bool -> p Bool
_euclidOffToBool _ 0 _ _ = silence
_euclidOffToBool n k s pat = ((fromIntegral s % fromIntegral k) `_early`) ((\a b -> if b then a else not a) <$> _euclid n k <*> pat)

euclidOffToBool :: Pattern p => p Int -> p Int -> p Int -> p Bool -> p Bool
euclidOffToBool = patternify_P_P_P_n _euclidOffToBool

_euclidOpen :: Pattern p => Int -> Int -> p Bool
_euclidOpen n k = timeCat $ map (\(l, v) -> (toRational l, pure v)) $ rle $ bjorklundNeg (n,k)

euclidOpen :: Pattern p => p Int -> p Int -> p Bool
euclidOpen = patternify_P_P _euclidOpen

-- TODO - euclidOpenOff..

_euclidInv :: Pattern p => Int -> Int -> p Bool
_euclidInv n k = _euclid (-n) k

{- | `euclidInv` fills in the blanks left by `e`
 -
 @e 3 8 "x"@ -> @"x ~ ~ x ~ ~ x ~"@

 @euclidInv 3 8 "x"@ -> @"~ x x ~ x x ~ x"@
-}
euclidInv :: Pattern p => p Int -> p Int -> p Bool
euclidInv = patternify_P_P _euclidInv

-- {- | `euclidfull n k pa pb` stacks @e n k pa@ with @einv n k pb@ -}
-- euclidFull :: Pattern p => p Int -> p Int -> p a -> p a -> p a
-- euclidFull n k pa pb = stack [ euclid n k pa, euclidInv n k pb ]

-- _euclidBool :: Pattern p => Int -> Int -> p Bool
-- _euclidBool n k = fastFromList $ bjorklundNeg (n,k)



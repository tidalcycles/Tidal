module Sound.Tidal.Bjorklund (bjorklund) where

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

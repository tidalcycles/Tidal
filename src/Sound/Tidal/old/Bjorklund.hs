module Sound.Tidal.Bjorklund (bjorklund) where

-- The below is (c) Rohan Drape, taken from the hmt library and
-- distributed here under the terms of the GNU Public Licence.  Tidal
-- used to just include the library but removed for now due to
-- dependency problems.. We could however likely benefit from other
-- parts of the library..

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

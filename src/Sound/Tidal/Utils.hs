module Sound.Tidal.Utils where

mapBoth :: (a -> a) -> (a,a) -> (a,a)
mapBoth f (a,b) = (f a, f b)

mapPartTimes :: (a -> a) -> ((a,a),(a,a)) -> ((a,a),(a,a))
mapPartTimes f part = mapBoth (mapBoth f) part

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x,y) = (f x,y)

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (x,y) = (x,f y)

delta (a,b) = b-a

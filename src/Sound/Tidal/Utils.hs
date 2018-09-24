module Sound.Tidal.Utils where

import Data.List (delete)

mapBoth :: (a -> a) -> (a,a) -> (a,a)
mapBoth f (a,b) = (f a, f b)

mapPartTimes :: (a -> a) -> ((a,a),(a,a)) -> ((a,a),(a,a))
mapPartTimes f part = mapBoth (mapBoth f) part

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x,y) = (f x,y)

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (x,y) = (x,f y)

delta :: Num a => (a, a) -> a
delta (a,b) = b-a

removeCommon :: Eq a => [a] -> [a] -> ([a],[a])
removeCommon [] bs = ([],bs)
removeCommon as [] = (as,[])
removeCommon (a:as) bs | elem a bs = removeCommon as (delete a bs)
                       | otherwise = (a:as',bs')
                           where (as',bs') = removeCommon as bs

readMaybe        :: (Read a) => String -> Maybe a
readMaybe s      =  case [x | (x,t) <- reads s, ("","") <- lex t] of
                         [x] -> Just x
                         _   -> Nothing

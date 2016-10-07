{-|
Module: Utils
Description: Helper functions not directly specific to Tidal
-}
module Sound.Tidal.Utils where

import System.Environment (lookupEnv)
import Data.Maybe (listToMaybe, fromMaybe)
import Control.Exception

{- | enumerate a list of things

>>> enumerate ["foo","bar","baz"]
[(1,"foo"), (2,"bar"), (3,"baz")]
-}
enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

-- | apply @f@ to the first element of a tuple
mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x,y) = (f x,y)

-- | apply function to the first value of each tuple in given list
mapFsts :: (a -> b) -> [(a, c)] -> [(b, c)]
mapFsts = map . mapFst

-- | apply @f@ to the second element of a tuple
mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (x,y) = (x,f y)

-- | apply function to the second value of each tuple in given list
mapSnds :: (a -> b) -> [(c, a)] -> [(c, b)]
mapSnds = fmap . mapSnd


{- | split given list of @a@ by given single a, e.g.

>>> wordsBy (== ':') "bd:3"
["bd", "3"]
-}
wordsBy :: (a -> Bool) -> [a] -> [[a]]
wordsBy p s = case dropWhile p s of
   []      -> []
   s':rest -> (s':w) : wordsBy p (drop 1 s'')
          where (w, s'') = break p rest

maybeRead :: String -> Maybe Double
maybeRead = fmap fst . listToMaybe . reads

-- | shorthand for first element of triple
fst' (a, _, _) = a
-- | shorthand for second element of triple
snd' (_, b, _) = b
-- | shorthand for third element of triple
thd' (_, _, c) = c

-- | apply @f@ to the first element of a triple
mapFst' :: (a -> x) -> (a, b, c) -> (x, b, c)
mapFst' f (x,y,z) = (f x,y,z)

-- | apply @f@ to the second element of a triple
mapSnd' :: (b -> x) -> (a, b, c) -> (a, x, c)
mapSnd' f (x,y,z) = (x,f y,z)

-- | apply @f@ to the third element of a triple
mapThd' :: (c -> x) -> (a, b, c) -> (a, b, x)
mapThd' f (x,y,z) = (x,y,f z)

-- | apply function to the second value of each triple in given list
mapFsts' :: (a -> x) -> [(a, b, c)] -> [(x, b, c)]
mapFsts' = fmap . mapFst'

-- | apply function to the second value of each triple in given list
mapSnds' :: (b -> x) -> [(a, b, c)] -> [(a, x, c)]
mapSnds' = fmap . mapSnd'

-- | apply function to the third value of each triple in given list
mapThds' :: (c -> x) -> [(a, b, c)] -> [(a, b, x)]
mapThds' = fmap . mapThd'

-- | map @f@ over a given list of arcs
mapArcs :: (a -> a) -> [(a, a, x)] -> [(a, a, x)]
mapArcs f = (mapFsts' f) . (mapSnds' f)

{- | combines two lists by interleaving them

>>> mergelists [1,2,3] [9,8,7]
[1,9,2,8,3,7]
-}
mergelists :: [a] -> [a] -> [a]
mergelists xs     []     = xs
mergelists []     ys     = ys
mergelists (x:xs) (y:ys) = x : y : mergelists xs ys

{- | like `!!` selects @n@th element from xs, but wraps over at the end of @xs@

>>> map ((!!!) [1,3,5]) [0,1,2,3,4,5]
[1,3,5,1,3,5]
-}
(!!!) :: [a] -> Int -> a
(!!!) xs n = xs !! (n `mod` length xs)

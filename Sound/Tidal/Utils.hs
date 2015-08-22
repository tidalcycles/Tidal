module Sound.Tidal.Utils where

import System.Environment (getEnv)
import Data.Maybe (listToMaybe)
import Control.Exception

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x,y) = (f x,y)

mapFsts :: (a -> b) -> [(a, c)] -> [(b, c)]
mapFsts = map . mapFst

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (x,y) = (x,f y)

mapSnds :: (a -> b) -> [(c, a)] -> [(c, b)]
mapSnds = fmap . mapSnd

wordsBy :: (a -> Bool) -> [a] -> [[a]]
wordsBy p s = case dropWhile p s of
   []      -> []
   s':rest -> (s':w) : wordsBy p (drop 1 s'')
          where (w, s'') = break p rest

maybeRead :: String -> Maybe Double
maybeRead = fmap fst . listToMaybe . reads

fst' (a, _, _) = a
snd' (_, b, _) = b
thd' (_, _, c) = c


mapFst' :: (a -> x) -> (a, b, c) -> (x, b, c)
mapFst' f (x,y,z) = (f x,y,z)

mapSnd' :: (b -> x) -> (a, b, c) -> (a, x, c)
mapSnd' f (x,y,z) = (x,f y,z)

mapThd' :: (c -> x) -> (a, b, c) -> (a, b, x)
mapThd' f (x,y,z) = (x,y,f z)

mapFsts' :: (a -> x) -> [(a, b, c)] -> [(x, b, c)]
mapFsts' = fmap . mapFst'

mapSnds' :: (b -> x) -> [(a, b, c)] -> [(a, x, c)]
mapSnds' = fmap . mapSnd'

mapThds' :: (c -> x) -> [(a, b, c)] -> [(a, b, x)]
mapThds' = fmap . mapThd'

mapArcs :: (a -> a) -> [(a, a, x)] -> [(a, a, x)] 
mapArcs f = (mapFsts' f) . (mapSnds' f)

getEnvDefault :: String -> String -> IO String
getEnvDefault defValue var = do
  res <- try . getEnv $ var
  return $ either (const defValue) id (res :: Either IOException String)

mergelists :: [a] -> [a] -> [a]
mergelists xs     []     = xs
mergelists []     ys     = ys
mergelists (x:xs) (y:ys) = x : y : mergelists xs ys

(!!!) :: [a] -> Int -> a
(!!!) xs n = xs !! (n `mod` length xs)

module Sound.Tidal.Utils where

import Data.List (delete)
import System.IO (hPutStrLn, stderr)

writeError :: String -> IO ()
writeError = hPutStrLn stderr

mapBoth :: (a -> a) -> (a,a) -> (a,a)
mapBoth f (a,b) = (f a, f b)

mapPartTimes :: (a -> a) -> ((a,a),(a,a)) -> ((a,a),(a,a))
mapPartTimes f = mapBoth (mapBoth f)

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x,y) = (f x,y)

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (x,y) = (x,f y)

delta :: Num a => (a, a) -> a
delta (a,b) = b-a

-- | The midpoint of two values
mid :: Fractional a => (a,a) -> a
mid (a,b) = a + ((b - a) / 2)

removeCommon :: Eq a => [a] -> [a] -> ([a],[a])
removeCommon [] bs = ([],bs)
removeCommon as [] = (as,[])
removeCommon (a:as) bs | a `elem` bs = removeCommon as (delete a bs)
                       | otherwise = (a:as',bs')
                      where (as',bs') = removeCommon as bs

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case [x | (x,t) <- reads s, ("","") <- lex t] of
                   [x] -> Just x
                   _   -> Nothing

{- | like `!!` selects @n@th element from xs, but wraps over at the end of @xs@

>>> map ((!!!) [1,3,5]) [0,1,2,3,4,5]
[1,3,5,1,3,5]
-}
(!!!) :: [a] -> Int -> a
(!!!) xs n = xs !! (n `mod` length xs)


{- | Safer version of !! --}
nth :: Int -> [a] -> Maybe a
nth _ []       = Nothing
nth 0 (x : _)  = Just x
nth n (_ : xs) = nth (n - 1) xs

accumulate :: Num t => [t] -> [t]
accumulate [] = []
accumulate (x:xs) = scanl (+) x xs

{- | enumerate a list of things

>>> enumerate ["foo","bar","baz"]
[(1,"foo"), (2,"bar"), (3,"baz")]
-}
enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

{- | split given list of @a@ by given single a, e.g.

>>> wordsBy (== ':') "bd:3"
["bd", "3"]
-}
wordsBy :: (a -> Bool) -> [a] -> [[a]]
wordsBy p s = case dropWhile p s of
   []      -> []
   s':rest -> (s':w) : wordsBy p (drop 1 s'')
          where (w, s'') = break p rest

-- A hack to add to the source code context for mini-notation, so
-- events know where they are within a whole tidal pattern
deltaMini :: String -> String
deltaMini = outside 0 0
  where outside :: Int -> Int -> String -> String
        outside _ _ [] = []
        outside column line ('"':xs) = ("(deltaContext "
                                         ++ show column
                                         ++ " "
                                         ++ show line
                                         ++ " \""
                                         ++ inside (column+1) line xs
                                       )
        outside _ line ('\n':xs) = '\n':(outside 0 (line+1) xs)
        outside column line (x:xs) = x:(outside (column+1) line xs)
        inside :: Int -> Int -> String -> String
        inside _ _ [] = []
        inside column line ('"':xs) = '"':')':(outside (column+1) line xs)
        inside _ line ('\n':xs) = '\n':(inside 0 (line+1) xs)
        inside column line (x:xs) = x:(inside (column+1) line xs)

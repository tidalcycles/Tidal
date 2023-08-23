module Sound.Tidal.Utils where

import           System.IO (hPutStrLn, stderr)

writeError :: String -> IO ()
writeError = hPutStrLn stderr

rle :: Eq a => [a] -> [(Int, a)]
rle [] = []
rle (a:as) = loop (1,a) as
  where loop (n,x) [] = [(n,x)]
        loop (n,x) (x':xs) | x == x' = loop (n+1,x) xs
                           | otherwise = (n,x) : loop (1,x') xs

{- | like `!!` selects @n@th element from xs, but wraps over at the end of @xs@

>>> map ((!!!) [1,3,5]) [0,1,2,3,4,5]
[1,3,5,1,3,5]
-}
(!!!) :: [a] -> Int -> a
(!!!) xs n = xs !! (n `mod` length xs)

{- | split given list of @a@ by given single a, e.g.

>>> wordsBy (== ':') "bd:3"
["bd", "3"]
-}
wordsBy :: (a -> Bool) -> [a] -> [[a]]
wordsBy p s = case dropWhile p s of
   []      -> []
   s':rest -> (s':w) : wordsBy p (drop 1 s'')
          where (w, s'') = break p rest

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case [x | (x,t) <- reads s, ("","") <- lex t] of
                   [x] -> Just x
                   _   -> Nothing

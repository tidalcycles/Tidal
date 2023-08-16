module Sound.Tidal.Utils where

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

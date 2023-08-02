module Sound.Tidal.Utils where

rle :: Eq a => [a] -> [(Int, a)]
rle (x:xs) = loop (1,x) xs
  where loop (n,x) [] = [(n,x)]
        loop (n,x) (x':xs) | x == x' = loop (n+1,x) xs
                           | otherwise = (n,x) : loop (1,x') xs

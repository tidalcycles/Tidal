{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

module Sound.Tidal.Utils where

{-
    Utils.hs - A library of handy Haskell utility functions
    Copyright (C) 2020, Alex McLean and contributors

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

import Data.List (delete)
import Data.Set (Set)
import qualified Data.Set as Set
import System.IO (hPutStrLn, stderr)

-- import qualified Data.IntSet as IntSet
-- import Data.IntSet (IntSet)
#ifdef __GLASGOW_HASKELL__
import           GHC.Exts  (build)
#endif

writeError :: String -> IO ()
writeError = hPutStrLn stderr

mapBoth :: (a -> a) -> (a, a) -> (a, a)
mapBoth f (a, b) = (f a, f b)

mapPartTimes :: (a -> a) -> ((a, a), (a, a)) -> ((a, a), (a, a))
mapPartTimes f = mapBoth (mapBoth f)

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x, y) = (f x, y)

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (x, y) = (x, f y)

delta :: (Num a) => (a, a) -> a
delta (a, b) = b - a

-- | The midpoint of two values
mid :: (Fractional a) => (a, a) -> a
mid (a, b) = a + ((b - a) / 2)

removeCommon :: (Eq a) => [a] -> [a] -> ([a], [a])
removeCommon [] bs = ([], bs)
removeCommon as [] = (as, [])
removeCommon (a : as) bs
  | a `elem` bs = removeCommon as (delete a bs)
  | otherwise = (a : as', bs')
  where
    (as', bs') = removeCommon as bs

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case [x | (x, t) <- reads s, ("", "") <- lex t] of
  [x] -> Just x
  _ -> Nothing

-- | like `!!` selects @n@th element from xs, but wraps over at the end of @xs@
--
-- >>> map ((!!!) [1,3,5]) [0,1,2,3,4,5]
-- [1,3,5,1,3,5]
(!!!) :: [a] -> Int -> a
(!!!) xs n = xs !! (n `mod` length xs)

-- | Safer version of !! -
nth :: Int -> [a] -> Maybe a
nth _ [] = Nothing
nth 0 (x : _) = Just x
nth n (_ : xs) = nth (n - 1) xs

accumulate :: (Num t) => [t] -> [t]
accumulate [] = []
accumulate (x : xs) = scanl (+) x xs

-- | enumerate a list of things
--
-- >>> enumerate ["foo","bar","baz"]
-- [(1,"foo"), (2,"bar"), (3,"baz")]
enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]

-- | split given list of @a@ by given single a, e.g.
--
-- >>> wordsBy (== ':') "bd:3"
-- ["bd", "3"]
wordsBy :: (a -> Bool) -> [a] -> [[a]]
wordsBy p s = case dropWhile p s of
  [] -> []
  s' : rest -> (s' : w) : wordsBy p (drop 1 s'')
    where
      (w, s'') = break p rest

matchMaybe :: Maybe a -> Maybe a -> Maybe a
matchMaybe Nothing y = y
matchMaybe x _ = x

-- Available in Data.Either, but only since 4.10
fromRight :: b -> Either a b -> b
fromRight _ (Right b) = b
fromRight b _ = b

-- Available in Data.Function, but only since 4.18
applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen True f x = f x
applyWhen False _ x = x

-- pair up neighbours in list
pairs :: [a] -> [(a, a)]
pairs rs = zip rs (drop 1 rs)

-- The following is from Data.Containers.ListUtils, (c) Gershom Bazerman 2018,
-- Used under a BSD 3-clause license
-- https://hackage.haskell.org/package/containers

nubOrd :: (Ord a) => [a] -> [a]
nubOrd = nubOrdOn id
{-# INLINE nubOrd #-}

nubOrdOn :: (Ord b) => (a -> b) -> [a] -> [a]
nubOrdOn f = \xs -> nubOrdOnExcluding f Set.empty xs
{-# INLINE nubOrdOn #-}

nubOrdOnExcluding :: (Ord b) => (a -> b) -> Set b -> [a] -> [a]
nubOrdOnExcluding f = go
  where
    go _ [] = []
    go s (x : xs)
      | fx `Set.member` s = go s xs
      | otherwise = x : go (Set.insert fx s) xs
      where
        !fx = f x

#ifdef __GLASGOW_HASKELL__
{-# INLINABLE [1] nubOrdOnExcluding #-}

{-# RULES
-- Rewrite to a fusible form.
"nubOrdOn" [~1] forall f as s. nubOrdOnExcluding  f s as =
  build (\c n -> foldr (nubOrdOnFB f c) (constNubOn n) as s)

-- Rewrite back to a plain form
"nubOrdOnList" [1] forall f as s.
    foldr (nubOrdOnFB f (:)) (constNubOn []) as s =
       nubOrdOnExcluding f s as
 #-}

nubOrdOnFB :: Ord b
           => (a -> b)
           -> (a -> r -> r)
           -> a
           -> (Set b -> r)
           -> Set b
           -> r
nubOrdOnFB f c x r s
  | fx `Set.member` s = r s
  | otherwise = x `c` r (Set.insert fx s)
  where !fx = f x
{-# INLINABLE [0] nubOrdOnFB #-}

constNubOn :: a -> b -> a
constNubOn x _ = x
{-# INLINE [0] constNubOn #-}
#endif

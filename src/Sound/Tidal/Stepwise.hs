{-
    Tactus.hs - Functions that deal with stepwise manipulation of pattern
    Copyright (C) 2024, Alex McLean and contributors

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


module Sound.Tidal.Stepwise where

import           Data.List           (transpose)
import           Data.Maybe          (fromMaybe)

import qualified Sound.Tidal.Core    as T
import qualified Sound.Tidal.Pattern as T
import qualified Sound.Tidal.UI      as T

cat :: [T.Pattern a] -> T.Pattern a
cat pats = T.timecat $ map (\pat -> (fromMaybe 1 $ T.tactus pat, pat)) pats

_add :: Rational -> T.Pattern a -> T.Pattern a
-- raise error?
_add _ pat@(T.Pattern _ Nothing _) = pat
_add r pat@(T.Pattern _ (Just t) _)
  | r == 0 = T.nothing
  | (abs r) >= t = pat
  | r < 0 = T.zoom (1-((abs r)/t),1) pat
  | otherwise = T.zoom (0, (r/t)) pat

add :: T.Pattern Rational -> T.Pattern a -> T.Pattern a
add = T.tParam _add

_sub :: Rational -> T.Pattern a -> T.Pattern a
_sub _ pat@(T.Pattern _ Nothing _)  = pat
_sub r pat@(T.Pattern _ (Just t) _) | r >= t = T.nothing
                                    | r < 0 = _add (0- (t+r)) pat
                                    | otherwise = _add (t-r) pat

sub :: T.Pattern Rational -> T.Pattern a -> T.Pattern a
sub = T.tParam _sub

when :: T.Pattern Bool -> (T.Pattern a -> T.Pattern a) -> T.Pattern a -> T.Pattern a
when patb f pat@(T.Pattern _ (Just t) _) = T.while (T._steps t patb) f pat
-- TODO raise exception?
when _ _ pat                             = pat

-- _lastof :: Int -> (T.Pattern a -> T.Pattern a) -> T.Pattern a -> T.Pattern a
-- _lastof i f pat | i <= 1 = pat
--                     | otherwise = when (fastcat $ map pure $ (replicate (i-1) False) ++ [True]) f pat

_lastof :: Int -> (T.Pattern a -> T.Pattern a) -> T.Pattern a -> T.Pattern a
_lastof n f pat | n <= 1 = pat
                    | otherwise = T._fast t $ cat $ reverse $ (f $ head cycles):tail cycles
  where cycles = reverse $ T.separateCycles n $ T._slow t pat
        t = fromMaybe 1 $ T.tactus pat

lastof :: T.Pattern Int -> (T.Pattern a -> T.Pattern a) -> T.Pattern a -> T.Pattern a
lastof (T.Pattern _ _ (Just i)) f pat = _lastof i f pat
lastof tp f p                         = T.innerJoin $ (\t -> _lastof t f p) <$> tp

_firstof :: Int -> (T.Pattern a -> T.Pattern a) -> T.Pattern a -> T.Pattern a
_firstof n f pat | n <= 1 = pat
                    | otherwise = T._fast t $ cat $ (f $ head cycles):tail cycles
  where cycles = T.separateCycles n $ T._slow t pat
        t = fromMaybe 1 $ T.tactus pat

firstof :: T.Pattern Int -> (T.Pattern a -> T.Pattern a) -> T.Pattern a -> T.Pattern a
firstof (T.Pattern _ _ (Just i)) f pat = _firstof i f pat
firstof tp f p = T.innerJoin $ (\t -> _firstof t f p) <$> tp

every :: T.Pattern Int -> (T.Pattern a -> T.Pattern a) -> T.Pattern a -> T.Pattern a
every = firstof

-- | Like @taper@, but returns a list of repetitions
taperlist :: T.Pattern a -> [T.Pattern a]
taperlist pat@(T.Pattern _ (Just t) _) = pat : map (\r -> _sub r pat) [1 .. t]
-- TODO exception?
taperlist pat                          = [pat]

-- | Plays one fewer  from the pattern each repetition, down to nothing
taper :: T.Pattern a -> T.Pattern a
taper = cat . taperlist

-- | Successively plays a pattern from each group in turn
alt :: [[T.Pattern a]] -> T.Pattern a
alt groups = cat $ concat $ take (c * length groups) $ transpose $ map cycle groups
  where c = foldl1 lcm $ map length groups

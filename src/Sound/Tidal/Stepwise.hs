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

import           Sound.Tidal.Core
import           Sound.Tidal.Pattern
import           Sound.Tidal.UI      (while)
import           Sound.Tidal.Utils   (applyWhen)

s_cat :: [Pattern a] -> Pattern a
s_cat pats = timecat $ map (\pat -> (fromMaybe 1 $ tactus pat, pat)) pats

_s_add :: Rational -> Pattern a -> Pattern a
-- raise error?
_s_add _ pat@(Pattern _ Nothing _) = pat
_s_add r pat@(Pattern _ (Just t) _)
  | r == 0 = nothing
  | (abs r) >= t = pat
  | r < 0 = zoom (1-((abs r)/t),1) pat
  | otherwise = zoom (0, (r/t)) pat

s_add :: Pattern Rational -> Pattern a -> Pattern a
s_add = patternify _s_add

_s_sub :: Rational -> Pattern a -> Pattern a
_s_sub _ pat@(Pattern _ Nothing _)  = pat
_s_sub r pat@(Pattern _ (Just t) _) | r >= t = nothing
                                    | r < 0 = _s_add (0- (t+r)) pat
                                    | otherwise = _s_add (t-r) pat

s_sub :: Pattern Rational -> Pattern a -> Pattern a
s_sub = patternify _s_sub

s_while :: Pattern Bool -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
s_while patb f pat@(Pattern _ (Just t) _) = while (_steps t patb) f pat
-- TODO raise exception?
s_while _ _ pat                           = pat

_s_nth :: Bool -> Bool -> Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
_s_nth lastone stepwise n f pat
  | n <= 1 = pat
  | otherwise = applyWhen stepwise (_fast t) $ s_cat $ applyWhen lastone reverse $ (f $ head cycles):tail cycles
  where cycles = applyWhen lastone reverse $ separateCycles n $ applyWhen stepwise (_slow t) pat
        t = fromMaybe 1 $ tactus pat

s_nthcycle :: Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
s_nthcycle (Pattern _ _ (Just i)) f pat = _s_nth True False i f pat
s_nthcycle tp f p = innerJoin $ (\t -> _s_nth True False t f p) <$> tp

s_nthcycle' :: Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
s_nthcycle' (Pattern _ _ (Just i)) f pat = _s_nth False False i f pat
s_nthcycle' tp f p = innerJoin $ (\t -> _s_nth False False t f p) <$> tp

s_nth :: Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
s_nth (Pattern _ _ (Just i)) f pat = _s_nth True True i f pat
s_nth tp f p = innerJoin $ (\t -> _s_nth True True t f p) <$> tp

s_nth' :: Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
s_nth' (Pattern _ _ (Just i)) f pat = _s_nth False True i f pat
s_nth' tp f p = innerJoin $ (\t -> _s_nth False True t f p) <$> tp

s_every :: Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
s_every = s_nth'

-- | Like @s_taper@, but returns a list of repetitions
s_taperlist :: Pattern a -> [Pattern a]
s_taperlist pat@(Pattern _ (Just t) _) = pat : map (\r -> _s_sub r pat) [1 .. t]
-- TODO exception?
s_taperlist pat                        = [pat]

-- | Plays one fewer step from the pattern each repetition, down to nothing
s_taper :: Pattern a -> Pattern a
s_taper = s_cat . s_taperlist

-- | Successively plays a pattern from each group in turn
s_alt :: [[Pattern a]] -> Pattern a
s_alt groups = s_cat $ concat $ take (c * length groups) $ transpose $ map cycle groups
  where c = foldl1 lcm $ map length groups

_s_expand :: Rational -> Pattern a -> Pattern a
_s_expand factor pat = withTactus (* factor) pat

_s_contract :: Rational -> Pattern a -> Pattern a
_s_contract factor pat = withTactus (/ factor) pat

s_expand :: Pattern Rational -> Pattern a -> Pattern a
s_expand = patternify _s_expand

s_contract :: Pattern Rational -> Pattern a -> Pattern a
s_contract = patternify _s_contract

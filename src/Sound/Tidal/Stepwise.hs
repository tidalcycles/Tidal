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

stepcat :: [Pattern a] -> Pattern a
stepcat pats = timecat $ map (\pat -> (fromMaybe 1 $ tactus pat, pat)) pats

_stepadd :: Rational -> Pattern a -> Pattern a
-- raise error?
_stepadd _ pat@(Pattern _ Nothing _) = pat
_stepadd r pat@(Pattern _ (Just t) _)
  | r == 0 = nothing
  | (abs r) >= t = pat
  | r < 0 = zoom (1-((abs r)/t),1) pat
  | otherwise = zoom (0, (r/t)) pat

stepadd :: Pattern Rational -> Pattern a -> Pattern a
stepadd = tParam _stepadd

_stepsub :: Rational -> Pattern a -> Pattern a
_stepsub _ pat@(Pattern _ Nothing _)  = pat
_stepsub r pat@(Pattern _ (Just t) _) | r >= t = nothing
                                      | r < 0 = _stepadd (0- (t+r)) pat
                                      | otherwise = _stepadd (t-r) pat

stepsub :: Pattern Rational -> Pattern a -> Pattern a
stepsub = tParam _stepsub

stepwhen :: Pattern Bool -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
stepwhen patb f pat@(Pattern _ (Just t) _) = while (_steps t patb) f pat
-- TODO raise exception?
stepwhen _ _ pat                           = pat

_stepevery :: Bool -> Bool -> Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
_stepevery lastone stepwise n f pat
  | n <= 1 = pat
  | otherwise = applyWhen stepwise (_fast t) $ stepcat $ applyWhen lastone reverse $ (f $ head cycles):tail cycles
  where cycles = applyWhen lastone reverse $ separateCycles n $ applyWhen stepwise (_slow t) pat
        t = fromMaybe 1 $ tactus pat

steplastof :: Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
steplastof (Pattern _ _ (Just i)) f pat = _stepevery True False i f pat
steplastof tp f p = innerJoin $ (\t -> _stepevery True False t f p) <$> tp

stepfirstof :: Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
stepfirstof (Pattern _ _ (Just i)) f pat = _stepevery False False i f pat
stepfirstof tp f p = innerJoin $ (\t -> _stepevery False False t f p) <$> tp

steplastofstep :: Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
steplastofstep (Pattern _ _ (Just i)) f pat = _stepevery True True i f pat
steplastofstep tp f p = innerJoin $ (\t -> _stepevery True True t f p) <$> tp

stepfirstofstep :: Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
stepfirstofstep (Pattern _ _ (Just i)) f pat = _stepevery False True i f pat
stepfirstofstep tp f p = innerJoin $ (\t -> _stepevery False True t f p) <$> tp

stepevery :: Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
stepevery = stepfirstof

-- | Like @steptaper@, but returns a list of repetitions
steptaperlist :: Pattern a -> [Pattern a]
steptaperlist pat@(Pattern _ (Just t) _) = pat : map (\r -> _stepsub r pat) [1 .. t]
-- TODO exception?
steptaperlist pat                        = [pat]

-- | Plays one fewer step from the pattern each repetition, down to nothing
steptaper :: Pattern a -> Pattern a
steptaper = stepcat . steptaperlist

-- | Successively plays a pattern from each group in turn
stepalt :: [[Pattern a]] -> Pattern a
stepalt groups = stepcat $ concat $ take (c * length groups) $ transpose $ map cycle groups
  where c = foldl1 lcm $ map length groups

_stepexpand :: Rational -> Pattern a -> Pattern a
_stepexpand factor pat = withTactus (* factor) pat

_stepcontract :: Rational -> Pattern a -> Pattern a
_stepcontract factor pat = withTactus (/ factor) pat

stepexpand :: Pattern Rational -> Pattern a -> Pattern a
stepexpand = tParam _stepexpand

stepcontract :: Pattern Rational -> Pattern a -> Pattern a
stepcontract = tParam _stepcontract

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

import           Data.List           (sort, transpose, sortOn)
import           Data.Maybe          (catMaybes, fromMaybe, isJust, fromJust)

import           Sound.Tidal.Core
import           Sound.Tidal.Pattern
import           Sound.Tidal.UI      (while)
import           Sound.Tidal.Utils   (applyWhen, nubOrd, pairs, enumerate)

-- _lcmtactus :: [Pattern a] -> Maybe Time
-- _lcmtactus pats = foldl1 lcmr <$> (sequence $ map tactus pats)



s_patternify :: (a -> Pattern b -> Pattern c) -> (Pattern a -> Pattern b -> Pattern c)
s_patternify f (Pattern _ _ (Just a)) b = f a b
s_patternify f pa p                     = stepJoin $ (`f` p) <$> pa

s_patternify2 :: (a -> b -> c -> Pattern d) -> Pattern a -> Pattern b -> c -> Pattern d
s_patternify2 f a b p = stepJoin $ (\x y -> f x y p) <$> a <*> b

-- Breaks up pattern of patterns at event boundaries, then timecats them all together
stepJoin :: Pattern (Pattern a) -> Pattern a
stepJoin pp = Pattern q first_t Nothing
  where q st@(State a c) = query (stepcat $ retime $ slices $
                                  -- query whole, single cycle of pp (should there be a splitCycles here???)
                                  query (rotL (sam $ start a) pp) (st {arc = Arc 0 1})) st
        first_t :: Maybe (Pattern Rational)
        first_t = tactus $ stepcat $ retime $ slices $ queryArc pp (Arc 0 1)
        -- retime each pattern slice
        retime :: [(Time, Pattern a)] -> [Pattern a]
        retime xs = map (\(dur, pat) -> adjust dur pat) xs
          where occupied_perc = sum $ map fst $ filter (isJust . tactus . snd) xs
                occupied_tactus = sum $ catMaybes $ map (tactus . snd) xs
                total_tactus = (/ occupied_perc) <$> occupied_tactus 
                adjust _ pat@(Pattern {tactus = Just _}) = pat
                adjust dur pat = setTactus (Just $ (* dur) <$> total_tactus) pat
        -- break up events at all start/end points, into groups
        -- stacked into single patterns, with duration. Some patterns
        -- will be have no events.
        slices :: [Event (Pattern a)] -> [(Time, Pattern a)]
        slices evs = map (\s -> ((snd s - fst s), stack $ map (\x -> withContext (\c -> combineContexts [c, context x])  $ value x) $ fit s evs)) $ pairs $ sort $ nubOrd $ 0:1:concatMap (\ev -> start (part ev):stop (part ev):[]) evs
        -- list of slices of events within the given range
        fit :: (Rational, Rational) -> [Event (Pattern a)] -> [Event (Pattern a)]
        fit (b,e) evs = catMaybes $ map (match (b,e)) evs
        -- slice of event within the given range
        match :: (Rational, Rational) -> Event (Pattern a) -> Maybe (Event (Pattern a))
        match (b,e) ev = do a <- subArc (Arc b e) $ part ev
                            return ev {part = a}

stepcat :: [Pattern a] -> Pattern a
stepcat pats = innerJoin $ (timecat . map snd . sortOn fst) <$> (tpat $ epats pats)
    where -- enumerated patterns, ignoring those without tactus
          epats :: [Pattern a] -> [(Int, Pattern a)]
          epats pats = enumerate $ filter (isJust . tactus) pats
          -- 
          tpat :: [(Int, Pattern a)] -> Pattern [(Int, (Time, Pattern a))]
          tpat pats = sequence $ map (\(i, pat) -> (\t -> (i, (t, pat))) <$> (fromJust $ tactus pat) ) pats

increase :: Pattern Time -> Pattern a -> Pattern a
-- raise error?
increase _ pat@(Pattern _ Nothing _) = pat
increase npat pat@(Pattern _ (Just tpat) _) = setTactus (Just tpat') $ zoompat b e pat
  where b = (\n t -> if n >= 0 then 0 else 1-((abs n)/t)) <$> npat <*> tpat
        e = (\n t -> if n >= 0 then n/t else 1) <$> npat <*> tpat
        tpat' = (\a b -> min (abs a) b) <$> npat <*> tpat

decrease :: Pattern Rational -> Pattern a -> Pattern a
decrease npat pat@(Pattern _ Nothing _) = pat
decrease npat pat@(Pattern _ (Just tpat) _) = increase (f <$> tpat <*> npat) pat
   where f t n | n >= 0 = t - n
               | otherwise = 0 - (t + n)

_expand :: Rational -> Pattern a -> Pattern a
_expand factor pat = withTactus (* factor) pat

_contract :: Rational -> Pattern a -> Pattern a
_contract factor pat = withTactus (/ factor) pat

expand :: Pattern Rational -> Pattern a -> Pattern a
expand = s_patternify _expand

contract :: Pattern Rational -> Pattern a -> Pattern a
contract = s_patternify _contract



{-
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

s_everycycle :: Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
s_everycycle = s_nthcycle'

-- | Like @s_taper@, but returns a list of repetitions
s_taperlist :: Pattern a -> [Pattern a]
s_taperlist pat@(Pattern _ (Just t) _) = pat : map (\r -> _s_sub r pat) [1 .. t]
-- TODO exception?
s_taperlist pat                        = [pat]


s_taperlistBy :: Int -> Int -> Pattern a -> [Pattern a]
s_taperlistBy amount times pat@(Pattern _ (Just t) _) 
  | times == 1 = [pat]
  | times <= 0 = []
  | amount == 0 = [pat]
  | backwards = reverse l
  | otherwise = l
  where backwards = amount > 0
        n = toRational $ abs amount
        start = t - (toRational $ max 0 $ n * (toRational $ times - 1))
        l = (map (\i -> zoom (0, (start + (n * (toRational i))) / t) pat) [0 .. times-2]) ++ [pat]

-- | Plays one fewer step from the pattern each repetition, down to nothing
s_taper :: Pattern a -> Pattern a
s_taper = s_cat . s_taperlist

-- | Plays one fewer step from the pattern each repetition, down to nothing
_s_taperBy :: Int -> Int -> Pattern a -> Pattern a
_s_taperBy amount times pat = s_cat $ s_taperlistBy amount times pat

-- | Plays one fewer step from the pattern each repetition, down to nothing
s_taperBy :: Pattern Int -> Pattern Int -> Pattern a -> Pattern a
s_taperBy = s_patternify2 _s_taperBy

-- | Successively plays a pattern from each group in turn
s_alt :: [[Pattern a]] -> Pattern a
s_alt groups = s_cat $ concat $ take (c * length groups) $ transpose $ map cycle groups
  where c = foldl1 lcm $ map length groups

-}

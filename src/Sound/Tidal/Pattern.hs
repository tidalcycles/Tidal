-- (c) Alex McLean, Aravind Mohandas and contributors 2022
-- Shared under the terms of the GNU Public License v3.0

module Sound.Tidal.Pattern where

import Data.Ratio

import qualified Data.Map.Strict as Map

import Sound.Tidal.Value (ValueMap, Value(S,F))

class (Functor f, Applicative f, Monad f) => Pattern f where
  slowcat :: [f a] -> f a
  fastcat :: [f a] -> f a
  fastcat pats = _fast (toRational $ length pats) $ slowcat pats
  _fast :: Rational -> f a -> f a
  silence :: f a
  atom :: a -> f a
  stack :: [f a] -> f a
  _patternify :: (a -> f b -> f c) -> (f a -> f b -> f c)
  rev :: f a -> f a
  _ply :: Rational -> f a-> f a
  euclid :: f Int -> f Int -> f String -> f String
  _euclid :: Int -> Int -> f a-> f a
  timeCat :: [(Rational, f a)] -> f a
  _run :: (Enum a, Num a) => a -> f a
  _scan :: (Enum a, Num a) => a -> f a
  every :: f Int -> (f b -> f b) -> f b -> f b
  _every :: Int -> (f a -> f a) -> f a -> f a
  -- listToPat :: [a] -> f a
  iter :: f Int -> f a -> f a
  iter' :: f Int -> f a -> f a
  _iter :: Int -> f a -> f a
  _iter' :: Int -> f a -> f a

_slow :: Pattern p => Rational -> p x -> p x
_slow t = _fast (1/t)

slow :: Pattern p => p Rational -> p x -> p x
slow = _patternify _slow

-- | An alias for @slow@
sparsity :: Pattern p => p Rational -> p x -> p x
sparsity = slow

fast :: Pattern p => p Rational -> p x -> p x
fast = _patternify _fast

-- | An alias for @fast@
density :: Pattern p => p Rational -> p x -> p x
density = fast

fastAppend :: Pattern p => p x -> p x -> p x
fastAppend a b = fastcat [a,b]

slowAppend :: Pattern p => p x -> p x -> p x
slowAppend a b = slowcat [a,b]

append :: Pattern p => p x -> p x -> p x
append = slowAppend

-- | Converts from a range from 0 to 1, to a range from -1 to 1
toBipolar :: (Pattern p, Fractional x) => p x -> p x
toBipolar pat = fmap (\v -> (v*2)-1) pat

-- | Converts from a range from -1 to 1, to a range from 0 to 1
fromBipolar :: (Pattern p, Fractional x) => p x -> p x
fromBipolar pat = fmap (\v -> (v+1)/2) pat

-- | Turns a list of values into a pattern
fromList :: Pattern t => [a] -> t a
fromList = slowcat . map pure

-- | Turns a list of values into a pattern
fastFromList :: Pattern t => [a] -> t a
fastFromList = fastcat . map pure

-- | 'fromMaybes; is similar to 'fromList', but allows values to
-- be optional using the 'Maybe' type, so that 'Nothing' results in
-- gaps in the pattern.
fromMaybes :: Pattern t => [Maybe a] -> t a
fromMaybes = fastcat . map f
  where f Nothing = silence
        f (Just x) = pure x

run :: (Pattern t, Enum a, Num a) => t a -> t a
run = (>>= _run)

scan :: (Pattern t, Enum a, Num a) => t a -> t a
scan = (>>= _run)

-- ************************************************************ --

sound :: (Pattern a) => a String -> a ValueMap
sound pat = (Map.singleton "sound" . S) <$> pat


note :: (Pattern a) => a Double -> a ValueMap
note pat = (Map.singleton "note" . F) <$> pat

-- ************************************************************ --




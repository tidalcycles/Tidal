{-# OPTIONS_GHC -Wall -fno-warn-orphans -fno-warn-name-shadowing #-}

module Sound.Tidal.Sieve where

import Control.Applicative
import Data.Monoid

import Sound.Tidal.Parse
import Sound.Tidal.Pattern
import Sound.Tidal.Strategies
import Sound.Tidal.Time

-- The sieve datatype, which represents a Xenakis sieve.
-- (for an overview, see www.mitpressjournals.org/doi/pdf/10.1162/0148926054094396)

data Sieve a = Sieve {sieveAt :: Int -> a}

instance Functor Sieve where
    fmap f s = Sieve $ \i -> f (sieveAt s i)

instance Applicative Sieve where
    pure b = Sieve $ \i -> b
    (<*>) a b = Sieve $ \i -> (sieveAt a i) (sieveAt b i)

-- | The basic notation for and constructor of a sieve here is `m@@n`, which
-- represents all integers whose modulo with `m` is equal to `n`
infixl 9 @@
(@@) :: Int -> Int -> Sieve Bool
m @@ i = Sieve $ \j -> (j `mod` m) == i

-- If Haskell's logic operators had been defined on a type class, we could
-- declare Sieve to be an instance, but they haven't so here we are

-- | `#||#` gives the union (logical OR) of two sieves
infixl 2 #||#
(#||#) :: Applicative f => f Bool -> f Bool -> f Bool
(#||#) = liftA2 (||)

-- | `#&&#` gives the intersection (logical AND) of two sieves
infixl 3 #&&#
(#&&#) :: Applicative f => f Bool -> f Bool -> f Bool
(#&&#) = liftA2 (&&)

-- | `not'` gives the complement of a sieve
not' :: Applicative f => f Bool -> f Bool
not' = fmap not

-- | `#^^#` gives the exclusive disjunction (logical XOR) of two sieves
infixl 2 #^^#
(#^^#) :: Applicative f => f Bool -> f Bool -> f Bool
(#^^#) x y = (x #&&# not' y) #||# (y #&&# not' x)

-- | @sieveToList n@ returns a list of the values of the sieve for each
-- nonnegative integer less than @n@ 
-- For example: `sieveToList 10 $ 3@@1` returns 
-- `[False, True, False, False, True, False, False, True, False, False]`
sieveToList :: Int -> Sieve b -> [b]
sieveToList n s = map (sieveAt s) [0..n-1]

-- | @sieveToPat n@ returns a pattern where the cycle is divided into @n@
-- beats, and there is an event whenever the matching beat number is in the
-- sieve
-- For example: `sieveToPat 8 $ 3@@1` returns `"~ x ~ ~ x ~ ~ x"`
sieveToPat :: Int -> Sieve Bool -> Pattern String
sieveToPat n s = p $ concatMap b2s $ sieveToList n s where
  b2s b | b == True = "x " | otherwise = "~ "

-- | @stepSieve n str@ works like `sieveToPat` but uses `str` in the pattern
-- instead of `x`
stepSieve :: Int -> String -> Sieve Bool -> Pattern String
stepSieve n str sieve = step str (toString n sieve)
  where toString n s = map b2c $ sieveToList n s
        b2c b | b == True = 'x' | otherwise = '-'

-- | @slowstepSieve t@ is shorthand for applying `slow t` to the result of
-- `stepSieve`
slowstepSieve :: Pattern Time -> Int -> String -> Sieve Bool -> Pattern String
slowstepSieve t n str sieve = slow t $ stepSieve n str sieve

-- | @scaleSieve n@ turns a sieve into a list of integers less than @n@
-- present in the sieve, and then uses that with the @toScale@ function to
-- turn a pattern of numbers into a pattern of notes in the scale
-- For example: `scaleSieve 8 (3@@1) "0 1 2 1"` first converts the sieve
-- to the scale `[1, 4, 7]` and then uses that with @toScale@ to return the
-- pattern `"1 4 7 4"`
scaleSieve :: Int -> Sieve Bool -> Pattern Int -> Pattern Int
scaleSieve n sieve = toScale (toInts n sieve)
  where toInts n s = map snd $ filter fst $ zip (sieveToList n s) [0..n-1]

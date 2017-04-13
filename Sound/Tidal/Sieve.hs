{-# OPTIONS_GHC -Wall -fno-warn-orphans -fno-warn-name-shadowing #-}

module Sound.Tidal.Sieve where

import Control.Applicative

import Sound.Tidal.Parse
import Sound.Tidal.Pattern
import Sound.Tidal.Strategies
import Sound.Tidal.Time

data Sieve a = Sieve {sieveAt :: Int -> a}

instance Functor Sieve where
    fmap f s = Sieve $ \i -> f (sieveAt s i)

instance Applicative Sieve where
    pure b = Sieve $ \i -> b
    (<*>) a b = Sieve $ \i -> (sieveAt a i) (sieveAt b i)

infixl 9 @@
(@@) :: Int -> Int -> Sieve Bool
m @@ i = Sieve $ \j -> (j `mod` m) == i

infixl 2 #||#
(#||#) :: Applicative f => f Bool -> f Bool -> f Bool
(#||#) = liftA2 (||)

infixl 3 #&&#
(#&&#) :: Applicative f => f Bool -> f Bool -> f Bool
(#&&#) = liftA2 (&&)

not' :: Applicative f => f Bool -> f Bool
not' = fmap not

infixl 2 #^^#
(#^^#) :: Applicative f => f Bool -> f Bool -> f Bool
(#^^#) x y = (x #&&# not' y) #||# (y #&&# not' x)

sieveToList :: Int -> Sieve b -> [b]
sieveToList n s = map (sieveAt s) [0..n-1]

sieveToPat :: Int -> Sieve Bool -> Pattern String
sieveToPat n s = p $ concatMap b2s $ sieveToList n s where
  b2s b | b == True = "x " | otherwise = "~ "

stepSieve :: Int -> String -> Sieve Bool -> Pattern String
stepSieve n str sieve = step str (toString n sieve)
  where toString n s = map b2c $ sieveToList n s
        b2c b | b == True = 'x' | otherwise = '-'

slowstepSieve :: Pattern Time -> Int -> String -> Sieve Bool -> Pattern String
slowstepSieve t n str sieve = slow t $ stepSieve n str sieve

scaleSieve :: Int -> Sieve Bool -> Pattern Int -> Pattern Int
scaleSieve n sieve = toScale (toInts n sieve)
  where toInts n s = map snd $ filter fst $ zip (sieveToList n s) [0..n-1]

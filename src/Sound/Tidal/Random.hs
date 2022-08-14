module Random (
    rand
  , gauss
  , rayley
  , irand
  , poisson
  , binomial
  , pascal
  , geometric
  , benford
) where

import Sound.Tidal.UI (timeToRand, irand, rand)
import Sound.Tidal.Pattern

{- | Boxmuller algorightm. Maps uniform distribution to Gauss distribution.

Maps the two uniform random variables x y to two Gauss distributed variables.
x and y should be between 0 and 1. The output distribution has mean 0 and
variance 1.
-}
boxmuller :: Floating a => a -> a -> (a, a)
boxmuller x y = (r * cos t, r * sin t)
  where r = boxmullerMagnitude x
        t = 2 * pi * y


boxmullerMagnitude :: Floating a => a -> a
boxmullerMagnitude = sqrt . ((-2) *) . log


{- | Donald Knuth's algorithm to generate poisson distributed numbers.

Maps a sequence of uniform iid random numbers `us` to a poisson distributed
number with mean `lambda`.

The input `us` should be an infinite list of uniform random variables
between 0 and 1. If the list is finite, the sequence wraps which can cause
the result not to be poisson distributed any more.
-}
knuthPoisson :: (Floating a, Ord a) => [a] -> a -> Int
knuthPoisson us lambda = recurse us (exp (-lambda)) 1 0
  where recurse (u:us') l p k | p > l     = recurse us' l (p * u) (k + 1)
                              | otherwise = k
        recurse [] l p k = recurse us l p k  -- really should not occur


{- | Do n Bernoulli trials, count successes

The input `us` should have at least length `n`.
-}
doBernoulli :: (Floating a, Ord a) => [a] -> Int -> a -> Int
doBernoulli us n p = length $ filter (< p) $ take n us


{- | Do Bernoulli trials until r failures, count successes

The input `us` should be an infinite list of uniform random variables
between 0 and 1. If the list is finite, the sequence wraps which can cause
the result not to be poisson distributed any more.
-}
bernoulliUntil :: (Floating a, Ord a) => [a] -> Int -> a -> Int
bernoulliUntil us = recurse 0 us where
  recurse cnt  []     r p = recurse cnt us r p
  recurse cnt  _      r _ | r <= 0 = cnt
  recurse cnt (u:us') r p | u < p     = recurse (cnt + 1) us'  r      p
                          | otherwise = recurse  cnt      us' (r - 1) p


-- | Distribution of benfords law
benfordsLaw :: Floating a => [a]
benfordsLaw = 0 : (dist . fromIntegral <$> ([1..9] :: [Int]))
  where dist d = logBase 10 $ (d + 1) / d


{- | Sample from a discrete positive random variable with given probability
mass function.

`pmf` is the probability mass function given as a list starting from 0.
`us` is the source of randomness. Must be uniformly distributed on [0, 1).

The length of `us` should larger or equal to `ps`.
-}
withDistribution :: (Floating a, Ord a) => [a] -> [a] -> Int
withDistribution pmf us = recurse 0 0 pmf us where
  recurse cnt _    []     _      = cnt + 1
  recurse cnt acc  ps     []     = recurse cnt acc ps us
  recurse cnt acc (p:ps) (u:us') | u < acc + p = cnt
                                 | otherwise   = recurse (cnt+1) (acc+p) ps us'


fmap2 :: (a -> a -> (b,b)) -> [a] -> [b]
fmap2 f (x:x':xs) = let (y,y') = f x x' in y:y':fmap2 f xs
fmap2 _ _         = []


(<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<$$>) = fmap . fmap


-- | Infinite list of uniform [0, 1) random variables
rands :: Floating a => Pattern [a]
rands = Pattern evts
  where evts (State a@(Arc s e) _) = [Event (Context []) Nothing a rnds]
          where rnds = fmap (rnd . fromIntegral) ([1 .. ] :: [Int])
                rnd k = realToFrac (timeToRand (3.141592 * k * (e + s)/2) :: Double)


-- | Infinite list of Normal (0, 1) random variables (correlated)
gausss :: Floating a => Pattern [a]
gausss = Pattern evts
  where evts (State a@(Arc s e) _) = [Event (Context []) Nothing a values]
          where values = fmap2 boxmuller rnds
                rnds = fmap (rnd . fromIntegral) ([0 .. ] :: [Int])
                rnd k = realToFrac (timeToRand (3.141592 * k * (e + s)/2) :: Double)


-- | A normal (Gauss) distributed random signal.
gauss :: Floating a => Pattern a
gauss = head <$> gausss

-- | A rayley distributed random signal.
rayley :: Floating a => Pattern a
rayley = boxmullerMagnitude . head <$> rands

{- | A poisson distributed random signal.

Takes one parameter 'lambda' which is the expected value.
-}
poisson :: (Floating a, Ord a) => Pattern a -> Pattern Int
poisson lambda = knuthPoisson <$> rands <*> lambda


{- | Binomial distributed random signal.

Takes two parameters, the number of trials `n`
and the probability of success `p`
-}
binomial :: (Floating a, Ord a) => Pattern Int -> Pattern a -> Pattern Int
binomial n p = doBernoulli <$> rands <*> n <*> p


{- | Negative-binomial (Pascal) distributed signal.

Takes two parameters, the number of allowed failures `r`
and the probability of success `p`
-}
pascal :: (Floating a, Ord a) => Pattern Int -> Pattern a -> Pattern Int
pascal r p = bernoulliUntil <$> rands <*> r <*> p

{- | Geometric distributed signal.

Takes one parameter, which is the decay rate of the pmf
-}
geometric :: (Floating a, Ord a) => Pattern a -> Pattern Int
geometric = pascal 1 . (1-)

-- | Numbers distributed according to benfords law.
benford :: Pattern Int
benford = withDistribution benfordsLaw <$> (rands :: Pattern [Double])

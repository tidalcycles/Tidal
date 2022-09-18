module Sound.Tidal.Random where

import Prelude hiding ((<*), (*>))

import Data.Bits (xor, shiftR, rotateL)
import Data.Word (Word32)

import Sound.Tidal.Core
import Sound.Tidal.Pattern

-- | Randomisation via murmur3 hash
-- simplified code from murmur3 package
murmur3 :: Word32 -> Word32 -> Word32
murmur3 seed x = h8
  where
    k1 = x
    k2 = k1 * c1
    k3 = k2 `rotateL` 15
    k4 = k3 * c2
    k5 = seed `xor` k4
    k6 = k5 `rotateL` 13
    h1 = k6 * 5 + c3
    h2 = h1
    h3 = h2 `xor` 4
    h4 = h3 `xor` (h3 `shiftR` 16)
    h5 = h4 * c4
    h6 = h5 `xor` (h5 `shiftR` 13)
    h7 = h6 * c5
    h8 = h7 `xor` (h7 `shiftR` 16)
    c1 = 0xcc9e2d51
    c2 = 0x1b873593
    c3 = 0xe6546b64
    c4 = 0x85ebca6b
    c5 = 0xc2b2ae35

-- stretch 300 cycles over the range of [0,2**29 == 536870912) then apply the xorshift algorithm
timeToIntSeed :: RealFrac a => a -> Word32
timeToIntSeed = truncate . (* 2^(32::Int)) . remainder . (/ 2^(16::Int))
  where remainder = snd . (properFraction :: RealFrac a => a -> (Int, a))

intSeedToRand :: Fractional a => Word32 -> a
intSeedToRand = (/ 2^(32::Int)) . realToFrac

timeToRand :: (RealFrac a, Fractional b) => a -> b
timeToRand = intSeedToRand . murmur3 0 . timeToIntSeed

timeToRands :: (RealFrac a, Fractional b) => a -> [b]
timeToRands t = intSeedToRand <$> next 0 (timeToIntSeed t)
  where next :: Word32 -> Word32 -> [Word32]
        next seed s = let r = murmur3 seed s in (r : next r s)

_time :: Pattern Time
_time = sig fromRational

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
knuthPoisson (u:us) lambda = recurse us p0 u 0
  where p0 = exp (-lambda)
        recurse (u':us') l p k | p > l     = recurse us' l (p * u') (k + 1)
                               | otherwise = k
        recurse [] l p k = recurse us l p k  -- really should not occur
knuthPoisson [] _ = -1  -- really should not occur either


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


erf :: Floating a => a -> a
erf x = sqrt $ 1 - exp (- c0 * xSq * p1)
  where p1 = (1 + c1 * xSq) / (1 + c2 * xSq)
        c0 = 4 / pi
        c1 = (10 - pi*pi) / (5 * (pi - 3) * pi)
        c2 = (120 - 60 * pi + 7 * pi*pi) / (15 * (pi - 3) * pi)
        xSq = x*x

{-|

`rand` generates a continuous pattern of (pseudo-)random numbers between `0` and `1`.

@
sound "bd*8" # pan rand
@

pans bass drums randomly

@
sound "sn sn ~ sn" # gain rand
@

makes the snares' randomly loud and quiet.

Numbers coming from this pattern are 'seeded' by time. So if you reset
time (via `cps (-1)`, then `cps 1.1` or whatever cps you want to
restart with) the random pattern will emit the exact same _random_
numbers again.

In cases where you need two different random patterns, you can shift
one of them around to change the time from which the _random_ pattern
is read, note the difference:

@
jux (# gain rand) $ sound "sn sn ~ sn" # gain rand
@

and with the juxed version shifted backwards for 1024 cycles:

@
jux (# ((1024 <~) $ gain rand)) $ sound "sn sn ~ sn" # gain rand
@
-}
rand :: Fractional a => Pattern a
rand = rand' <$> _time

rand' :: (RealFrac a, Fractional b) => a -> b
rand' = timeToRand

-- | Boolean rand - a continuous stream of true/false values, with a 50/50 chance.
brand :: Pattern Bool
brand = brand' <$> _time

brand' :: RealFrac a => a -> Bool
brand' = brandBy' (0.5 :: Double)

-- | Boolean rand with probability as input, e.g. brandBy 0.25 is 25% chance of being true.
brandBy :: Pattern Double -> Pattern Bool
brandBy probpat = brandBy' <$> probpat *> _time

brandBy' :: (Fractional a, Ord a, RealFrac b) => a -> b -> Bool
brandBy' prob = (< prob) . rand'

{- | Just like `rand` but for whole numbers, `irand n` generates a pattern of (pseudo-) random whole numbers between `0` to `n-1` inclusive. Notably used to pick a random
samples from a folder:

@
d1 $ segment 4 $ n (irand 5) # sound "drum"
@
-}
irand :: Num a => Pattern Int -> Pattern a
irand i = irand' <$> i *> _time

irand' :: (RealFrac a, Num b) => Int -> a -> b
irand' i = fromIntegral . (floor :: Double -> Int) . (* fromIntegral i) . rand'


-- | Infinite list of uniform [0, 1) random variables
rands :: Floating a => Pattern [a]
rands = rands' <$> _time

rands' :: (RealFrac a, Fractional b) => a -> [b]
rands' = timeToRands

-- | Infinite list of correlated uniform [0, 1) random signals (shifted by 1)
randsC :: Floating a => Pattern [a]
randsC = randsC' <$> _time

randsC' :: (RealFrac a, Fractional b) => a -> [b]
randsC' t = rand' . (+t) . fromIntegral <$> [0 :: Int ..]

-- | Infinite list of Normal (0, 1) random variables (uncorrelated)
gausss :: Floating a => Pattern [a]
gausss = gausss' <$> _time

gausss' :: (RealFrac a, Floating b, Fractional b) => a -> [b]
gausss' = fmap2 boxmuller . rands'

-- | Infinite list of correlated normal (0, 1) random signals (shifted by 1)
gaussC :: Floating a => Pattern [a]
gaussC = gaussC' <$> _time

gaussC' :: (RealFrac a, Floating b) => a -> [b]
gaussC' = fmap2 boxmuller . randsC'

-- | A normal (Gauss) distributed random signal.
gauss :: Floating a => Pattern a
gauss = gauss' <$> _time

gauss' :: (RealFrac a, Floating b) => a -> b
gauss' = head . gausss'

-- | A rayley distributed random signal.
rayley :: Floating a => Pattern a
rayley = rayley' <$> _time

rayley' :: (RealFrac a, Floating b) => a -> b
rayley' = boxmullerMagnitude . rand'

levys :: Floating a => Pattern [a]
levys = fmap (**(-2)) . fmap2 boxmuller <$> rands

levys' :: (RealFrac a, Floating b) => a -> [b]
levys' = fmap (**(-2)) . fmap2 boxmuller . rands'

-- | A rayley distributed random signal.
levy :: Floating a => Pattern a
levy = levy' <$> _time

levy' :: (RealFrac a, Floating b) => a -> b
levy' = head . levys'

cauchy :: Floating a => Pattern a
cauchy = cauchy' <$> _time

cauchy' :: (RealFrac a, Floating b) => a -> b
cauchy' = tan . (*pi) . (+(-1/2)) . rand'

invLaw :: Floating a => Pattern a
invLaw = invLaw' <$> _time

invLaw' :: (RealFrac a, Fractional b) => a -> b
invLaw' = (+(-1)) . (1/) . rand'

{- | A poisson distributed random signal.

Takes one parameter 'lambda' which is the expected value.
-}
poisson :: (Floating a, Ord a, Num b) => Pattern a -> Pattern b
poisson lambda = poisson' <$> lambda <*> _time

poisson' :: (Floating a, Ord a, RealFrac b, Num c) => a -> b -> c
poisson' lambda t = fromIntegral $ knuthPoisson (rands' t) lambda

{- | Binomial distributed random signal.

Takes two parameters, the number of trials `n`
and the probability of success `p`
-}
binomial :: (Floating a, Ord a, Num b) => Pattern Int -> Pattern a -> Pattern b
binomial n p = binomial' <$> n <*> p <*> _time

binomial' :: (Floating a, Ord a, RealFrac b, Num c) => Int -> a -> b -> c
binomial' n p t = fromIntegral $ doBernoulli (rands' t) n p

{- | Negative-binomial (Pascal) distributed signal.

Takes two parameters, the number of allowed failures `r`
and the probability of success `p`
-}
pascal :: (Floating a, Ord a, Num b) => Pattern Int -> Pattern a -> Pattern b
pascal r p = pascal' <$> r <*> p <*> _time

pascal' :: (Floating a, Ord a, RealFrac b, Num c) => Int -> a -> b -> c
pascal' r p t = fromIntegral $ bernoulliUntil (rands' t) r p

{- | Geometric distributed signal.

Takes one parameter, which is the decay rate of the pmf
-}
geometric :: (Floating a, Ord a, Num b) => Pattern a -> Pattern b
geometric p = geometric' <$> p <*> _time

geometric' :: (Floating a, Ord a, RealFrac b, Num c) => a -> b -> c
geometric' = pascal' 1 . (1-)

-- | Numbers distributed according to benfords law.
benford :: Fractional a => Pattern a
benford = benford' <$> _time

benford' :: (RealFrac a, Fractional b) => a -> b
benford' t = fromIntegral $ withDistribution (benfordsLaw :: [Double]) (rands' t)


-- CORRELATED RANDOM SIGNALS --

smootherStep :: Floating a => a -> a
smootherStep x = 6.0 * x**5 - 15.0 * x**4 + 10.0 * x**3

{- | 1D Perlin (smooth) noise, works like rand but smoothly moves between random
values each cycle. `perlinWith` takes a pattern as the RNG's "input" instead
of automatically using the cycle count.
@
d1 $ s "arpy*32" # cutoff (perlinWith (saw * 4) * 2000)
@
will generate a smooth random pattern for the cutoff frequency which will
repeat every cycle (because the saw does)
The `perlin` function uses the cycle count as input and can be used much like @rand@.
-}
perlinWith :: Fractional a => Pattern Double -> Pattern a
perlinWith p = fmap realToFrac $ (interp) <$> (p-pa) <*> (timeToRand <$> pa) <*> (timeToRand <$> pb) where
  pa = (fromIntegral :: Int -> Double) . floor <$> p
  pb = (fromIntegral :: Int -> Double) . (+1) . floor <$> p
  interp x a b = a + smootherStep x * (b-a)

perlin :: Fractional a => Pattern a
perlin = perlinWith (sig fromRational)

perlinsWith :: Fractional a => Pattern Double -> Pattern [a]
perlinsWith p = realToFrac <$$> vals
  where pas = timeToRands . fI . floor <$> p
        pbs = timeToRands . fI . (+1) . floor <$> p
        fI :: Int -> Double
        fI = fromIntegral
        phase = p - (fI . floor <$> p)
        interp x (a,b) = a + smootherStep x * (b-a)
        vals = do
          x <- phase
          pas' <- pas
          pbs' <- pbs
          return $ interp x <$> zip pas' pbs'

perlins :: Fractional a => Pattern [a]
perlins = perlinsWith (sig fromRational)

{- `perlin2With` is Perlin noise with a 2-dimensional input. This can be
useful for more control over how the randomness repeats (or doesn't).
@
d1
 $ s "[supersaw:-12*32]"
 # lpf (rangex 60 5000 $ perlin2With (cosine*2) (sine*2))
 # lpq 0.3
@
will generate a smooth random cutoff pattern that repeats every cycle without
any reversals or discontinuities (because the 2D path is a circle).
`perlin2` only needs one input because it uses the cycle count as the
second input.
-}
perlin2With :: Pattern Double -> Pattern Double -> Pattern Double
perlin2With x y = (/2) . (+1) $ interp2 <$> xfrac <*> yfrac <*> dota <*> dotb <*> dotc <*> dotd where
  fl = fmap ((fromIntegral :: Int -> Double) . floor)
  ce = fmap ((fromIntegral :: Int -> Double) . (+1) . floor)
  xfrac = x - fl x
  yfrac = y - fl y
  randAngle a b = 2 * pi * timeToRand (a + 0.0001 * b)
  pcos x' y' = cos $ randAngle <$> x' <*> y'
  psin x' y' = sin $ randAngle <$> x' <*> y'
  dota = pcos (fl x) (fl y) * xfrac       + psin (fl x) (fl y) * yfrac
  dotb = pcos (ce x) (fl y) * (xfrac - 1) + psin (ce x) (fl y) * yfrac
  dotc = pcos (fl x) (ce y) * xfrac       + psin (fl x) (ce y) * (yfrac - 1)
  dotd = pcos (ce x) (ce y) * (xfrac - 1) + psin (ce x) (ce y) * (yfrac - 1)
  interp2 x' y' a b c d = (1.0 - s x') * (1.0 - s y') * a  +  s x' * (1.0 - s y') * b
                          + (1.0 - s x') * s y' * c  +  s x' * s y' * d
  s = smootherStep

perlin2 :: Pattern Double -> Pattern Double
perlin2 = perlin2With (sig fromRational)


-- | Auto-correlated random signal that is quasi periodic with period 1,
-- | normal distributed.
corr :: Floating a => Pattern Int -> Pattern a
corr n = do
  nInt <- n
  rs <- take nInt <$> gaussC
  let nFloat = fromIntegral nInt
  return $ sum rs / sqrt nFloat


-- | Auto-correlated random signal that is quasi periodic with period 1,
-- | uniformly distributed.
corru :: Floating a => Pattern Int -> Pattern a
corru = fmap erf . corr


-- | Auto-correlated random signal generated from random fourier coefficients
perfur :: (Fractional b, Integral a) => Pattern a -> Pattern b
perfur n = perlin + (realToFrac <$> vals)
  where coeff = fmap2 boxmuller . timeToRands' . fI . floor <$> t
        t = sig fromRational :: Pattern Double
        fI = fromIntegral :: Int -> Double
        phase = t - (fI . floor <$> t)
        phs n' x k = 2 * pi * k * x / fromIntegral n'
        base n' x = (sin . phs n' x <$> [1..]) ++ (cos . phs n' x <$> [1..])
        vals = do
          n' <- n
          x <- phase
          cs <- coeff
          return $ sum $ zipWith (*) cs (base n' x)



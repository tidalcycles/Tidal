module Sound.Tidal.Signal.Random where

import           Prelude                     hiding ((*>), (<*))

import           Data.Bits                   (shiftL, shiftR, xor)
import           Data.List                   (findIndices)
import           Data.Ratio

import           Sound.Tidal.InstanceHacks   ()
import           Sound.Tidal.Pattern
import           Sound.Tidal.Signal
import           Sound.Tidal.Signal.Waveform
import           Sound.Tidal.Types
import           Sound.Tidal.Utils

-- ************************************************************ --
-- Internal functions

-- cf. George Marsaglia (2003). "Xorshift RNGs". Journal of Statistical Software 8:14.
-- https://www.jstatsoft.org/article/view/v008i14
_xorwise :: Int -> Int
_xorwise x =
  let a = xor (shiftL x 13) x
      b = xor (shiftR a 17) a
  in xor (shiftL b 5) b

-- stretch 300 cycles over the range of [0,2**29 == 536870912) then apply the xorshift algorithm
_timeToIntSeed :: RealFrac a => a -> Int
_timeToIntSeed = _xorwise . truncate . (* 536870912) . snd . (properFraction :: (RealFrac a => a -> (Int,a))) . (/ 300)

_intSeedToRand :: Fractional a => Int -> a
_intSeedToRand = (/ 536870912) . realToFrac . (`mod` 536870912)

_timeToRand :: (RealFrac a, Fractional b) => a -> b
_timeToRand = _intSeedToRand . _timeToIntSeed

_timeToRands :: (RealFrac a, Fractional b) => a -> Int -> [b]
_timeToRands t n = _timeToRands' (_timeToIntSeed t) n

_timeToRands' :: Fractional a => Int -> Int -> [a]
_timeToRands' seed n
  | n <= 0 = []
  | otherwise = _intSeedToRand seed : _timeToRands' (_xorwise seed) (n-1)

-- ************************************************************ --
-- Pseudo-random waveforms

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

in cases where you need two different random patterns, you can shift
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
rand :: Fractional a => Signal a
rand = Signal (\(State a@(Span s e) _) -> [Event mempty Nothing a (realToFrac (_timeToRand ((e + s)/2) :: Double))])

-- | Boolean rand - a continuous stream of true/false values, with a 50/50 chance.
brand :: Signal Bool
brand = _brandBy 0.5

-- | Boolean rand with probability as input, e.g. brandBy 0.25 is 25% chance of being true.
brandBy :: Signal Double -> Signal Bool
brandBy probpat = innerJoin $ _brandBy <$> probpat

_brandBy :: Double -> Signal Bool
_brandBy prob = fmap (< prob) rand


{- | Just like `rand` but for whole numbers, `irand n` generates a pattern of (pseudo-) random whole numbers between `0` to `n-1` inclusive. Notably used to pick a random
samples from a folder:

@
d1 $ segment 4 $ n (irand 5) # sound "drum"
@
-}
irand :: Num a => Signal Int -> Signal a
irand = (>>= _irand)

_irand :: Num a => Int -> Signal a
_irand i = fromIntegral . (floor :: Double -> Int) . (* fromIntegral i) <$> rand

{- | 1 dimensional Perlin (smooth) noise, works like rand but smoothly moves between random
values each cycle. `perlinWith` takes a pattern as the RNG's "input" instead
of automatically using the cycle count.
@
d1 $ s "arpy*32" # cutoff (perlinWith (saw * 4) * 2000)
@
will generate a smooth random pattern for the cutoff frequency which will
repeat every cycle (because the saw does)
The `perlin` function uses the cycle count as input and can be used much like @rand@.
-}
perlinWith :: Fractional a => Signal Double -> Signal a
perlinWith p = fmap realToFrac $ interp <$> (p-pa) <*> (_timeToRand <$> pa) <*> (_timeToRand <$> pb) where
  pa = (fromIntegral :: Int -> Double) . floor <$> p
  pb = (fromIntegral :: Int -> Double) . (+1) . floor <$> p
  interp x a b = a + smootherStep x * (b-a)
  smootherStep x = 6.0 * x**5 - 15.0 * x**4 + 10.0 * x**3

perlin :: Fractional a => Signal a
perlin = perlinWith (waveform fromRational)

{- `perlin2With` is Perlin noise with a 2-dimensional input. This can be
useful for more control over how the randomness repeats (or doesn't).
@
d1
 $ s "[supersaw:-12*32]"
 # lpf (rangex 60 5000 $ perlin2With (cosine*2) (sine*2))
 # lpq 0.3
@
will generate a smooth random cutoff pattern that repeats every cycle without
any reversals or discontinuities (because the 2 dimensional path is a circle).
`perlin2` only needs one input because it uses the cycle count as the
second input.
-}
perlin2With :: Signal Double -> Signal Double -> Signal Double
perlin2With x y = (/2) . (+1) $ interp2 <$> xfrac <*> yfrac <*> dota <*> dotb <*> dotc <*> dotd where
  fl = fmap ((fromIntegral :: Int -> Double) . floor)
  ce = fmap ((fromIntegral :: Int -> Double) . (+1) . floor)
  xfrac = x - fl x
  yfrac = y - fl y
  randAngle a b = 2 * pi * _timeToRand (a + 0.0001 * b)
  pcos x' y' = cos $ randAngle <$> x' <*> y'
  psin x' y' = sin $ randAngle <$> x' <*> y'
  dota = pcos (fl x) (fl y) * xfrac       + psin (fl x) (fl y) * yfrac
  dotb = pcos (ce x) (fl y) * (xfrac - 1) + psin (ce x) (fl y) * yfrac
  dotc = pcos (fl x) (ce y) * xfrac       + psin (fl x) (ce y) * (yfrac - 1)
  dotd = pcos (ce x) (ce y) * (xfrac - 1) + psin (ce x) (ce y) * (yfrac - 1)
  interp2 x' y' a b c d = (1.0 - s x') * (1.0 - s y') * a  +  s x' * (1.0 - s y') * b
                          + (1.0 - s x') * s y' * c  +  s x' * s y' * d
  s x' = 6.0 * x'**5 - 15.0 * x'**4 + 10.0 * x'**3

perlin2 :: Signal Double -> Signal Double
perlin2 = perlin2With (waveform fromRational)


{- | Randomly picks an element from the given list

@
sound "superpiano(3,8)" # note (choose ["a", "e", "g", "c"])
@

plays a melody randomly choosing one of the four notes "a", "e", "g", "c".
-}
choose :: [a] -> Signal a
choose = chooseBy rand

{- | `cycleChoose` is like `choose` but only picks a new item from the list
once each cycle -}
cycleChoose :: [a] -> Signal a
cycleChoose = segment 1 . choose

chooseBy :: Signal Double -> [a] -> Signal a
chooseBy _ [] = silence
chooseBy f xs = (xs !!!) . floor <$> range 0 (fromIntegral $ length xs) f

{- | Like @choose@, but works on an a list of tuples of values and weights

@
sound "superpiano(3,8)" # note (wchoose [("a",1), ("e",0.5), ("g",2), ("c",1)])
@

in the above example, the "a" and "c" notes are twice as likely to
play as the "e" note, and half as likely to play as the "g" note.

-}
wchoose :: [(a, Double)] -> Signal a
wchoose = wchooseBy rand

wchooseBy :: Signal Double -> [(a,Double)] -> Signal a
wchooseBy pat pairs = match <$> pat
  where
    match r = values !! head (findIndices (> (r*total)) cweights)
    cweights = scanl1 (+) (map snd pairs)
    values = map fst pairs
    total = sum $ map snd pairs

-- ************************************************************ --
-- Signal degraders

{- |
Similar to `degrade` `degradeBy` allows you to control the percentage of events that
are removed. For example, to remove events 90% of the time:

@
d1 $ slow 2 $ degradeBy 0.9 $ sound "[[[feel:5*8,feel*3] feel:3*8], feel*4]"
   # accelerate "-6"
   # speed "2"
@

-}
degradeBy :: Signal Double -> Signal a -> Signal a
degradeBy = patternify_p_n _degradeBy

_degradeBy :: Double -> Signal a -> Signal a
_degradeBy = _degradeByUsing rand

-- Useful for manipulating random stream, e.g. to change 'seed'
_degradeByUsing :: Signal Double -> Double -> Signal a -> Signal a
_degradeByUsing prand x p = fmap fst $ filterValues ((> x) . snd) $ (,) <$> p <* prand

unDegradeBy :: Signal Double -> Signal a -> Signal a
unDegradeBy = patternify_p_n _unDegradeBy

_unDegradeBy :: Double -> Signal a -> Signal a
_unDegradeBy x p = fmap fst $ filterValues ((<= x) . snd) $ (,) <$> p <* rand

degradeOverBy :: Int -> Signal Double -> Signal a -> Signal a
degradeOverBy i tx p = innerJoin $ (\x -> fmap fst $ filterValues ((> x) . snd) $ (,) <$> p <* _fastRepeatCycles i rand) <$> slow (fromIntegral i) tx

{- | Use @sometimesBy@ to apply a given function "sometimes". For example, the
following code results in `density 2` being applied about 25% of the time:

@
d1 $ sometimesBy 0.25 (density 2) $ sound "bd*8"
@

There are some aliases as well:

@
sometimes = sometimesBy 0.5
often = sometimesBy 0.75
rarely = sometimesBy 0.25
almostNever = sometimesBy 0.1
almostAlways = sometimesBy 0.9
@
-}

{- | `degrade` randomly removes events from a pattern 50% of the time:

@
d1 $ slow 2 $ degrade $ sound "[[[feel:5*8,feel*3] feel:3*8], feel*4]"
   # accelerate "-6"
   # speed "2"
@

The shorthand syntax for `degrade` is a question mark: `?`. Using `?`
will allow you to randomly remove events from a portion of a pattern:

@
d1 $ slow 2 $ sound "bd ~ sn bd ~ bd? [sn bd?] ~"
@

You can also use `?` to randomly remove events from entire sub-patterns:

@
d1 $ slow 2 $ sound "[[[feel:5*8,feel*3] feel:3*8]?, feel*4]"
@
-}
degrade :: Signal a -> Signal a
degrade = _degradeBy 0.5

-- ************************************************************ --
-- Sometimes and friends

sometimesBy :: Signal Double -> (Signal a -> Signal a) -> Signal a -> Signal a
sometimesBy x f pat = overlay (degradeBy x pat) (f $ unDegradeBy x pat)

sometimesBy' :: Signal Double -> (Signal a -> Signal a) -> Signal a -> Signal a
sometimesBy' x f pat = overlay (degradeBy x pat) (unDegradeBy x $ f pat)

-- | @sometimes@ is an alias for sometimesBy 0.5 .
sometimes :: (Signal a -> Signal a) -> Signal a -> Signal a
sometimes = sometimesBy 0.5

sometimes' :: (Signal a -> Signal a) -> Signal a -> Signal a
sometimes' = sometimesBy' 0.5

-- | @often@ is an alias for sometimesBy 0.75 .
often :: (Signal a -> Signal a) -> Signal a -> Signal a
often = sometimesBy 0.75

often' :: (Signal a -> Signal a) -> Signal a -> Signal a
often' = sometimesBy' 0.75

-- | @rarely@ is an alias for sometimesBy 0.25 .
rarely :: (Signal a -> Signal a) -> Signal a -> Signal a
rarely = sometimesBy 0.25

rarely' :: (Signal a -> Signal a) -> Signal a -> Signal a
rarely' = sometimesBy' 0.25

-- | @almostNever@ is an alias for sometimesBy 0.1
almostNever :: (Signal a -> Signal a) -> Signal a -> Signal a
almostNever = sometimesBy 0.1

almostNever' :: (Signal a -> Signal a) -> Signal a -> Signal a
almostNever' = sometimesBy 0.1

-- | @almostAlways@ is an alias for sometimesBy 0.9
almostAlways :: (Signal a -> Signal a) -> Signal a -> Signal a
almostAlways = sometimesBy 0.9

almostAlways' :: (Signal a -> Signal a) -> Signal a -> Signal a
almostAlways' = sometimesBy' 0.9

never :: (Signal a -> Signal a) -> Signal a -> Signal a
never = flip const

always :: (Signal a -> Signal a) -> Signal a -> Signal a
always = id

{- | @someCyclesBy@ is a cycle-by-cycle version of @sometimesBy@. It has a
`someCycles = someCyclesBy 0.5` alias -}
someCyclesBy :: Signal Double -> (Signal a -> Signal a) -> Signal a -> Signal a
someCyclesBy pd f pat = innerJoin $ (\d -> _someCyclesBy d f pat) <$> pd

_someCyclesBy :: Double -> (Signal a -> Signal a) -> Signal a -> Signal a
_someCyclesBy x = whenT test
  where test c = _timeToRand (fromIntegral c :: Double) < x

somecyclesBy :: Signal Double -> (Signal a -> Signal a) -> Signal a -> Signal a
somecyclesBy = someCyclesBy

someCycles :: (Signal a -> Signal a) -> Signal a -> Signal a
someCycles = someCyclesBy 0.5

somecycles :: (Signal a -> Signal a) -> Signal a -> Signal a
somecycles = someCycles

-- | @randcat ps@: does a @slowcat@ on the list of signals @ps@ but
-- randomises the order in which they are played.
randcat :: [Signal a] -> Signal a
randcat ps = early (_segment 1 $ (% 1) . fromIntegral <$> (_irand (length ps) :: Signal Int)) (slowcat ps)

-- | A weighted @randcat@
wrandcat :: [(Signal a, Double)] -> Signal a
wrandcat ps = innerJoin $ wchooseBy (segment 1 rand) ps

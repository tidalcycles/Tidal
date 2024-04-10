{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-
    UI.hs - Tidal's main 'user interface' functions, for transforming
    patterns, building on the Core ones.
    Copyright (C) 2020, Alex McLean and contributors

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

{-|
    This module provides the main user interface functions, including sources
    of randomness and transformations of patterns. All these functions are available
    in the context of the TidalCycles REPL.

    Many functions in this module taking 'Pattern' values as arguments have a
    corresponding function with an underscore prepended to its name (e.g.
    'degradeBy' and '_degradeBy'). These functions accept plain values, not
    'Pattern's, and are generally intended for those developing or extending Tidal.

-}

module Sound.Tidal.UI where

import           Prelude               hiding ((*>), (<*))

import           Data.Bits             (Bits, shiftL, shiftR, testBit, xor)
import           Data.Char             (digitToInt, isDigit, ord)

import           Data.Bool             (bool)
import           Data.Fixed            (mod')
import           Data.List             (elemIndex, findIndex, findIndices,
                                        groupBy, intercalate, sort, sortOn,
                                        transpose)
import qualified Data.Map.Strict       as Map
import           Data.Maybe            (fromJust, fromMaybe, isJust, mapMaybe)
import           Data.Ratio            (Ratio, (%))
import qualified Data.Text             as T

import           Sound.Tidal.Bjorklund (bjorklund)
import           Sound.Tidal.Core
import qualified Sound.Tidal.Params    as P
import           Sound.Tidal.Pattern
import           Sound.Tidal.Utils

------------------------------------------------------------------------
-- * UI

-- ** Randomisation


{-|
An implementation of the well-known @xorshift@ random number generator.
Given a seed number, generates a reasonably random number out of it.
This is an efficient algorithm suitable for use in tight loops and used
to implement the below functions, which are used to implement 'rand'.

See George Marsaglia (2003). ["Xorshift RNGs"](https://www.jstatsoft.org/article/view/v008i14),
in Journal of Statistical Software, pages 8–14.

-}
xorwise :: Int -> Int
xorwise x =
  let a = xor (shiftL x 13) x
      b = xor (shiftR a 17) a
  in xor (shiftL b 5) b

-- stretch 300 cycles over the range of [0,2**29 == 536870912) then apply the xorshift algorithm
timeToIntSeed :: RealFrac a => a -> Int
timeToIntSeed = xorwise . truncate . (* 536870912) . snd . (properFraction :: (RealFrac a => a -> (Int,a))) . (/ 300)

intSeedToRand :: Fractional a => Int -> a
intSeedToRand = (/ 536870912) . realToFrac . (`mod` 536870912)

timeToRand :: (RealFrac a, Fractional b) => a -> b
timeToRand = intSeedToRand . timeToIntSeed

timeToRands :: (RealFrac a, Fractional b) => a -> Int -> [b]
timeToRands t n = timeToRands' (timeToIntSeed t) n

timeToRands' :: Fractional a => Int -> Int -> [a]
timeToRands' seed n
  | n <= 0 = []
  | otherwise = (intSeedToRand seed) : (timeToRands' (xorwise seed) (n-1))

{-|

@rand@ is an oscillator that generates a continuous pattern of (pseudo-)random
numbers between 0 and 1.

For example, to randomly pan around the stereo field:

> d1 $ sound "bd*8" # pan rand

Or to enjoy a randomised speed from 0.5 to 1.5, add 0.5 to it:

> d1 $ sound "arpy*4" # speed (rand + 0.5)

To make the snares randomly loud and quiet:

> sound "sn sn ~ sn" # gain rand

Numbers coming from this pattern are \'seeded\' by time. So if you reset time
(using 'resetCycles', 'setCycle', or 'cps') the random pattern will emit the
exact same _random_ numbers again.

In cases where you need two different random patterns, you can shift
one of them around to change the time from which the _random_ pattern
is read, note the difference:

> jux (# gain rand) $ sound "sn sn ~ sn" # gain rand

and with the juxed version shifted backwards for 1024 cycles:

> jux (# ((1024 <~) $ gain rand)) $ sound "sn sn ~ sn" # gain rand
-}
rand :: Fractional a => Pattern a
rand = pattern (\(State a@(Arc s e) _) -> [Event (Context []) Nothing a (realToFrac $ (timeToRand ((e + s)/2) :: Double))])

-- | Boolean rand - a continuous stream of true\/false values, with a 50\/50 chance.
brand :: Pattern Bool
brand = _brandBy 0.5

-- | Boolean rand with probability as input, e.g. @brandBy 0.25@ produces trues 25% of the time.
brandBy :: Pattern Double -> Pattern Bool
brandBy probpat = innerJoin $ (\prob -> _brandBy prob) <$> probpat

_brandBy :: Double -> Pattern Bool
_brandBy prob = fmap (< prob) rand

{- | Just like `rand` but for whole numbers, @irand n@ generates a pattern of (pseudo-) random whole numbers between @0@ to @n-1@ inclusive. Notably used to pick a random
samples from a folder:

@
d1 $ segment 4 $ n (irand 5) # sound "drum"
@
-}
irand :: Num a => Pattern Int -> Pattern a
irand = (>>= _irand)

_irand :: Num a => Int -> Pattern a
_irand i = fromIntegral . (floor :: Double -> Int) . (* fromIntegral i) <$> rand

{- | 1D Perlin (smooth) noise, works like 'rand' but smoothly moves between random
values each cycle. @perlinWith@ takes a pattern as the random number generator's
"input" instead of automatically using the cycle count.

> d1 $ s "arpy*32" # cutoff (perlinWith (saw * 4) * 2000)

will generate a smooth random pattern for the cutoff frequency which will
repeat every cycle (because the saw does).

The `perlin` function uses the cycle count as input and can be used much like @rand@.
-}
perlinWith :: Fractional a => Pattern Double -> Pattern a
perlinWith p = fmap realToFrac $ (interp) <$> (p-pa) <*> (timeToRand <$> pa) <*> (timeToRand <$> pb) where
  pa = (fromIntegral :: Int -> Double) . floor <$> p
  pb = (fromIntegral :: Int -> Double) . (+1) . floor <$> p
  interp x a b = a + smootherStep x * (b-a)
  smootherStep x = 6.0 * x**5 - 15.0 * x**4 + 10.0 * x**3

{- | As 'perlin' with a suitable choice of input pattern (@'sig' 'fromRational'@).

  The @perlin@ function produces a new random value to move to every cycle. If
  you want a new random value to be generated more or less frequently, you can use
  fast or slow, respectively:

  > d1 $ sound "bd*32" # speed (fast 4 $ perlin + 0.5)
  > d1 $ sound "bd*32" # speed (slow 4 $ perlin + 0.5)
-}
perlin :: Fractional a => Pattern a
perlin = perlinWith (sig fromRational)

{-| @perlin2With@ is Perlin noise with a 2-dimensional input. This can be
useful for more control over how the randomness repeats (or doesn't).

@
d1
  $ s "[supersaw:-12*32]"
  # lpf (rangex 60 5000 $ perlin2With (cosine*2) (sine*2))
  # lpq 0.3
@

The above will generate a smooth random cutoff pattern that repeats every cycle
without any reversals or discontinuities (because the 2D path is a circle).

See also: `perlin2`, which only needs one input because it uses the cycle count
as the second input.
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
  s x' = 6.0 * x'**5 - 15.0 * x'**4 + 10.0 * x'**3

-- | As 'perlin2' with a suitable choice of input pattern (@'sig' 'fromRational'@).
perlin2 :: Pattern Double -> Pattern Double
perlin2 = perlin2With (sig fromRational)

{- | Randomly picks an element from the given list.

@
sound "superpiano(3,8)" # note (choose ["a", "e", "g", "c"])
@

plays a melody randomly choosing one of the four notes \"a\", \"e\", \"g\", \"c\".

As with all continuous patterns, you have to be careful to give them structure; in this case choose gives you an infinitely detailed stream of random choices.

> choose = 'chooseBy' 'rand'
-}
choose :: [a] -> Pattern a
choose = chooseBy rand


{- | Given a pattern of doubles, @chooseBy@ normalizes them so that each
corresponds to an index in the provided list. The returned pattern
contains the corresponding elements in the list.

It is like choose, but instead of selecting elements of the list randomly, it
uses the given pattern to select elements.

@'choose' = chooseBy 'rand'@

The following results in the pattern @"a b c"@:

> chooseBy "0 0.25 0.5" ["a","b","c","d"]
-}
chooseBy :: Pattern Double -> [a] -> Pattern a
chooseBy _ [] = silence
chooseBy f xs = (xs !!!) . floor <$> range 0 (fromIntegral $ length xs) f

{- | Like @choose@, but works on an a list of tuples of values and weights

@
sound "superpiano(3,8)" # note (wchoose [("a",1), ("e",0.5), ("g",2), ("c",1)])
@

In the above example, the "a" and "c" notes are twice as likely to
play as the "e" note, and half as likely to play as the "g" note.

> wchoose = 'wchooseBy' 'rand'
-}
wchoose :: [(a,Double)] -> Pattern a
wchoose = wchooseBy rand

{- | Given a pattern of probabilities and a list of @(value, weight)@ pairs,
@wchooseBy@ creates a @'Pattern' value@ by choosing values based on those
probabilities and weighted appropriately by the weights in the list of pairs.
-}
wchooseBy :: Pattern Double -> [(a,Double)] -> Pattern a
wchooseBy pat pairs = match <$> pat
  where
    match r = values !! head (findIndices (> (r*total)) cweights)
    cweights = scanl1 (+) (map snd pairs)
    values = map fst pairs
    total = sum $ map snd pairs

{-| @randcat ps@: does a @slowcat@ on the list of patterns @ps@ but
  randomises the order in which they are played.

  > d1 $ sound (randcat ["bd*2 sn", "jvbass*3", "drum*2", "ht mt"])
-}
randcat :: [Pattern a] -> Pattern a
randcat ps = spread' rotL (_segment 1 $ (% 1) . fromIntegral <$> (_irand (length ps) :: Pattern Int)) (slowcat ps)

{-| As 'randcat', but allowing weighted choice.

  In the following, the first pattern is the most likely and will play about half the time, and the last pattern is the less likely, with only a 10% probability.

  > d1 $ sound
  >    $ wrandcat
  >        [ ("bd*2 sn", 5), ("jvbass*3", 2), ("drum*2", 2), ("ht mt", 1) ]
-}
wrandcat :: [(Pattern a, Double)] -> Pattern a
wrandcat ps = unwrap $ wchooseBy (segment 1 rand) ps

{- | @degrade@ randomly removes events from a pattern 50% of the time:

> d1 $ slow 2 $ degrade $ sound "[[[feel:5*8,feel*3] feel:3*8], feel*4]"
>    # accelerate "-6"
>    # speed "2"

The shorthand syntax for @degrade@ is a question mark: @?@. Using @?@
will allow you to randomly remove events from a portion of a pattern:

> d1 $ slow 2 $ sound "bd ~ sn bd ~ bd? [sn bd?] ~"

You can also use @?@ to randomly remove events from entire sub-patterns:

> d1 $ slow 2 $ sound "[[[feel:5*8,feel*3] feel:3*8]?, feel*4]"
-}
degrade :: Pattern a -> Pattern a
degrade = _degradeBy 0.5

{- |
Similar to `degrade`, @degradeBy@ allows you to control the percentage of events that
are removed. For example, to remove events 90% of the time:

@
d1 $ slow 2 $ degradeBy 0.9 $ sound "[[[feel:5*8,feel*3] feel:3*8], feel*4]"
   # accelerate "-6"
   # speed "2"
@

You can also invoke this behavior in the shorthand notation by specifying a percentage, as a
number between 0 and 1, after the question mark:

@
d1 $ s "bd hh?0.8 bd hh?0.4"
@
-}
degradeBy :: Pattern Double -> Pattern a -> Pattern a
degradeBy = tParam _degradeBy

_degradeBy :: Double -> Pattern a -> Pattern a
_degradeBy = _degradeByUsing rand

-- Useful for manipulating random stream, e.g. to change 'seed'
_degradeByUsing :: Pattern Double -> Double -> Pattern a -> Pattern a
_degradeByUsing prand x p = fmap fst $ filterValues ((> x) . snd) $ (,) <$> p <* prand

{-|
As 'degradeBy', but the pattern of probabilities represents the chances to retain rather
than remove the corresponding element.
-}
unDegradeBy :: Pattern Double -> Pattern a -> Pattern a
unDegradeBy = tParam _unDegradeBy

_unDegradeBy :: Double -> Pattern a -> Pattern a
_unDegradeBy x p = fmap fst $ filterValues ((<= x) . snd) $ (,) <$> p <* rand

degradeOverBy :: Int -> Pattern Double -> Pattern a -> Pattern a
degradeOverBy i tx p = unwrap $ (\x -> fmap fst $ filterValues ((> x) . snd) $ (,) <$> p <* fastRepeatCycles i rand) <$> slow (fromIntegral i) tx


{- | Use @sometimesBy@ to apply a given function "sometimes". For example, the
following code results in @density 2@ being applied about 25% of the time:

@
d1 $ sometimesBy 0.25 (density 2) $ sound "bd*8"
@

There are some aliases as well:

@
'sometimes' = sometimesBy 0.5
'often' = sometimesBy 0.75
'rarely' = sometimesBy 0.25
'almostNever' = sometimesBy 0.1
'almostAlways' = sometimesBy 0.9
@
-}
sometimesBy :: Pattern Double -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
sometimesBy x f pat = overlay (degradeBy x pat) (f $ unDegradeBy x pat)

{- | As 'sometimesBy', but applies the given transformation to the pattern in its entirety
before filtering its actual appearances. Less efficient than 'sometimesBy' but may
be useful when the passed pattern transformation depends on properties of the
pattern before probabilities are taken into account.

@
'sometimes'' = sometimesBy' 0.5
'often'' = sometimesBy' 0.75
'rarely'' = sometimesBy' 0.25
'almostNever'' = sometimesBy' 0.1
'almostAlways'' = sometimesBy' 0.9
@
-}
sometimesBy' :: Pattern Double -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
sometimesBy' x f pat = overlay (degradeBy x pat) (unDegradeBy x $ f pat)

-- | @sometimes@ is an alias for @'sometimesBy' 0.5@.
sometimes :: (Pattern a -> Pattern a) -> Pattern a -> Pattern a
sometimes = sometimesBy 0.5

-- | @sometimes'@ is an alias for @'sometimesBy'' 0.5@.
sometimes' :: (Pattern a -> Pattern a) -> Pattern a -> Pattern a
sometimes' = sometimesBy' 0.5

-- | @often@ is an alias for @'sometimesBy' 0.75@.
often :: (Pattern a -> Pattern a) -> Pattern a -> Pattern a
often = sometimesBy 0.75

-- | @often'@ is an alias for @'sometimesBy'' 0.75@.
often' :: (Pattern a -> Pattern a) -> Pattern a -> Pattern a
often' = sometimesBy' 0.75

-- | @rarely@ is an alias for @'sometimesBy' 0.25@.
rarely :: (Pattern a -> Pattern a) -> Pattern a -> Pattern a
rarely = sometimesBy 0.25

-- | @rarely'@ is an alias for @'sometimesBy'' 0.25@.
rarely' :: (Pattern a -> Pattern a) -> Pattern a -> Pattern a
rarely' = sometimesBy' 0.25

-- | @almostNever@ is an alias for @'sometimesBy' 0.1@.
almostNever :: (Pattern a -> Pattern a) -> Pattern a -> Pattern a
almostNever = sometimesBy 0.1

-- | @almostNever'@ is an alias for @'sometimesBy'' 0.1@.
almostNever' :: (Pattern a -> Pattern a) -> Pattern a -> Pattern a
almostNever' = sometimesBy 0.1

-- | @almostAlways@ is an alias for @'sometimesBy' 0.9@.
almostAlways :: (Pattern a -> Pattern a) -> Pattern a -> Pattern a
almostAlways = sometimesBy 0.9

-- | @almostAlways'@ is an alias for @'sometimesBy'' 0.9@.
almostAlways' :: (Pattern a -> Pattern a) -> Pattern a -> Pattern a
almostAlways' = sometimesBy' 0.9

{-|
Never apply a transformation, returning the pattern unmodified.

@never = flip const@
-}

never :: (Pattern a -> Pattern a) -> Pattern a -> Pattern a
never = flip const

{-|
Apply the transformation to the pattern unconditionally.

@always = id@
-}
always :: (Pattern a -> Pattern a) -> Pattern a -> Pattern a
always = id

{- | @someCyclesBy@ is a cycle-by-cycle version of @'sometimesBy'@.

  For example the following will either distort all of the events in a cycle, or
  none of them:

  > d1 $ someCyclesBy 0.5 (# crush 2) $ n "0 1 [~ 2] 3" # sound "arpy"
-}
someCyclesBy :: Pattern Double -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
someCyclesBy pd f pat = innerJoin $ (\d -> _someCyclesBy d f pat) <$> pd

_someCyclesBy :: Double -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
_someCyclesBy x = when test
  where test c = timeToRand (fromIntegral c :: Double) < x

-- | Alias of 'someCyclesBy'.
somecyclesBy :: Pattern Double -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
somecyclesBy = someCyclesBy

-- | @someCycles = 'someCyclesBy' 0.5@
someCycles :: (Pattern a -> Pattern a) -> Pattern a -> Pattern a
someCycles = someCyclesBy 0.5

-- | Alias of 'someCycles'.
somecycles :: (Pattern a -> Pattern a) -> Pattern a -> Pattern a
somecycles = someCycles

-- ** Pattern transformations
--
-- $patternTransformations
--
-- Pattern transformations are functions generally of type
-- @'Pattern' a -> 'Pattern' a@. This means they take a pattern of any type
-- and return a pattern of that type.

{-|
@brak@ makes a pattern sound a bit like a breakbeat. It does this by, every
other cycle, squashing the pattern to fit half a cycle, and offsetting it by a
quarter of a cycle.

@
d1 $ sound (brak "bd sn kurt")
d1 $ brak $ sound "[feel feel:3, hc:3 hc:2 hc:4 ho:1]"
@
-}
brak :: Pattern a -> Pattern a
brak = when ((== 1) . (`mod` 2)) (((1%4) `rotR`) . (\x -> fastcat [x, silence]))

{- | Divides a pattern into a given number of subdivisions, plays the subdivisions
in order, but increments the starting subdivision each cycle. The pattern
wraps to the first subdivision after the last subdivision is played.

Example:

@
d1 $ iter 4 $ sound "bd hh sn cp"
@

This will produce the following over four cycles:

@
bd hh sn cp
hh sn cp bd
sn cp bd hh
cp bd hh sn
@

There is also `iter'`, which shifts the pattern in the opposite direction.

-}
iter :: Pattern Int -> Pattern c -> Pattern c
iter a pat = keepTactus pat $ tParam _iter a pat

_iter :: Int -> Pattern a -> Pattern a
_iter n p = slowcat $ map (\i -> (fromIntegral i % fromIntegral n) `rotL` p) [0 .. (n-1)]

{- | @iter'@ is the same as @iter@, but decrements the starting
subdivision instead of incrementing it. For example,

@
d1 $ iter' 4 $ sound "bd hh sn cp"
@

produces

@
bd hh sn cp
cp bd hh sn
sn cp bd hh
hh sn cp bd
@
-}
iter' :: Pattern Int -> Pattern c -> Pattern c
iter' = tParam _iter'

_iter' :: Int -> Pattern a -> Pattern a
_iter' n p = slowcat $ map (\i -> (fromIntegral i % fromIntegral n) `rotR` p) [0 .. (n-1)]

{- | @palindrome p@ applies @rev@ to @p@ every other cycle, so that the pattern
alternates between forwards and backwards. For example, these are equivalent:

@
d1 $ palindrome $ sound "arpy:0 arpy:1 arpy:2 arpy:3"
d1 $ slow 2 $ sound "arpy:0 arpy:1 arpy:2 arpy:3 arpy:3 arpy:2 arpy:1 arpy:0"
d1 $ every 2 rev $ sound "arpy:0 arpy:1 arpy:2 arpy:3"
@
-}
palindrome :: Pattern a -> Pattern a
palindrome p = slowAppend p (rev p)

-- | Degrades a pattern over the given time.
fadeOut :: Time -> Pattern a -> Pattern a
fadeOut dur p = innerJoin $ (`_degradeBy` p) <$> _slow dur envL

-- | Alternate version to @fadeOut@ where you can provide the time from which the fade starts
fadeOutFrom :: Time -> Time -> Pattern a -> Pattern a
fadeOutFrom from dur p = innerJoin $ (`_degradeBy` p) <$> (from `rotR` _slow dur envL)

-- | ’Undegrades’ a pattern over the given time.
fadeIn :: Time -> Pattern a -> Pattern a
fadeIn dur p = innerJoin $ (`_degradeBy` p) <$> _slow dur envLR

-- | Alternate version to @fadeIn@ where you can provide the time from
-- which the fade in starts
fadeInFrom :: Time -> Time -> Pattern a -> Pattern a
fadeInFrom from dur p = innerJoin $ (`_degradeBy` p) <$> (from `rotR` _slow dur envLR)

{- | The 'spread' function allows you to take a pattern transformation
which takes a parameter, such as `slow`, and provide several
parameters which are switched between. In other words it "spreads" a
function across several values.

Taking a simple high hat loop as an example:

> d1 $ sound "ho ho:2 ho:3 hc"

We can slow it down by different amounts, such as by a half:

> d1 $ slow 2 $ sound "ho ho:2 ho:3 hc"

Or by four thirds (i.e. speeding it up by a third; @4%3@ means four over
three):

> d1 $ slow (4%3) $ sound "ho ho:2 ho:3 hc"

But if we use `spread`, we can make a pattern which alternates between
the two speeds:

> d1 $ spread slow [2,4%3] $ sound "ho ho:2 ho:3 hc"

Note that if you pass @($)@ as the function to spread values over, you
can put functions as the list of values. ('spreadf' is an alias for @spread ($)@.)
For example:

> d1 $ spread ($) [density 2, rev, slow 2, striate 3, (# speed "0.8")]
>    $ sound "[bd*2 [~ bd]] [sn future]*2 cp jvbass*4"

Above, the pattern will have these transforms applied to it, one at a time, per cycle:

* cycle 1: @density 2@ - pattern will increase in speed
* cycle 2: @rev@ - pattern will be reversed
* cycle 3: @slow 2@ - pattern will decrease in speed
* cycle 4: @striate 3@ - pattern will be granualized
* cycle 5: @(# speed "0.8")@ - pattern samples will be played back more slowly

After @(# speed "0.8")@, the transforms will repeat and start at @density 2@ again.
-}
spread :: (a -> t -> Pattern b) -> [a] -> t -> Pattern b
spread f xs p = slowcat $ map (`f` p) xs

-- | An alias for 'spread' consistent with 'fastspread'.
slowspread :: (a -> t -> Pattern b) -> [a] -> t -> Pattern b
slowspread = spread

{- | @fastspread@ works the same as `spread`, but the result is squashed into a single cycle. If you gave four values to @spread@, then the result would seem to speed up by a factor of four. Compare these two:

> d1 $ spread chop [4,64,32,16] $ sound "ho ho:2 ho:3 hc"
> d1 $ fastspread chop [4,64,32,16] $ sound "ho ho:2 ho:3 hc"

There is also `slowspread`, which is an alias of @spread@.
-}
fastspread :: (a -> t -> Pattern b) -> [a] -> t -> Pattern b
fastspread f xs p = fastcat $ map (`f` p) xs

{- | There's a version of this function, `spread'` (pronounced "spread prime"), which takes a /pattern/ of parameters, instead of a list:

> d1 $ spread' slow "2 4%3" $ sound "ho ho:2 ho:3 hc"

This is quite a messy area of Tidal—due to a slight difference of
implementation this sounds completely different! One advantage of
using `spread'` though is that you can provide polyphonic parameters, e.g.:

> d1 $ spread' slow "[2 4%3, 3]" $ sound "ho ho:2 ho:3 hc"
-}
spread' :: Monad m => (a -> b -> m c) -> m a -> b -> m c
spread' f vpat pat = vpat >>= \v -> f v pat

{- | @spreadChoose f xs p@ is similar to `slowspread` but picks values from
@xs@ at random, rather than cycling through them in order.

> d1 $ spreadChoose ($) [gap 4, striate 4] $ sound "ho ho:2 ho:3 hc"
-}
spreadChoose :: (t -> t1 -> Pattern b) -> [t] -> t1 -> Pattern b
spreadChoose f vs p = do v <- _segment 1 (choose vs)
                         f v p

-- | A shorter alias for 'spreadChoose'.
spreadr :: (t -> t1 -> Pattern b) -> [t] -> t1 -> Pattern b
spreadr = spreadChoose

{-| Decide whether to apply one or another function depending on the result of a test function, which is passed the current cycle as a number.

@
d1 $ ifp ((== 0) . flip mod 2)
         (striate 4)
         (# coarse "24 48")
   $ sound "hh hc"
@

This will apply @'striate' 4@ for every /even/ cycle and apply @# coarse "24 48"@ for every /odd/.

Detail: As you can see the test function is arbitrary and does not rely on
anything Tidal specific. In fact it uses only plain Haskell functionality, that
is: it calculates the modulo of 2 of the current cycle which is either 0 (for
even cycles) or 1. It then compares this value against 0 and returns the result,
which is either @True@ or @False@. This is what the @ifp@ signature's first part
signifies: @(Int -> Bool)@, a function that takes a whole number and returns
either @True@ or @False@.
-}
ifp :: (Int -> Bool) -> (Pattern a -> Pattern a) -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
ifp test f1 f2 p = splitQueries $ p {query = q, pureValue = Nothing}
  where q a | test (floor $ start $ arc a) = query (f1 p) a
            | otherwise = query (f2 p) a

-- | @wedge t p p'@ combines patterns @p@ and @p'@ by squashing the
-- @p@ into the portion of each cycle given by @t@, and @p'@ into the
-- remainer of each cycle.
-- > d1 $ wedge (1/4) (sound "bd*2 arpy*3 cp sn*2") (sound "odx [feel future]*2 hh hh")
wedge :: Pattern Time -> Pattern a -> Pattern a -> Pattern a
wedge pt pa pb = innerJoin $ (\t -> _wedge t pa pb) <$> pt

_wedge :: Time -> Pattern a -> Pattern a -> Pattern a
_wedge 0 _ p' = p'
_wedge 1 p _  = p
_wedge t p p' = overlay (_fastGap (1/t) p) (t `rotR` _fastGap (1/(1-t)) p')


{- | @whenmod@ has a similar form and behavior to `every`, but requires an
additional number. It applies the function to the pattern when the
remainder of the current loop number divided by the first parameter
is greater or equal than the second parameter.

For example, the following makes every other block of four loops twice
as dense:

> d1 $ whenmod 8 4 (density 2) (sound "bd sn kurt")
-}
whenmod :: Pattern Time -> Pattern Time -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
whenmod a b f pat = innerJoin $ (\a' b' -> _whenmod a' b' f pat) <$> a <*> b

_whenmod :: Time -> Time -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
_whenmod a b = whenT (\t -> ((t `mod'` a) >= b ))


{- |
> superimpose f p = stack [p, f p]

@superimpose@ plays a modified version of a pattern at the same time as the
original pattern, resulting in two patterns being played at the same time. The
following are equivalent:

> d1 $ superimpose (fast 2) $ sound "bd sn [cp ht] hh"
> d1 $ stack [sound "bd sn [cp ht] hh",
>             fast 2 $ sound "bd sn [cp ht] hh"
>            ]

More examples:

> d1 $ superimpose (density 2) $ sound "bd sn [cp ht] hh"
> d1 $ superimpose ((# speed "2") . (0.125 <~)) $ sound "bd sn cp hh"

-}
superimpose :: (Pattern a -> Pattern a) -> Pattern a -> Pattern a
superimpose f p = stack [p, f p]

{- | @trunc@ truncates a pattern so that only a fraction of the pattern is played.
The following example plays only the first quarter of the pattern:

> d1 $ trunc 0.25 $ sound "bd sn*2 cp hh*4 arpy bd*2 cp bd*2"

You can also pattern the first parameter, for example to cycle through three values, one per cycle:

> d1 $ trunc "<0.75 0.25 1>" $ sound "bd sn:2 [mt rs] hc"
-}
trunc :: Pattern Time -> Pattern a -> Pattern a
trunc = tParam _trunc

_trunc :: Time -> Pattern a -> Pattern a
_trunc t = compress (0, t) . zoomArc (Arc 0 t)

{- | @linger@ is similar to `trunc`, in that it truncates a pattern so that
only the first fraction of the pattern is played, but the truncated part of the
pattern loops to fill the remainder of the cycle.

> d1 $ linger 0.25 $ sound "bd sn*2 cp hh*4 arpy bd*2 cp bd*2"

For example this repeats the first quarter, so you only hear a single repeating note:

> d1 $ linger 0.25 $ n "0 2 [3 4] 2" # sound "arpy"

or slightly more interesting, applied only every fourth cycle:

> d1 $ every 4 (linger 0.25) $ n "0 2 [3 4] 2" # sound "arpy"

or to a chopped-up sample:

> d1 $ every 2 (linger 0.25) $ loopAt 2 $ chop 8 $ sound "breaks125"

You can also pattern the first parameter, for example to cycle through three
values, one per cycle:

> d1 $ linger "<0.75 0.25 1>" $ sound "bd sn:2 [mt rs] hc"
> d1 $ linger "<0.25 0.5 1>" $ loopAt 2 $ chop 8 $ sound "breaks125"

If you give it a negative number, it will linger on the last part of
the pattern, instead of the start of it. E.g. to linger on the last
quarter:

> d1 $ linger (-0.25) $ sound "bd sn*2 cp hh*4 arpy bd*2 cp bd*2"
-}
linger :: Pattern Time -> Pattern a -> Pattern a
linger = tParam _linger

_linger :: Time -> Pattern a -> Pattern a
_linger n p | n < 0 = _fast (1/n) $ zoomArc (Arc (1 + n) 1) p
            | otherwise = _fast (1/n) $ zoomArc (Arc 0 n) p

{- |
Use @within@ to apply a function to only a part of a pattern. It takes two
arguments: a start time and an end time, specified as floats between 0 and 1,
which are applied to the relevant pattern. Note that the second argument must be
greater than the first for the function to have any effect.

For example, to apply @'fast' 2@ to only the first half of a pattern:

> d1 $ within (0, 0.5) (fast 2) $ sound "bd*2 sn lt mt hh hh hh hh"

Or, to apply @(# 'speed' "0.5")@ to only the last quarter of a pattern:

> d1 $ within (0.75, 1) (# speed "0.5") $ sound "bd*2 sn lt mt hh hh hh hh"
-}
within :: (Time, Time) -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
within (s, e) f p = stack [filterWhen (\t -> cyclePos t >= s && cyclePos t < e) $ f p,
                           filterWhen (\t -> not $ cyclePos t >= s && cyclePos t < e) p
                          ]

withinArc :: Arc -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
withinArc (Arc s e) = within (s, e)

{- |
For many cases, @within'@ will function exactly as within.
The difference between the two occurs when applying functions that change the timing of notes such as 'fast' or '<~'.
within first applies the function to all notes in the cycle, then keeps the results in the specified interval, and then combines it with the old cycle (an "apply split combine" paradigm).
within' first keeps notes in the specified interval, then applies the function to these notes, and then combines it with the old cycle (a "split apply combine" paradigm).

For example, whereas using the standard version of within

> d1 $ within (0, 0.25) (fast 2) $ sound "bd hh cp sd"

sounds like:

> d1 $ sound "[bd hh] hh cp sd"

using this alternative version, within'

> d1 $ within' (0, 0.25) (fast 2) $ sound "bd hh cp sd"

sounds like:

> d1 $ sound "[bd bd] hh cp sd"

-}
within' :: (Time, Time) -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
within' a@(s, e) f p =
  stack [ filterWhen (\t -> cyclePos t >= s && cyclePos t < e) $ compress a $ f $ zoom a p
        , filterWhen (\t -> not $ cyclePos t >= s && cyclePos t < e) p
        ]

{-|
Reverse the part of the pattern sliced out by the @(start, end)@ pair.

@revArc a = within a rev@
-}
revArc :: (Time, Time) -> Pattern a -> Pattern a
revArc a = within a rev

{- | You can use the @euclid@ function to apply a Euclidean algorithm over a
complex pattern, although the structure of that pattern will be lost:

> d1 $ euclid 3 8 $ sound "bd*2 [sn cp]"

In the above, three sounds are picked from the pattern on the right according
to the structure given by the @euclid 3 8@. It ends up picking two @bd@ sounds, a
@cp@ and missing the @sn@ entirely.

A negative first argument provides the inverse of the euclidean pattern.

These types of sequences use "Bjorklund's algorithm", which wasn't made for
music but for an application in nuclear physics, which is exciting. More
exciting still is that it is very similar in structure to the one of the first
known algorithms written in Euclid's book of elements in 300 BC. You can read
more about this in the paper
[The Euclidean Algorithm Generates Traditional Musical Rhythms](http://cgm.cs.mcgill.ca/~godfried/publications/banff.pdf)
by Toussaint. Some examples from this paper are included below,
including rotation as a third parameter in some cases (see 'euclidOff').

+------------+-----------------------------------------------------------------+
| Pattern    | Example                                                         |
+============+=================================================================+
| (2,5)      | A thirteenth century Persian rhythm called Khafif-e-ramal.      |
+------------+-----------------------------------------------------------------+
| (3,4)      | The archetypal pattern of the Cumbia from Colombia, as well as  |
|            | a Calypso rhythm from Trinidad.                                 |
+------------+-----------------------------------------------------------------+
| (3,5,2)    | Another thirteenth century Persian rhythm by the name of        |
|            | Khafif-e-ramal, as well as a Rumanian folk-dance rhythm.        |
+------------+-----------------------------------------------------------------+
| (3,7)      | A Ruchenitza rhythm used in a Bulgarian folk-dance.             |
+------------+-----------------------------------------------------------------+
| (3,8)      | The Cuban tresillo pattern.                                     |
+------------+-----------------------------------------------------------------+
| (4,7)      | Another Ruchenitza Bulgarian folk-dance rhythm.                 |
+------------+-----------------------------------------------------------------+
| (4,9)      | The Aksak rhythm of Turkey.                                     |
+------------+-----------------------------------------------------------------+
| (4,11)     | The metric pattern used by Frank Zappa in his piece titled      |
|            | Outside Now.                                                    |
+------------+-----------------------------------------------------------------+
| (5,6)      | Yields the York-Samai pattern, a popular Arab rhythm.           |
+------------+-----------------------------------------------------------------+
| (5,7)      | The Nawakhat pattern, another popular Arab rhythm.              |
+------------+-----------------------------------------------------------------+
| (5,8)      | The Cuban cinquillo pattern.                                    |
+------------+-----------------------------------------------------------------+
| (5,9)      | A popular Arab rhythm called Agsag-Samai.                       |
+------------+-----------------------------------------------------------------+
| (5,11)     | The metric pattern used by Moussorgsky in                       |
|            | Pictures at an Exhibition.                                      |
+------------+-----------------------------------------------------------------+
| (5,12)     | The Venda clapping pattern of a South African children’s song.  |
+------------+-----------------------------------------------------------------+
| (5,16)     | The Bossa-Nova rhythm necklace of Brazil.                       |
+------------+-----------------------------------------------------------------+
| (7,8)      | A typical rhythm played on the Bendir (frame drum).             |
+------------+-----------------------------------------------------------------+
| (7,12)     | A common West African bell pattern.                             |
+------------+-----------------------------------------------------------------+
| (7,16,14)  | A Samba rhythm necklace from Brazil.                            |
+------------+-----------------------------------------------------------------+
| (9,16)     | A rhythm necklace used in the Central African Republic.         |
+------------+-----------------------------------------------------------------+
| (11,24,14) | A rhythm necklace of the Aka Pygmies of Central Africa.         |
+------------+-----------------------------------------------------------------+
| (13,24,5)  | Another rhythm necklace of the Aka Pygmies of the upper Sangha. |
+------------+-----------------------------------------------------------------+

There was once a shorter alias @e@ for this function. It has been removed, but you
may see references to it in older Tidal code.
-}
euclid :: Pattern Int -> Pattern Int -> Pattern a -> Pattern a
euclid = tParam2 _euclid

_euclid :: Int -> Int -> Pattern a -> Pattern a
_euclid n k a | n >= 0 = fastcat $ fmap (bool silence a) $ bjorklund (n,k)
              | otherwise = fastcat $ fmap (bool a silence) $ bjorklund (-n,k)

{- |

@euclidFull n k pa pb@ stacks @'euclid' n k pa@ with @'euclidInv' n k pb@. That
is, it plays one pattern on the euclidean rhythm and a different pattern on
the off-beat.

For example, to implement the traditional flamenco rhythm, you could use hard
claps for the former and soft claps for the latter:

> d1 $ euclidFull 3 7 "realclaps" ("realclaps" # gain 0.8)

-}
euclidFull :: Pattern Int -> Pattern Int -> Pattern a -> Pattern a -> Pattern a
euclidFull n k pa pb = stack [ euclid n k pa, euclidInv n k pb ]

-- | Less expressive than 'euclid' due to its constrained types, but may be more efficient.
_euclidBool :: Int -> Int -> Pattern Bool -- TODO: add 'euclidBool'?
_euclidBool n k | n >= 0 = fastFromList $ bjorklund (n,k)
                | otherwise = fastFromList $ fmap (not) $ bjorklund (-n,k)

_euclid' :: Int -> Int -> Pattern a -> Pattern a
_euclid' n k p = fastcat $ map (\x -> if x then p else silence) (bjorklund (n,k))

{- |
As 'euclid', but taking a third rotational parameter corresponding to the onset
at which to start the rhythm.
-}
euclidOff :: Pattern Int -> Pattern Int -> Pattern Int -> Pattern a -> Pattern a
euclidOff = tParam3 _euclidOff

-- | A shorter alias for 'euclidOff'.
eoff :: Pattern Int -> Pattern Int -> Pattern Int -> Pattern a -> Pattern a
eoff = euclidOff

_euclidOff :: Int -> Int -> Int -> Pattern a -> Pattern a
_euclidOff _ 0 _ _ = silence
_euclidOff n k s p = (rotL $ fromIntegral s%fromIntegral k) (_euclid n k p)

-- | As 'euclidOff', but specialized to 'Bool'. May be more efficient than 'euclidOff'.
euclidOffBool :: Pattern Int -> Pattern Int -> Pattern Int -> Pattern Bool -> Pattern Bool
euclidOffBool = tParam3 _euclidOffBool

_euclidOffBool :: Int -> Int -> Int -> Pattern Bool -> Pattern Bool
_euclidOffBool _ 0 _ _ = silence
_euclidOffBool n k s p = ((fromIntegral s % fromIntegral k) `rotL`) ((\a b -> if b then a else not a) <$> _euclidBool n k <*> p)

distrib :: [Pattern Int] -> Pattern a -> Pattern a
distrib ps p = do p' <- sequence ps
                  _distrib p' p

_distrib :: [Int] -> Pattern a -> Pattern a
_distrib xs p = boolsToPat (foldr distrib' (replicate (last xs) True) (reverse $ layers xs)) p
  where
    distrib' :: [Bool] -> [Bool] -> [Bool]
    distrib' [] _           = []
    distrib' (_:a) []       = False : distrib' a []
    distrib' (True:a) (x:b) = x : distrib' a b
    distrib' (False:a) b    = False : distrib' a b
    layers = map bjorklund . (zip<*>tail)
    boolsToPat a b' = flip const <$> filterValues (== True) (fastFromList a) <* b'

{-| @euclidInv@ fills in the blanks left by `euclid`, i.e., it inverts the
pattern.

For example, whereas @euclid 3 8 "x"@ produces

> "x ~ ~ x ~ ~ x ~"

@euclidInv 3 8 "x"@ produces

> "~ x x ~ x x ~ x"

As another example, in

> d1 $ stack [ euclid 5 8 $ s "bd"
>            , euclidInv 5 8 $ s "hh27"
>            ]

the hi-hat event fires on every one of the eight even beats that the bass drum
does not.
-}
euclidInv :: Pattern Int -> Pattern Int -> Pattern a -> Pattern a
euclidInv = tParam2 _euclidInv

_euclidInv :: Int -> Int -> Pattern a -> Pattern a
_euclidInv n k a = _euclid (-n) k a

index :: Real b => b -> Pattern b -> Pattern c -> Pattern c
index sz indexpat pat =
  spread' (zoom' $ toRational sz) (toRational . (*(1-sz)) <$> indexpat) pat
  where
    zoom' tSz s = zoomArc (Arc s (s+tSz))

{-
-- | @prrw f rot (blen, vlen) beatPattern valuePattern@: pattern rotate/replace.
prrw :: (a -> b -> c) -> Int -> (Time, Time) -> Pattern a -> Pattern b -> Pattern c
prrw f rot (blen, vlen) beatPattern valuePattern =
  let
    ecompare (_,e1,_) (_,e2,_) = compare (fst e1) (fst e2)
    beats  = sortBy ecompare $ arc beatPattern (0, blen)
    values = fmap thd' . sortBy ecompare $ arc valuePattern (0, vlen)
    cycles = blen * (fromIntegral $ lcm (length beats) (length values) `div` (length beats))
  in
    _slow cycles $ stack $ zipWith
    (\( _, (start, end), v') v -> (start `rotR`) $ densityGap (1 / (end - start)) $ pure (f v' v))
    (sortBy ecompare $ arc (_fast cycles $ beatPattern) (0, blen))
    (drop (rot `mod` length values) $ cycle values)

-- | @prr rot (blen, vlen) beatPattern valuePattern@: pattern rotate/replace.
prr :: Int -> (Time, Time) -> Pattern String -> Pattern b -> Pattern b
prr = prrw $ flip const

{-|
@preplace (blen, plen) beats values@ combines the timing of @beats@ with the values
of @values@. Other ways of saying this are:
* sequential convolution
* @values@ quantized to @beats@.

Examples:

@
d1 $ sound $ preplace (1,1) "x [~ x] x x" "bd sn"
d1 $ sound $ preplace (1,1) "x(3,8)" "bd sn"
d1 $ sound $ "x(3,8)" <~> "bd sn"
d1 $ sound "[jvbass jvbass:5]*3" |+| (shape $ "1 1 1 1 1" <~> "0.2 0.9")
@

It is assumed the pattern fits into a single cycle. This works well with
pattern literals, but not always with patterns defined elsewhere. In those cases
use @preplace@ and provide desired pattern lengths:
@
let p = slow 2 $ "x x x"

d1 $ sound $ preplace (2,1) p "bd sn"
@
-}
preplace :: (Time, Time) -> Pattern String -> Pattern b -> Pattern b
preplace = preplaceWith $ flip const

-- | @prep@ is an alias for preplace.
prep :: (Time, Time) -> Pattern String -> Pattern b -> Pattern b
prep = preplace

preplace1 :: Pattern String -> Pattern b -> Pattern b
preplace1 = preplace (1, 1)

preplaceWith :: (a -> b -> c) -> (Time, Time) -> Pattern a -> Pattern b -> Pattern c
preplaceWith f (blen, plen) = prrw f 0 (blen, plen)

prw :: (a -> b -> c) -> (Time, Time) -> Pattern a -> Pattern b -> Pattern c
prw = preplaceWith

preplaceWith1 :: (a -> b -> c) -> Pattern a -> Pattern b -> Pattern c
preplaceWith1 f = prrw f 0 (1, 1)

prw1 :: (a -> b -> c) -> Pattern a -> Pattern b -> Pattern c
prw1 = preplaceWith1

(<~>) :: Pattern String -> Pattern b -> Pattern b
(<~>) = preplace (1, 1)

-- | @protate len rot p@ rotates pattern @p@ by @rot@ beats to the left.
-- @len@: length of the pattern, in cycles.
-- Example: @d1 $ every 4 (protate 2 (-1)) $ slow 2 $ sound "bd hh hh hh"@
protate :: Time -> Int -> Pattern a -> Pattern a
protate len rot p = prrw (flip const) rot (len, len) p p

prot :: Time -> Int -> Pattern a -> Pattern a
prot = protate

prot1 :: Int -> Pattern a -> Pattern a
prot1 = protate 1

{-| The @<<~@ operator rotates a unit pattern to the left, similar to @<~@,
but by events rather than linear time. The timing of the pattern remains constant:

@
d1 $ (1 <<~) $ sound "bd ~ sn hh"
-- will become
d1 $ sound "sn ~ hh bd"
@ -}

(<<~) :: Int -> Pattern a -> Pattern a
(<<~) = protate 1

-- | @~>>@ is like @<<~@ but for shifting to the right.
(~>>) :: Int -> Pattern a -> Pattern a
(~>>) = (<<~) . (0-)

-- | @pequal cycles p1 p2@: quickly test if @p1@ and @p2@ are the same.
pequal :: Ord a => Time -> Pattern a -> Pattern a -> Bool
pequal cycles p1 p2 = (sort $ arc p1 (0, cycles)) == (sort $ arc p2 (0, cycles))
-}

{- | @rot n p@ "rotates" the values in a pattern @p@ by @n@ beats to the left,
preserving its structure. For example, in the following, each value will shift
to its neighbour's position one step to the left, so that @b@ takes the place of
@a@, @a@ of @c@, and @c@ of @b@:

> rot 1 "a ~ b c"

The result is equivalent of:

> "b ~ c a"

The first parameter is the number of steps, and may be given as a pattern. For example, in

> d1 $ rot "<0 0 1 3>" $ n "0 ~ 1 2 0 2 ~ 3*2" # sound "drum"

the pattern will not be rotated for the first two cycles, but will rotate it
by one the third cycle, and by three the fourth cycle.

Additional example:

> d1 $ every 4 (rot 2) $ slow 2 $ sound "bd hh hh hh"
-}
rot :: Ord a => Pattern Int -> Pattern a -> Pattern a
rot = tParam _rot

-- | Calculates a whole cycle, rotates it, then constrains events to the original query arc.
_rot :: Ord a => Int -> Pattern a -> Pattern a
_rot i pat = splitQueries $ pat {query = \st -> f st (query pat (st {arc = wholeCycle (arc st)}))}
  where -- TODO maybe events with the same arc (part+whole) should be
        -- grouped together in the rotation?
        f st es = constrainEvents (arc st) $ shiftValues $ sort $ defragParts es
        shiftValues es | i >= 0 =
                         zipWith (\e s -> e {value = s}) es
                         (drop i $ cycle $ map value es)
                       | otherwise =
                         zipWith (\e s -> e{value = s}) es
                         (drop (length es - abs i) $ cycle $ map value es)
        wholeCycle (Arc s _) = Arc (sam s) (nextSam s)
        constrainEvents :: Arc -> [Event a] -> [Event a]
        constrainEvents a es = mapMaybe (constrainEvent a) es
        constrainEvent :: Arc -> Event a -> Maybe (Event a)
        constrainEvent a e =
          do
            p' <- subArc (part e) a
            return e {part = p'}

{-| @segment n p@ ’samples’ the pattern @p@ at a rate of @n@ events per cycle.
Useful for turning a continuous pattern into a discrete one.

In the following example, the pattern originates from the shape of a sine
wave, a continuous pattern. Without @segment@, the samples will get triggered
at an undefined frequency which may be very high.

> d1 $ n (slow 2 $ segment 16 $ range 0 32 $ sine) # sound "amencutup"
-}
segment :: Pattern Time -> Pattern a -> Pattern a
segment = tParam _segment

_segment :: Time -> Pattern a -> Pattern a
_segment n p = _fast n (pure id) <* p

-- | @discretise@: the old (deprecated) name for 'segment'
discretise :: Pattern Time -> Pattern a -> Pattern a
discretise = segment

-- @fromNote p@: converts a pattern of human-readable pitch names
-- into pitch numbers. For example, @"cs2"@ will be parsed as C Sharp
-- in the 2nd octave with the result of @11@, and @"b-3"@ as
-- @-25@. Pitches can be decorated using:
--
--    * s = Sharp, a half-step above (@"gs-1"@)
--    * f = Flat, a half-step below (@"gf-1"@)
--    * n = Natural, no decoration (@"g-1" and "gn-1"@ are equivalent)
--    * ss = Double sharp, a whole step above (@"gss-1"@)
--    * ff = Double flat, a whole step below (@"gff-1"@)
--
-- Note that TidalCycles now assumes that middle C is represented by
-- the value 0, rather than the previous value of 60. This function
-- is similar to previously available functions @tom@ and @toMIDI@,
-- but the default octave is now 0 rather than 5.
{-

definition moved to Parse.hs ..

toMIDI :: Pattern String -> Pattern Int
toMIDI p = fromJust <$> (filterValues (isJust) (noteLookup <$> p))
  where
    noteLookup :: String -> Maybe Int
    noteLookup [] = Nothing
    noteLookup s | not (last s `elem` ['0' .. '9']) = noteLookup (s ++ "0")
                 | not (isLetter (s !! 1)) = noteLookup((head s):'n':(tail s))
                 | otherwise = parse s
    parse x = (\a b c -> a+b+c) <$> pc x <*> sym x <*> Just(12*digitToInt (last x))
    pc x = lookup (head x) [('c',0),('d',2),('e',4),('f',5),('g',7),('a',9),('b',11)]
    sym x = lookup (init (tail x)) [("s",1),("f",-1),("n",0),("ss",2),("ff",-2)]
-}

-- @tom p@: Alias for @toMIDI@.
-- tom = toMIDI


{- | The `fit` function takes a pattern of integer numbers, which are used to select values from the given list. What makes this a bit strange is that only a given number of values are selected each cycle. For example:

> d1 $ sound (fit 3 ["bd", "sn", "arpy", "arpy:1", "casio"] "0 [~ 1] 2 1")

The above fits three samples into the pattern, i.e. for the first cycle this
will be @"bd"@, @"sn"@ and @"arpy"@, giving the result @"bd [~ sn] arpy sn"@
(note that we start counting at zero, so that 0 picks the first value). The
following cycle the /next/ three values in the list will be picked, i.e.
@"arpy:1"@, @"casio"@ and @"bd"@, giving the pattern
@"arpy:1 [~ casio] bd casio"@ (note that the list wraps round here).

-}
fit :: Pattern Int -> [a] -> Pattern Int -> Pattern a
fit pint xs p = (tParam func) pint (xs,p)
  where func i (xs',p') = _fit i xs' p'

_fit :: Int -> [a] -> Pattern Int -> Pattern a
_fit perCycle xs p = (xs !!!) <$> (p {query = map (\e -> fmap (+ pos e) e) . query p})
  where pos e = perCycle * floor (start $ part e)


permstep :: RealFrac b => Int -> [a] -> Pattern b -> Pattern a
permstep nSteps things p = unwrap $ (\n -> fastFromList $ concatMap (\x -> replicate (fst x) (snd x)) $ zip (ps !! floor (n * fromIntegral (length ps - 1))) things) <$> _segment 1 p
      where ps = permsort (length things) nSteps
            deviance avg xs = sum $ map (abs . (avg-) . fromIntegral) xs
            permsort n total = map fst $ sortOn snd $ map (\x -> (x,deviance (fromIntegral total / (fromIntegral n :: Double)) x)) $ perms n total
            perms 0 _ = []
            perms 1 n = [[n]]
            perms n total = concatMap (\x -> map (x:) $ perms (n-1) (total-x)) [1 .. (total-(n-1))]

{-|
  @struct a b@ structures pattern @b@ in terms of the pattern of boolean
  values @a@. Only @True@ values in the boolean pattern are used.

  The following are equivalent:

  > d1 $ struct ("t ~ t*2 ~") $ sound "cp"
  > d1 $ sound "cp ~ cp*2 ~"

  The structure comes from a boolean pattern, i.e. a binary pattern containing
  true or false values. Above we only used true values, denoted by @t@. It’s also
  possible to include false values with @f@, which @struct@ will simply treat as
  silence. For example, this would have the same outcome as the above:

  > d1 $ struct ("t f t*2 f") $ sound "cp"

  These true / false binary patterns become useful when you conditionally
  manipulate them, for example, ‘inverting’ the values using 'every' and 'inv':

  > d1 $ struct (every 3 inv "t f t*2 f") $ sound "cp"

  In the above, the boolean values will be ‘inverted’ every third cycle, so that
  the structure comes from the @f@s rather than @t@. Note that euclidean patterns
  also create true/false values, for example:

  > d1 $ struct (every 3 inv "t(3,8)") $ sound "cp"

  In the above, the euclidean pattern creates @"t f t f t f f t"@ which gets
  inverted to @"f t f t f t t f"@ every third cycle. Note that if you prefer you
  can use 1 and 0 instead of @t@ and @f@.
-}
struct :: Pattern Bool -> Pattern a -> Pattern a
struct ps pv = filterJust $ (\a b -> if a then Just b else Nothing ) <$> ps <* pv

-- | @substruct a b@: similar to @struct@, but each event in pattern @a@ gets replaced with pattern @b@, compressed to fit the timespan of the event.
substruct :: Pattern Bool -> Pattern b -> Pattern b
substruct s p = p {query = f}
  where f st =
          concatMap ((\a' -> queryArc (compressArcTo a' p) a') . wholeOrPart) $ filter value $ query s st

randArcs :: Int -> Pattern [Arc]
randArcs n =
  do rs <- mapM (\x -> pure (toRational x / toRational n) <~ choose [1 :: Int,2,3]) [0 .. (n-1)]
     let rats = map toRational rs
         total = sum rats
         pairs = pairUp $ accumulate $ map (/total) rats
     return pairs
       where pairUp [] = []
             pairUp xs = Arc 0 (head xs) : pairUp' xs
             pairUp' []       = []
             pairUp' [_]      = []
             pairUp' [a, _]   = [Arc a 1]
             pairUp' (a:b:xs) = Arc a b: pairUp' (b:xs)


-- TODO - what does this do? Something for @stripe@ ..
randStruct :: Int -> Pattern Int
randStruct n = splitQueries $ Pattern f Nothing Nothing
  where f st = map (\(a,b,c) -> Event (Context []) (Just a) (fromJust b) c) $ filter (\(_,x,_) -> isJust x) as
          where as = map (\(i, Arc s' e') ->
                    (Arc (s' + sam s) (e' + sam s),
                       subArc (Arc s e) (Arc (s' + sam s) (e' + sam s)), i)) $
                      enumerate $ value $ head $
                      queryArc (randArcs n) (Arc (sam s) (nextSam s))
                (Arc s e) = arc st

-- TODO - what does this do?
substruct' :: Pattern Int -> Pattern a -> Pattern a
substruct' s p = p {query = \st -> concatMap (f st) (query s st)}
  where f st (Event c (Just a') _ i) = map (\e -> e {context = combineContexts [c, context e]}) $ queryArc (compressArcTo a' (inside (pure $ 1/toRational(length (queryArc s (Arc (sam (start $ arc st)) (nextSam (start $ arc st)))))) (rotR (toRational i)) p)) a'
        -- Ignore analog events (ones without wholes)
        f _ _ = []

{- | @stripe n p@: repeats pattern @p@ @n@ times per cycle, i.e., the first
parameter gives the number of cycles to operate over. So, it is similar to
@fast@, but with random durations. For example @stripe 2@ will repeat a pattern
twice, over two cycles

In the following example, the start of every third repetition of the @d1@
pattern will match with the clap on the @d2@ pattern.

> d1 $ stripe 3 $ sound "bd sd ~ [mt ht]"
> d2 $ sound "cp"

The repetitions will be contiguous (touching, but not overlapping) and the
durations will add up to a single cycle. @n@ can be supplied as a pattern of
integers.
-}
stripe :: Pattern Int -> Pattern a -> Pattern a
stripe = tParam _stripe

_stripe :: Int -> Pattern a -> Pattern a
_stripe = substruct' . randStruct

-- | @slowstripe n p@ is the same as @stripe@, but the result is also
-- @n@ times slower, so that the mean average duration of the stripes
-- is exactly one cycle, and every @n@th stripe starts on a cycle
-- boundary (in Indian classical terms, the /sam/).
slowstripe :: Pattern Int -> Pattern a -> Pattern a
slowstripe n = slow (toRational <$> n) . stripe n

-- Lindenmayer patterns, these go well with the step sequencer
-- general rule parser (strings map to strings)
parseLMRule :: String -> [(String,String)]
parseLMRule s = map (splitOn ':') commaSplit
  where splitOn sep str = splitAt (fromJust $ elemIndex sep str)
                            $ filter (/= sep) str
        commaSplit = map T.unpack $ T.splitOn (T.pack ",") $ T.pack s

-- specific parser for step sequencer (chars map to string)
-- ruleset in form "a:b,b:ab"
parseLMRule' :: String -> [(Char, String)]
parseLMRule' str = map fixer $ parseLMRule str
  where fixer (c,r) = (head c, r)

{- | Returns the @n@th iteration of a
  [Lindenmayer System](https://en.wikipedia.org/wiki/L-system)
  with given start sequence.

  It takes an integer @b@, a Lindenmayer system rule set, and an initiating
  string as input in order to generate an L-system tree string of @b@ iterations.
  It can be used in conjunction with a step function to convert the generated
  string into a playable pattern. For example,

  > d1 $ slow 16
  >    $ sound
  >    $ step' ["feel:0", "sn:1", "bd:0"]
  >        ( take 512
  >        $ lindenmayer 5 "0:1~~~,1:0~~~2~~~~~0~~~2~,2:2~1~,~:~~1~" "0"
  >        )

  generates an L-system with initiating string @"0"@ and maps it onto a list
  of samples.

  Complex L-system trees with many rules and iterations can sometimes result in unwieldy strings. Using @take n@ to only use the first @n@ elements of the string, along with a 'slow' function, can make the generated values more manageable.

-}
lindenmayer :: Int -> String -> String -> String
lindenmayer _ _ [] = []
lindenmayer 1 r (c:cs) = fromMaybe [c] (lookup c $ parseLMRule' r)
                         ++ lindenmayer 1 r cs
lindenmayer n r s = iterate (lindenmayer 1 r) s !! n

{- | @lindenmayerI@ converts the resulting string into a a list of integers
with @fromIntegral@ applied (so they can be used seamlessly where floats or
rationals are required) -}
lindenmayerI :: Num b => Int -> String -> String -> [b]
lindenmayerI n r s = fmap (fromIntegral . digitToInt) $ lindenmayer n r s

{- | @runMarkov n tmat xi seed@ generates a Markov chain (as a list) of length @n@
using the transition matrix @tmat@ starting from initial state @xi@, starting
with random numbers generated from @seed@
Each entry in the chain is the index of state (starting from zero).
Each row of the matrix will be automatically normalized. For example:
@
runMarkov 8 [[2,3], [1,3]] 0 0
@
will produce a two-state chain 8 steps long, from initial state @0@, where the
transition probability from state 0->0 is 2/5, 0->1 is 3/5, 1->0 is 1/4, and
1->1 is 3/4.  -}
runMarkov :: Int -> [[Double]] -> Int -> Time -> [Int]
runMarkov n tp xi seed = reverse $ (iterate (markovStep $ renorm) [xi])!! (n-1) where
  markovStep tp' xs = (fromJust $ findIndex (r <=) $ scanl1 (+) (tp'!!(head xs))) : xs where
    r = timeToRand $ seed + (fromIntegral . length) xs / fromIntegral n
  renorm = [ map (/ sum x) x | x <- tp ]

{- | @markovPat n xi tp@ generates a one-cycle pattern of @n@ steps in a Markov
chain starting from state @xi@ with transition matrix @tp@. Each row of the
transition matrix is automatically normalized.  For example:

>>> markovPat 8 1 [[3,5,2], [4,4,2], [0,1,0]]
(0>⅛)|1
(⅛>¼)|2
(¼>⅜)|1
(⅜>½)|1
(½>⅝)|2
(⅝>¾)|1
(¾>⅞)|1
(⅞>1)|0
-}
markovPat :: Pattern Int -> Pattern Int -> [[Double]] -> Pattern Int
markovPat = tParam2 _markovPat

_markovPat :: Int -> Int -> [[Double]] -> Pattern Int
_markovPat n xi tp = splitQueries $ pattern (\(State a@(Arc s _) _) ->
  queryArc (listToPat $ runMarkov n tp xi (sam s)) a)

{-|
@mask@ takes a boolean pattern and ‘masks’ another pattern with it. That is,
events are only carried over if they match within a ‘true’ event in the binary
pattern, i.e., it removes events from the second pattern that don't start during
an event from the first.

For example, consider this kind of messy rhythm without any rests.

> d1 $ sound (slowcat ["sn*8", "[cp*4 bd*4, hc*5]"]) # n (run 8)

If we apply a mask to it

@
d1 $ s ( mask ("1 1 1 ~ 1 1 ~ 1" :: Pattern Bool)
         ( slowcat ["sn*8", "[cp*4 bd*4, bass*5]"] )
       )
  # n (run 8)
@

Due to the use of `slowcat` here, the same mask is first applied to @"sn*8"@ and
in the next cycle to @"[cp*4 bd*4, hc*5]"@.

You could achieve the same effect by adding rests within the `slowcat` patterns,
but mask allows you to do this more easily. It kind of keeps the rhythmic
structure and you can change the used samples independently, e.g.,

@
d1 $ s ( mask ("1 ~ 1 ~ 1 1 ~ 1")
         ( slowcat ["can*8", "[cp*4 sn*4, jvbass*16]"] )
       )
  # n (run 8)
@
-}
mask :: Pattern Bool -> Pattern a -> Pattern a
mask b p = const <$> p <* (filterValues id b)

-- TODO: refactor towards union
enclosingArc :: [Arc] -> Arc
enclosingArc [] = Arc 0 1
enclosingArc as = Arc (minimum (map start as)) (maximum (map stop as))

{-|
  @stretch@ takes a pattern, and if there’s silences at the start or end of the
  current cycle, it will zoom in to avoid them. The following are equivalent:

  > d1 $ note (stretch "~ 0 1 5 8*4 ~") # s "superpiano"
  > d1 $ note "0 1 5 8*4" # s "superpiano"

  You can pattern silences on the extremes of a cycle to make changes to the rhythm:

  > d1 $ note (stretch "~ <0 ~> 1 5 8*4 ~") # s "superpiano"
-}
stretch :: Pattern a -> Pattern a
-- TODO - should that be whole or part?
stretch p = splitQueries $ p {query = q, pureValue = Nothing}
  where q st = query (zoomArc (cycleArc $ enclosingArc $ map wholeOrPart $ query p (st {arc = Arc (sam s) (nextSam s)})) p) st
          where s = start $ arc st

{- | @fit'@ is a generalization of `fit`, where the list is instead constructed
by using another integer pattern to slice up a given pattern. The first argument
is the number of cycles of that latter pattern to use when slicing. It's easier
to understand this with a few examples:

> d1 $ sound (fit' 1 2 "0 1" "1 0" "bd sn")

So what does this do? The first @1@ just tells it to slice up a single cycle of
@"bd sn"@. The @2@ tells it to select two values each cycle, just like the first
argument to @fit@. The next pattern @"0 1"@ is the "from" pattern which tells
it how to slice, which in this case means @"0"@ maps to @"bd"@, and @"1"@ maps
to @"sn"@. The next pattern @"1 0"@ is the "to" pattern, which tells it how to
rearrange those slices. So the final result is the pattern @"sn bd"@.

A more useful example might be something like

> d1 $ fit' 1 4 (run 4) "[0 3*2 2 1 0 3*2 2 [1*8 ~]]/2"
>    $ chop 4
>    $ (sound "breaks152" # unit "c")

which uses @chop@ to break a single sample into individual pieces, which @fit'@ then puts into a list (using the @run 4@ pattern) and reassembles according to the complicated integer pattern.
-}
fit' :: Pattern Time -> Int -> Pattern Int -> Pattern Int -> Pattern a -> Pattern a
fit' cyc n from to p = squeezeJoin $ _fit n mapMasks to
  where mapMasks = [stretch $ mask (const True <$> filterValues (== i) from') p'
                     | i <- [0..n-1]]
        p' = density cyc p
        from' = density cyc from

{-|
  Treats the given pattern @p@ as having @n@ chunks, and applies the function @f@ to one of those sections per cycle.
  Running:
   - from left to right if chunk number is positive
   - from right to left if chunk number is negative

  > d1 $ chunk 4 (fast 4) $ sound "cp sn arpy [mt lt]"

  The following:

  > d1 $ chunk 4 (# speed 2) $ sound "bd hh sn cp"

  applies @(# speed 2)@ to the uppercased part of the cycle below:

  > BD hh sn cp
  > bd HH sn cp
  > bd hh SN cp
  > bd hh sn CP
-}
chunk :: Pattern Int -> (Pattern b -> Pattern b) -> Pattern b -> Pattern b
chunk npat f p = innerJoin $ (\n -> _chunk n f p) <$> npat

_chunk :: Integral a => a -> (Pattern b -> Pattern b) -> Pattern b -> Pattern b
_chunk n f p | n >= 0 = cat [withinArc (Arc (i % fromIntegral n) ((i+1) % fromIntegral n)) f p | i <- [0 .. fromIntegral n - 1]]
             | otherwise = do i <- _slow (toRational (-n)) $ rev $ run (fromIntegral (-n))
                              withinArc (Arc (i % fromIntegral (-n)) ((i+1) % fromIntegral (-n))) f p

-- | DEPRECATED, use 'chunk' with negative numbers instead
chunk' :: Integral a1 => Pattern a1 -> (Pattern a2 -> Pattern a2) -> Pattern a2 -> Pattern a2
chunk' npat f p = innerJoin $ (\n -> _chunk' n f p) <$> npat

-- | DEPRECATED, use '_chunk' with negative numbers instead
_chunk' :: Integral a => a -> (Pattern b -> Pattern b) -> Pattern b -> Pattern b
_chunk' n f p = _chunk (-n) f p

{-|
@inside@ carries out an operation /inside/ a cycle.
For example, while @rev "0 1 2 3 4 5 6 7"@ is the same as @"7 6 5 4 3 2 1 0"@,
@inside 2 rev "0 1 2 3 4 5 6 7"@ gives @"3 2 1 0 7 6 5 4"@.

What this function is really doing is ‘slowing down’ the pattern by a given
factor, applying the given function to it, and then ‘speeding it up’ by the same
factor. In other words, this:

> inside 2 rev "0 1 2 3 4 5 6 7"

Is doing this:

> fast 2 $ rev $ slow 2 "0 1 2 3 4 5 6 7"

so rather than whole cycles, each half of a cycle is reversed.
-}
inside :: Pattern Time -> (Pattern a1 -> Pattern a) -> Pattern a1 -> Pattern a
inside np f p = innerJoin $ (\n -> _inside n f p) <$> np

_inside :: Time -> (Pattern a1 -> Pattern a) -> Pattern a1 -> Pattern a
_inside n f p = _fast n $ f (_slow n p)

{-|
@outside@ is the inverse of the 'inside' function. @outside@ applies its function /outside/ the cycle.
Say you have a pattern that takes 4 cycles to repeat and apply the rev function:

> d1 $ rev $ cat [s "bd bd sn",s "sn sn bd", s"lt lt sd", s "sd sd bd"]

The above generates:

> d1 $ rev $ cat [s "sn bd bd",s "bd sn sn", s "sd lt lt", s "bd sd sd"]

However if you apply @outside@:

> d1 $ outside 4 (rev) $ cat [s "bd bd sn",s "sn sn bd", s"lt lt sd", s "sd sd bd"]

The result is:

> d1 $ rev $ cat [s "bd sd sd", s "sd lt lt", s "sn sn bd", s "bd bd sn"]

Notice that the whole idea has been reversed. What this function is really doing
is ‘speeding up’ the pattern by a given factor, applying the given function to
it, and then ‘slowing it down’ by the same factor. In other words, this:

> d1 $ slow 4 $ rev $ fast 4
>    $ cat [s "bd bd sn",s "sn sn bd", s"lt lt sd", s "sd sd bd"]

This compresses the idea into a single cycle before rev operates and then slows it back to the original speed.
-}
outside :: Pattern Time -> (Pattern a1 -> Pattern a) -> Pattern a1 -> Pattern a
outside np f p = innerJoin $ (\n -> _outside n f p) <$> np

_outside :: Time -> (Pattern a1 -> Pattern a) -> Pattern a1 -> Pattern a
_outside n = _inside (1/n)

{-|
  Takes a pattern and loops only the first cycle of the pattern. For example, the following code will only play the bass drum sample:

  > d1 $ loopFirst $ s "<<bd*4 ht*8> cp*4>"

  This function combines with 'sometimes' to insert events from the first cycle randomly into subsequent cycles of the pattern:

  > d1 $ sometimes loopFirst $ s "<<bd*4 ht*8> cp*4>"
-}
loopFirst :: Pattern a -> Pattern a
loopFirst p = splitQueries $ p {query = f}
  where f st = map
          (\(Event c w p' v) ->
             Event c (plus <$> w) (plus p') v) $
          query p (st {arc = minus $ arc st})
          where minus = fmap (subtract (sam s))
                plus = fmap (+ sam s)
                s = start $ arc st

timeLoop :: Pattern Time -> Pattern a -> Pattern a
timeLoop n = outside n loopFirst

{-|
  @seqPLoop@ will keep looping the sequence when it gets to the end:

  > d1 $ qtrigger $ seqPLoop
  >   [ (0, 12, sound "bd bd*2")
  >   , (4, 12, sound "hh*2 [sn cp] cp future*4")
  >   , (8, 12, sound (samples "arpy*8" (run 16)))
  >   ]
-}
seqPLoop :: [(Time, Time, Pattern a)] -> Pattern a
seqPLoop ps = timeLoop (pure $ maxT - minT) $ minT `rotL` seqP ps
  where minT = minimum $ map (\(x,_,_) -> x) ps
        maxT = maximum $ map (\(_,x,_) -> x) ps

{-|
@toScale@ lets you turn a pattern of notes within a scale (expressed as a
list) to note numbers.

For example:

> toScale [0, 4, 7] "0 1 2 3"

will turn into the pattern @"0 4 7 12"@.

@toScale@ is handy for quickly applying a scale without naming it:

> d1 $ n (toScale [0,2,3,5,7,8,10] "0 1 2 3 4 5 6 7") # sound "superpiano"

This function assumes your scale fits within an octave; if that's not true,
use 'toScale''.

@toScale = toScale' 12@
-}
toScale :: Num a => [a] -> Pattern Int -> Pattern a
toScale = toScale' 12

{- | As 'toScale', though allowing scales of arbitrary size.

An example: @toScale' 24 [0,4,7,10,14,17] (run 8)@ turns into @"0 4 7 10 14 17 24 28"@.
-}
toScale' :: Num a => Int -> [a] -> Pattern Int -> Pattern a
toScale' _ [] = const silence
toScale' o s = fmap noteInScale
  where octave x = x `div` length s
        noteInScale x = (s !!! x) + fromIntegral (o * octave x)


{- | @swingBy x n@ divides a cycle into @n@ slices and delays the notes in the
  second half of each slice by @x@ fraction of a slice. So if @x@ is 0 it does
  nothing, 0.5 delays for half the note duration, and 1 will wrap around to
  doing nothing again. The end result is a shuffle or swing-like rhythm. For
  example, the following will delay every other @"hh"@ 1/3 of the way to the
  next @"hh"@:

  > d1 $ swingBy (1/3) 4 $ sound "hh*8"
-}
swingBy :: Pattern Time -> Pattern Time -> Pattern a -> Pattern a
swingBy x n = inside n (withinArc (Arc 0.5 1) (x ~>))

{-|
As 'swingBy', with the cycle division set to ⅓.
-}
swing :: Pattern Time -> Pattern a -> Pattern a
swing = swingBy (pure $ 1%3)

{- | @cycleChoose@ is like `choose` but only picks a new item from the list
  once each cycle.

  > d1 $ sound "drum ~ drum drum" # n (cycleChoose [0,2,3])
-}
cycleChoose :: [a] -> Pattern a
cycleChoose = segment 1 . choose

{- | Internal function used by shuffle and scramble -}
_rearrangeWith :: Pattern Int -> Int -> Pattern a -> Pattern a
_rearrangeWith ipat n pat = innerJoin $ (\i -> _fast nT $ _repeatCycles n $ pats !! i) <$> ipat
  where
    pats = map (\i -> zoom (fromIntegral i / nT, fromIntegral (i+1) / nT) pat) [0 .. (n-1)]
    nT :: Time
    nT = fromIntegral n

{- | @shuffle n p@ evenly divides one cycle of the pattern @p@ into @n@ parts,
and returns a random permutation of the parts each cycle.  For example,
@shuffle 3 "a b c"@ could return @"a b c"@, @"a c b"@, @"b a c"@, @"b c a"@,
@"c a b"@, or @"c b a"@.  But it will /never/ return @"a a a"@, because that
is not a permutation of the parts.

This could also be called “sampling without replacement”.
-}
shuffle :: Pattern Int -> Pattern a -> Pattern a
shuffle = tParam _shuffle

_shuffle :: Int -> Pattern a -> Pattern a
_shuffle n = _rearrangeWith (randrun n) n

{- | @scramble n p@ is like 'shuffle' but randomly selects from the parts
of @p@ instead of making permutations.
For example, @scramble 3 "a b c"@ will randomly select 3 parts from
@"a"@ @"b"@ and @"c"@, possibly repeating a single part.

This could also be called “sampling with replacement”.
-}
scramble :: Pattern Int -> Pattern a -> Pattern a
scramble = tParam _scramble

_scramble :: Int -> Pattern a -> Pattern a
_scramble n = _rearrangeWith (_segment (fromIntegral n) $ _irand n) n

{-|
@randrun n@ generates a pattern of random integers less than @n@.

The following plays random notes in an octave:

@
d1 $ s "superhammond!12" # n (fromIntegral <$> randrun 13)
@

-}
randrun :: Int -> Pattern Int
randrun 0 = silence
randrun n' =
  splitQueries $ pattern (\(State a@(Arc s _) _) -> events a $ sam s)
  where events a seed = mapMaybe toEv $ zip arcs shuffled
          where shuffled = map snd $ sortOn fst $ zip rs [0 .. (n'-1)]
                rs = timeToRands seed n' :: [Double]
                arcs = zipWith Arc fractions (tail fractions)
                fractions = map (+ (sam $ start a)) [0, 1 / fromIntegral n' .. 1]
                toEv (a',v) = do a'' <- subArc a a'
                                 return $ Event (Context []) (Just a') a'' v

-- ** Composing patterns

{- | The function @seqP@ allows you to define when
a sound within a list starts and ends. The code below contains three
separate patterns in a `stack`, but each has different start times
(zero cycles, eight cycles, and sixteen cycles, respectively). All
patterns stop after 128 cycles:

@
d1 $ seqP [
  (0, 128, sound "bd bd*2"),
  (8, 128, sound "hh*2 [sn cp] cp future*4"),
  (16, 128, sound (samples "arpy*8" (run 16)))
]
@
-}
seqP :: [(Time, Time, Pattern a)] -> Pattern a
seqP ps = stack $ map (\(s, e, p) -> playFor s e (sam s `rotR` p)) ps

{-|
The @ur@ function is designed for longer form composition, by allowing you to
create ‘patterns of patterns’ in a repeating loop. It takes four parameters:
how long the loop will take, a pattern giving the structure of the composition,
a lookup table for named patterns to feed into that structure, and a second
lookup table for named transformations\/effects.

The /ur-/ prefix [comes from German](https://en.wiktionary.org/wiki/ur-#German) and
means /proto-/ or /original/. For a mnemonic device, think of this function as
assembling a set of original patterns (ur-patterns) into a larger, newer whole.

Lets say you had three patterns (called @a@, @b@ and @c@), and that you wanted
to play them four cycles each, over twelve cycles in total. Here is one way to
do it:

@
let pats =
  [ ( "a", stack [ n "c4 c5 g4 f4 f5 g4 e5 g4" # s "superpiano" # gain "0.7"
                 , n "[c3,g4,c4]" # s "superpiano"# gain "0.7"
                 ]
    )
  , ( "b", stack [ n "d4 c5 g4 f4 f5 g4 e5 g4" # s "superpiano" # gain "0.7"
                 , n "[d3,a4,d4]" # s "superpiano"# gain "0.7"
                 ]
    )
  , ( "c", stack [ n "f4 c5 g4 f4 f5 g4 e5 g4" # s "superpiano" # gain "0.7"
                 , n "[f4,c5,f4]" # s "superpiano"# gain "0.7"
                 ]
    )
  ]
in
d1 $ ur 12 "a b c" pats []
@

In the above, the fourth parameter is given as an empty list, but that is where
you can put another lookup table, of functions rather than patterns this time.
For example:

@
let
  pats = ...
  fx   = [ ("reverb", ( # (room 0.8 # sz 0.99 # orbit 1)))
         , ("faster", fast 2)
         ]
in
d1 $ ur 12 "a b:reverb c:faster" pats fx
@

In this example, @b@ has the function applied that’s named as reverb, while @c@
is made to go faster. It’s also possible to schedule multiple patterns at once,
like in the following:

@
let pats = [ ("drums", s "drum cp*2")
           , ("melody", s "arpy:2 arpy:3 arpy:5")
           , ("craziness", s "cp:4*8" # speed ( sine + 0.5 ))
           ]
    fx = [("higher", ( # speed 2))]
in
d1 $ ur 8 "[drums, melody] [drums,craziness,melody] melody:higher" pats fx
@
-}
ur :: Time -> Pattern String -> [(String, Pattern a)] -> [(String, Pattern a -> Pattern a)] -> Pattern a
ur t outer_p ps fs = _slow t $ unwrap $ adjust <$> timedValues (getPat . split <$> outer_p)
  where split = wordsBy (==':')
        getPat (s:xs) = (match s, transform xs)
        -- TODO - check this really can't happen..
        getPat _      = error "can't happen?"
        match s = fromMaybe silence $ lookup s ps'
        ps' = map (fmap (_fast t)) ps
        adjust (a, (p, f)) = f a p
        transform (x:_) a = transform' x a
        transform _ _     = id
        transform' str (Arc s e) p = s `rotR` inside (pure $ 1/(e-s)) (matchF str) p
        matchF str = fromMaybe id $ lookup str fs
        timedValues = filterJust . withEvent (\(Event c ma a' v) -> Event c ma a' (ma >>= \a -> Just (a,v))
                                             ) . filterDigital

{- | A simpler version of 'ur' that just provides name-value bindings that are
  reflected in the provided pattern.

  @inhabit@ allows you to link patterns to some @String@, or in other words,
  to give patterns a name and then call them from within another pattern of
  @String@s.

  For example, we can make our own bassdrum, hi-hat and snaredrum kit:

  > do
  >   let drum = inhabit [ ("bd", s "sine" |- accelerate 1.5)
  >                      , ("hh", s "alphabet:7" # begin 0.7 # hpf 7000)
  >                      , ("sd", s "invaders:3" # speed 12)
  >                      ]
  >   d1 $ drum "[bd*8?, [~hh]*4, sd(6,16)]"

  @inhabit@ can be very useful when using MIDI controlled drum machines, since you
  can give understandable drum names to patterns of notes.
-}
inhabit :: [(String, Pattern a)] -> Pattern String -> Pattern a
inhabit ps p = squeezeJoin $ (\s -> fromMaybe silence $ lookup s ps) <$> p

{- | @spaceOut xs p@ repeats a 'Pattern' @p@ at different durations given by the list of time values in @xs@. -}
spaceOut :: [Time] -> Pattern a -> Pattern a
spaceOut xs p = _slow (toRational $ sum xs) $ stack $ map (`compressArc` p) spaceArcs
  where markOut :: Time -> [Time] -> [Arc]
        markOut _ []           = []
        markOut offset (x:xs') = Arc offset (offset+x):markOut (offset+x) xs'
        spaceArcs = map (\(Arc a b) -> Arc (a/s) (b/s)) $ markOut 0 xs
        s = sum xs

{-| @flatpat@ takes a 'Pattern' of lists and pulls the list elements as
  separate 'Event's. For example, the following code uses @flatpat@ in combination with @listToPat@ to create an alternating pattern of chords:

  > d1 $ n (flatpat $ listToPat [[0,4,7],[(-12),(-8),(-5)]])
  >    # s "superpiano" # sustain 2

  This code is equivalent to:

  > d1 $ n ("[0,4,7] [-12,-8,-5]") # s "superpiano" # sustain 2
-}
flatpat :: Pattern [a] -> Pattern a
flatpat p = p {query = concatMap (\(Event c b b' xs) -> map (Event c b b') xs) . query p, pureValue = Nothing}

{- | @layer@ takes a list of 'Pattern'-returning functions and a seed element,
stacking the result of applying the seed element to each function in the list.

It allows you to layer up multiple functions on one pattern. For example, the following
will play two versions of the pattern at the same time, one reversed and one at twice
the speed:

> d1 $ layer [rev, fast 2] $ sound "arpy [~ arpy:4]"

The original version of the pattern can be included by using the @id@ function:

> d1 $ layer [id, rev, fast 2] $ sound "arpy [~ arpy:4]"
-}
layer :: [a -> Pattern b] -> a -> Pattern b
layer fs p = stack $ map ($ p) fs

-- | @arpeggiate@ finds events that share the same timespan, and spreads
-- them out during that timespan, so for example @arpeggiate "[bd,sn]"@
-- gets turned into @"bd sn"@. Useful for creating arpeggios/broken chords.
arpeggiate :: Pattern a -> Pattern a
arpeggiate = arpWith id

-- | Shorthand alias for arpeggiate
arpg :: Pattern a -> Pattern a
arpg = arpeggiate

arpWith :: ([EventF (ArcF Time) a] -> [EventF (ArcF Time) b]) -> Pattern a -> Pattern b
arpWith f p = withEvents munge p
  where munge es = concatMap (spreadOut . f) (groupBy (\a b -> whole a == whole b) $ sortOn whole es)
        spreadOut xs = mapMaybe (\(n, x) -> shiftIt n (length xs) x) $ enumerate xs
        shiftIt n d (Event c (Just (Arc s e)) a' v) =
          do
            a'' <- subArc (Arc newS newE) a'
            return (Event c (Just $ Arc newS newE) a'' v)
          where newS = s + (dur * fromIntegral n)
                newE = newS + dur
                dur = (e - s) / fromIntegral d
        -- TODO ignoring analog events.. Should we just leave them as-is?
        shiftIt _ _ _ = Nothing


{-| The @arp@ function takes an additional pattern of arpeggiate modes. For example:

@
d1 $ sound "superpiano" # n (arp "<up down diverge>" "<a'm9'8 e'7sus4'8>")
@

The different arpeggiate modes are:
@
up down updown downup up&down down&up converge
diverge disconverge pinkyup pinkyupdown
thumbup thumbupdown
@
-}
arp :: Pattern String -> Pattern a -> Pattern a
arp = tParam _arp

_arp :: String -> Pattern a -> Pattern a
_arp name p = arpWith f p
  where f = fromMaybe id $ lookup name arps
        arps :: [(String, [a] -> [a])]
        arps = [("up", id),
                ("down", reverse),
                ("updown", \x -> init x ++ init (reverse x)),
                ("downup", \x -> init (reverse x) ++ init x),
                ("up&down", \x -> x ++ reverse x),
                ("down&up", \x -> reverse x ++ x),
                ("converge", converge),
                ("diverge", reverse . converge),
                ("disconverge", \x -> converge x ++ tail (reverse $ converge x)),
                ("pinkyup", pinkyup),
                ("pinkyupdown", \x -> init (pinkyup x) ++ init (reverse $ pinkyup x)),
                ("thumbup", thumbup),
                ("thumbupdown", \x -> init (thumbup x) ++ init (reverse $ thumbup x))
               ]
        converge []     = []
        converge (x:xs) = x : converge' xs
        converge' [] = []
        converge' xs = last xs : converge (init xs)
        pinkyup xs = concatMap (:[pinky]) $ init xs
          where pinky = last xs
        thumbup xs = concatMap (\x -> [thumb,x]) $ tail xs
          where thumb = head xs

{- | @rolled@ plays each note of a chord quickly in order, as opposed to
simultaneously; to give a chord a harp-like or strum effect.

Notes are played low to high, and are evenly distributed within (1/4) of the chord event length, as opposed to arp/arpeggiate that spread the notes over the whole event.

@
rolled $ n "c'maj'4" # s "superpiano"
@

@rolled = rolledBy (1/4)@
-}
rolled :: Pattern a -> Pattern a
rolled = rolledBy (1/4)

{-
As 'rolled', but allows you to specify the length of the roll, i.e., the
fraction of the event that the notes will be spread over. The value in the
passed pattern is the divisor of the cycle length. A negative value will play
the arpeggio in reverse order.

@
rolledBy "<1 -0.5 0.25 -0.125>" $ note "c'maj9" # s "superpiano"
@
-}
rolledBy :: Pattern (Ratio Integer) -> Pattern a -> Pattern a
rolledBy pt = tParam rolledWith (segment 1 $ pt)

rolledWith :: Ratio Integer -> Pattern a -> Pattern a
rolledWith t = withEvents aux
         where aux es = concatMap (steppityIn) (groupBy (\a b -> whole a == whole b) $ ((isRev t) es))
               isRev b = (\x -> if x > 0 then id else reverse ) b
               steppityIn xs = mapMaybe (\(n, ev) -> (timeguard n xs ev t)) $ enumerate xs
               timeguard _ _ ev 0  = return ev
               timeguard n xs ev _ = (shiftIt n (length xs) ev)
               shiftIt n d (Event c (Just (Arc s e)) a' v) = do
                         a'' <- subArc (Arc newS e) a'
                         return (Event c (Just $ Arc newS e) a'' v)
                      where newS = s + (dur * fromIntegral n)
                            dur = ((e - s)) / ((1/ (abs t))*fromIntegral d)
               shiftIt _ _ ev =  return ev

{- TODO !

-- | @fill@ 'fills in' gaps in one pattern with events from another. For example @fill "bd" "cp ~ cp"@ would result in the equivalent of `"~ bd ~"`. This only finds gaps in a resulting pattern, in other words @"[bd ~, sn]"@ doesn't contain any gaps (because @sn@ covers it all), and @"bd ~ ~ sn"@ only contains a single gap that bridges two steps.
fill :: Pattern a -> Pattern a -> Pattern a
fill p' p = struct (splitQueries $ p {query = q, pureValue = Nothing}) p'
  where
    q st = removeTolerance (s,e) $ invert (s-tolerance, e+tolerance) $ query p (st {arc = (s-tolerance, e+tolerance)})
      where (s,e) = arc st
    invert (s,e) es = map arcToEvent $ foldr remove [(s,e)] (map part es)
    remove (s,e) xs = concatMap (remove' (s, e)) xs
    remove' (s,e) (s',e') | s > s' && e < e' = [(s',s),(e,e')] -- inside
                          | s > s' && s < e' = [(s',s)] -- cut off right
                          | e > s' && e < e' = [(e,e')] -- cut off left
                          | s <= s' && e >= e' = [] -- swallow
                          | otherwise = [(s',e')] -- miss
    arcToEvent a = ((a,a),"x")
    removeTolerance (s,e) es = concatMap (expand) $ map (withPart f) es
      where f a = concatMap (remove' (e,e+tolerance)) $ remove' (s-tolerance,s) a
            expand ((a,xs),c) = map (\x -> ((a,x),c)) xs
    tolerance = 0.01
-}

{- | @ply n@ repeats each event @n@ times within its arc.

For example, the following are equivalent:

@
d1 $ ply 3 $ s "bd ~ sn cp"
d1 $ s "[bd bd bd] ~ [sn sn sn] [cp cp cp]"
@

The first parameter may be given as a pattern, so that the following are equivalent:

@
d1 $ ply "2 3" $ s "bd ~ sn cp"
d1 $ s "[bd bd] ~ [sn sn sn] [cp cp cp]"
@

Here is an example of it being used conditionally:

@
d1 $ every 3 (ply 4) $ s "bd ~ sn cp"
@
-}
ply :: Pattern Rational -> Pattern a -> Pattern a
ply = tParam _ply

_ply :: Rational -> Pattern a -> Pattern a
_ply n pat = squeezeJoin $ (_fast n . pure) <$> pat

-- | As 'ply', but applies a function each time. The applications are compounded.
plyWith :: (Ord t, Num t) => Pattern t -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
plyWith np f p = innerJoin $ (\n -> _plyWith n f p) <$> np

_plyWith :: (Ord t, Num t) => t -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
_plyWith numPat f p = arpeggiate $ compound numPat
  where compound n | n <= 1 = p
                   | otherwise = overlay p (f $ compound $ n-1)

{-| Syncopates a rhythm, shifting (delaying) each event halfway into its arc
  (timespan).

  In mini-notation terms, it basically turns every instance of a into @[~ a]@,
  e.g., @"a b [c d] e"@ becomes the equivalent of
  @"[~ a] [~ b] [[~ c] [~ d]] [~ e]"@.
  Every beat then becomes an offbeat, and so the overall effect is to
  syncopate a pattern.

  In the following example, you can hear that the piano chords play between the
  snare and the bass drum. In 4/4 time, they are playing in the 2 and a half,
  and 4 and a half beats:

  > do
  >   resetCycles
  >   d1 $ stack [
  >     press $ n "~ c'maj ~ c'maj" # s "superpiano" # gain 0.9 # pan 0.6,
  >     s "[bd,clap sd bd sd]" # pan 0.4
  >     ] # cps (90/60/4)

  In the next example, the C major chord plays before the G major. As the slot
  that occupies the C chord is that of one eighth note, it is displaced by press
  only a sixteenth note:

  > do
  >   resetCycles
  >   d1 $ stack [
  >     press $ n "~ [c'maj ~] ~ ~" # s "superpiano" # gain 0.9 # pan 0.6,
  >     press $ n "~ g'maj ~ ~" # s "superpiano" # gain 0.9 # pan 0.4,
  >     s "[bd,clap sd bd sd]"
  >    ] # cps (90/60/4)
-}
press :: Pattern a -> Pattern a
press = _pressBy 0.5

{-| Like @press@, but allows you to specify the amount in which each event is
  shifted as a float from 0 to 1 (exclusive).

  @pressBy 0.5@ is the same as @press@, while @pressBy (1/3)@ shifts each event
  by a third of its arc.

  You can pattern the displacement to create interesting rhythmic effects:

  > d1 $ stack [
  >   s "bd sd bd sd",
  >   pressBy "<0 0.5>" $ s "co:2*4"
  > ]

  > d1 $ stack [
  >   s "[bd,co sd bd sd]",
  >   pressBy "<0 0.25 0.5 0.75>" $ s "cp"
  > ]
-}
pressBy :: Pattern Time -> Pattern a -> Pattern a
pressBy = tParam _pressBy

_pressBy :: Time -> Pattern a -> Pattern a
_pressBy r pat = squeezeJoin $ (compressTo (r,1) . pure) <$> pat

{-
  Uses the first (binary) pattern to switch between the following
  two patterns. The resulting structure comes from the source patterns, not the
  binary pattern. See also `stitch`.

  The following will play the first pattern for the first half of a cycle, and
  the second pattern for the other half; it combines two patterns of strings and
  passes the result to the sound function:

  > d1 $ sound (sew "t f" "bd*8" "cp*8")

  It’s possible to sew together two control patterns:

  > d1 $ sew "t <f t> <f [f t] t>"
  >          (n "0 .. 15" # s "future")
  >          (s "cp:3*16" # speed saw + 1.2)

  You can also use Euclidean rhythm syntax in the boolean sequence:

  > d1 $ sew "t(11,16)"
  >          (n "0 .. 15" # s "future")
  >          (s "cp:3*16" # speed sine + 1.5)
-}
sew :: Pattern Bool -> Pattern a -> Pattern a -> Pattern a
sew pb a b = overlay (mask pb a) (mask (inv pb) b)

{-| Uses the first (binary) pattern to switch between the following
  two patterns. The resulting structure comes from the binary
  pattern, not the source patterns. (In 'sew', by contrast, the resulting structure comes from the source patterns.)

  The following uses a euclidean pattern to control CC0:

  > d1 $ ccv (stitch "t(7,16)" 127 0) # ccn 0  # "midi"
-}
stitch :: Pattern Bool -> Pattern a -> Pattern a -> Pattern a
stitch pb a b = overlay (struct pb a)  (struct (inv pb) b)

-- | A binary pattern is used to conditionally apply a function to a
-- source pattern. The function is applied when a @True@ value is
-- active, and the pattern is let through unchanged when a @False@
-- value is active. No events are let through where no binary values
-- are active.
while :: Pattern Bool -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
while b f pat = sew b (f pat) pat

{-|
@stutter n t pat@ repeats each event in @pat@ @n@ times, separated by @t@ time (in fractions of a cycle).
It is like 'Sound.Tidal.Control.echo' that doesn't reduce the volume, or 'ply' if you controlled the timing.

> d1 $ stutter 4 (1/16) $ s "bd cp"

is functionally equivalent to

> d1 $ stut 4 1 (1/16) $ s "bd cp"
-}
stutter :: Integral i => i -> Time -> Pattern a -> Pattern a
stutter n t p = stack $ map (\i -> (t * fromIntegral i) `rotR` p) [0 .. (n-1)]

{- | The @jux@ function creates strange stereo effects by applying a
  function to a pattern, but only in the right-hand channel. For
  example, the following reverses the pattern on the righthand side:

  > d1 $ slow 32 $ jux (rev) $ striateBy 32 (1/16) $ sound "bev"

  When passing pattern transforms to functions like @jux@ and 'every',
  it's possible to chain multiple transforms together with `.` (function
  composition). For example this both reverses and halves the playback speed of
  the pattern in the righthand channel:

  > d1 $ slow 32 $ jux ((# speed "0.5") . rev) $ striateBy 32 (1/16) $ sound "bev"
-}
jux
  :: (Pattern ValueMap -> Pattern ValueMap)
     -> Pattern ValueMap -> Pattern ValueMap
jux = juxBy 1
juxcut
  :: (Pattern ValueMap -> Pattern ValueMap)
     -> Pattern ValueMap -> Pattern ValueMap
juxcut f p = stack [p     # P.pan (pure 0) # P.cut (pure (-1)),
                    f $ p # P.pan (pure 1) # P.cut (pure (-2))
                   ]

juxcut' :: [t -> Pattern ValueMap] -> t -> Pattern ValueMap
juxcut' fs p = stack $ map (\n -> ((fs !! n) p |+ P.cut (pure $ 1-n)) # P.pan (pure $ fromIntegral n / fromIntegral l)) [0 .. l-1]
  where l = length fs

{- | In addition to `jux`, `jux'` allows using a list of pattern
  transformations. Resulting patterns from each transformation will be spread via
  pan from left to right.

  For example, the following will put @iter 4@ of the pattern to the far left
  and `palindrome` to the far right. In the center, the original pattern will
  play and the chopped and the reversed version will appear mid left and mid
  right respectively.

  > d1 $ jux' [iter 4, chop 16, id, rev, palindrome] $ sound "bd sn"

One could also write:

@
d1 $ stack
      [ iter 4 $ sound "bd sn" # pan "0"
      , chop 16 $ sound "bd sn" # pan "0.25"
      , sound "bd sn" # pan "0.5"
      , rev $ sound "bd sn" # pan "0.75"
      , palindrome $ sound "bd sn" # pan "1"
      ]
@

-}
jux' :: [t -> Pattern ValueMap] -> t -> Pattern ValueMap
jux' fs p = stack $ map (\n -> (fs !! n) p |+ P.pan (pure $ fromIntegral n / fromIntegral l)) [0 .. l-1]
  where l = length fs

-- | Multichannel variant of `jux`, /not sure what it does/
jux4
  :: (Pattern ValueMap -> Pattern ValueMap)
     -> Pattern ValueMap -> Pattern ValueMap
jux4 f p = stack [p # P.pan (pure (5/8)), f $ p # P.pan (pure (1/8))]

{- |
With `jux`, the original and effected versions of the pattern are
panned hard left and right (i.e., panned at 0 and 1). This can be a
bit much, especially when listening on headphones. The variant @juxBy@
has an additional parameter, which brings the channel closer to the
centre. For example:

> d1 $ juxBy 0.5 (fast 2) $ sound "bd sn:1"

In the above, the two versions of the pattern would be panned at 0.25
and 0.75, rather than 0 and 1.
-}
juxBy
  :: Pattern Double
     -> (Pattern ValueMap -> Pattern ValueMap)
     -> Pattern ValueMap
     -> Pattern ValueMap
juxBy n f p = stack [p |+ P.pan 0.5 |- P.pan (n/2), f $ p |+ P.pan 0.5 |+ P.pan (n/2)]

{- |
Given a sample's directory name and number, this generates a string
suitable to pass to 'Data.String.fromString' to create a 'Pattern String'.
'samples' is a 'Pattern'-compatible interface to this function.

@pick name n = name ++ ":" ++ show n@
-}
pick :: String -> Int -> String
pick name n = name ++ ":" ++ show n

{- |
Given a pattern of sample directory names and a of pattern indices
create a pattern of strings corresponding to the sample at each
name-index pair.

An example:

> samples "jvbass [~ latibro] [jvbass [latibro jvbass]]"
>         ((1%2) `rotL` slow 6 "[1 6 8 7 3]")

The type signature is more general here, but you can consider this
to be a function of type @Pattern String -> Pattern Int -> Pattern String@.

@samples = liftA2 pick@
-}
samples :: Applicative f => f String -> f Int -> f String
samples p p' = pick <$> p <*> p'

{- |
Equivalent to 'samples', though the sample specifier pattern
(the @f Int@) will be evaluated first. Not a large difference
in the majority of cases.
-}
samples' :: Applicative f => f String -> f Int -> f String
samples' p p' = flip pick <$> p' <*> p

{-
scrumple :: Time -> Pattern a -> Pattern a -> Pattern a
scrumple o p p' = p'' -- overlay p (o `rotR` p'')
  where p'' = pattern $ \a -> concatMap
                              (\((s,d), vs) -> map (\x -> ((s,d),
                                                           snd x
                                                          )
                                                   )
                                                   (arc p' (s,s))
                              ) (arc p a)
-}

{-
 As 'spread', but specialized so that the list contains functions returning patterns.

@spreadf = 'spread' ($)@
-}
spreadf :: [a -> Pattern b] -> a -> Pattern b
spreadf = spread ($)

stackwith :: Unionable a => Pattern a -> [Pattern a] -> Pattern a
stackwith p ps | null ps = silence
               | otherwise = stack $ map (\(i, p') -> p' # ((fromIntegral i % l) `rotL` p)) (zip [0::Int ..] ps)
  where l = fromIntegral $ length ps

{-
cross f p p' = pattern $ \t -> concat [filter flt $ arc p t,
                                       filter (not . flt) $ arc p' t
                                      ]
]  where flt = f . cyclePos . fst . fst
-}

{- | `range` will take a pattern which goes from 0 to 1 (like `sine`), and range it to a different range - between the first and second arguments. In the below example, `range 1 1.5` shifts the range of `sine1` from 0 - 1 to 1 - 1.5.

> d1 $ jux (iter 4) $ sound "arpy arpy:2*2"
>   |+ speed (slow 4 $ range 1 1.5 sine1)

The above is equivalent to:

> d1 $ jux (iter 4) $ sound "arpy arpy:2*2"
>   |+ speed (slow 4 $ sine1 * 0.5 + 1)
-}
range :: Num a => Pattern a -> Pattern a -> Pattern a -> Pattern a
range fromP toP p = (\from to v -> ((v * (to-from)) + from)) <$> fromP *> toP *> p

_range :: (Functor f, Num b) => b -> b -> f b -> f b
_range from to p = (+ from) . (* (to-from)) <$> p

{- | `rangex` is an exponential version of `range`, good for using with
frequencies. For example, @range 20 2000 "0.5"@ will give @1010@ - halfway
between @20@ and @2000@. But @rangex 20 2000 0.5@ will give @200@ - halfway
between on a logarithmic scale. This usually sounds better if you’re using the
numbers as pitch frequencies. Since rangex uses logarithms, don’t try to scale
things to zero or less.
-}
rangex :: (Functor f, Floating b) => b -> b -> f b -> f b
rangex from to p = exp <$> _range (log from) (log to) p

{-|
  @off@ is similar to 'superimpose', in that it applies a function to a pattern
  and layers up the results on top of the original pattern. The difference
  is that @off@ takes an extra pattern being a time (in cycles) to shift the
  transformed version of the pattern by.

  The following plays a pattern on top of itself, but offset by an eighth of a
  cycle, with a distorting bitcrush effect applied:

  > d1 $ off 0.125 (# crush 2) $ sound "bd [~ sn:2] mt lt*2"

  The following makes arpeggios by adding offset patterns that are shifted up
  the scale:

  > d1 $ slow 2
  >    $ n (off 0.25 (+12)
  >    $ off 0.125 (+7)
  >    $ slow 2 "c(3,8) a(3,8,2) f(3,8) e(3,8,4)")
  >    # sound "superpiano"
-}
off :: Pattern Time -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
off tp f p = innerJoin $ (\tv -> _off tv f p) <$> tp

_off :: Time -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
_off t f p = superimpose (f . (t `rotR`)) p

offadd :: Num a => Pattern Time -> Pattern a -> Pattern a -> Pattern a
offadd tp pn p = off tp (+pn) p

{- |
  @steppify@ acts as a kind of simple step-sequencer using strings. For example,
  @steppify "sn" "x x 12"@ is equivalent to the pattern of strings given by @"sn ~
  sn ~ sn:1 sn:2 ~"@. @steppify@ substitutes the given string for each @x@, for each number
  it substitutes the string followed by a colon and the number, and for everything
  else it puts in a rest.

  In other words, @steppify@ generates a pattern of strings in exactly the syntax you’d want for selecting samples and that can be fed directly into the 's' function.

  > d1 $ s (steppify "sn" "x x 12 ")
-}
steppify :: String -> String -> Pattern String
steppify s cs = fastcat $ map f cs
    where f c | c == 'x' = pure s
              | isDigit c = pure $ s ++ ":" ++ [c]
              | otherwise = silence

{- | @stepifies@ is like @steppify@ but it takes a list of pairs, like steppify would, and
  it plays them all simultaneously.

  > d1 $ s (stepifies [("cp","x  x x  x x  x"),("bd", "xxxx")])
-}
stepifies :: [(String, String)] -> Pattern String
stepifies = stack . map (uncurry steppify)

{- | like `stepify`, but allows you to specify an array of strings to use for @0,1,2...@
  For example,

  > d1 $ s (stepify' ["superpiano","supermandolin"] "0 1 000 1")
  >    # sustain 4 # n 0

  is equivalent to

  > d1 $ s "superpiano ~ supermandolin ~ superpiano!3 ~ supermandolin"
  >    # sustain 4 # n 0
-}
stepify' :: [String] -> String -> Pattern String
stepify' ss cs = fastcat $ map f cs
    where f c | c == 'x' = pure $ head ss
              | isDigit c = pure $ ss !! digitToInt c
              | otherwise = silence


-- | Deprecated backwards-compatible alias for 'ghostWith'.
ghost'' :: Time -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
ghost'' = ghostWith

{-| Like 'ghost'', but a user-supplied function describes how to alter the pattern.

  In this example, ghost notes are applied to the snare hit, but these notes will
  be louder, not quieter, and the sample will have its beginning slightly cut:

  > d1 $ slow 2
  >    $ ghostWith (1/16) ((|*| gain 1.1) . (|> begin 0.05))
  >    $ sound "sn"

-}
ghostWith :: Time -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
ghostWith a f p = superimpose (((a*2.5) `rotR`) . f) $ superimpose (((a*1.5) `rotR`) . f) p

{-
@ghost' t pat@ Adds quieter, pitch-shifted, copies of an event @t@ cycles after events in @pat@, emulating ghost notes that are common in drumming patterns.

The following creates a kick snare pattern with ghost notes applied to the snare hit:

> d1 $ stack [ ghost' 0.125 $ sound "~ sn", sound "bd*2 [~ bd]" ]
-}
ghost' :: Time -> Pattern ValueMap -> Pattern ValueMap
ghost' a p = ghostWith a ((|*| P.gain (pure 0.7)) . (|> P.end (pure 0.2)) . (|*| P.speed (pure 1.25))) p

{-| As 'ghost'', but with the copies set to appear one-eighth of a cycle afterwards.

@ghost = ghost' 0.125@

The following creates a kick snare pattern with ghost notes applied to the snare hit:

> d1 $ stack [ ghost $ sound "~ sn", sound "bd*2 [~ bd]" ]
-}
ghost :: Pattern ValueMap -> Pattern ValueMap
ghost = ghost' 0.125

{- | A more literal weaving than the `weave` function. Given @tabby threads p1 p@,
   parameters representing the threads per cycle and the patterns to weave, and
   this function will weave them together using a plain (aka ’tabby’) weave,
   with a simple over/under structure
 -}
tabby :: Int -> Pattern a -> Pattern a -> Pattern a
tabby nInt p p' = stack [maskedWarp,
                      maskedWeft
                     ]
  where
    n = fromIntegral nInt
    weft = concatMap (const [[0..n-1], reverse [0..n-1]]) [0 .. (n `div` 2) - 1]
    warp = transpose weft
    thread xs p'' = _slow (n%1) $ fastcat $ map (\i -> zoomArc (Arc (i%n) ((i+1)%n)) p'') (concat xs)
    weftP = thread weft p'
    warpP = thread warp p
    maskedWeft = mask (every 2 rev $ _fast (n % 2) $ fastCat [silence, pure True]) weftP
    maskedWarp = mask (every 2 rev $ _fast (n % 2) $ fastCat [pure True, silence]) warpP

-- | Chooses from a list of patterns, using a pattern of floats (from 0 to 1).
select :: Pattern Double -> [Pattern a] -> Pattern a
select = tParam _select

_select :: Double -> [Pattern a] -> Pattern a
_select f ps =  ps !! floor (max 0 (min 1 f) * fromIntegral (length ps - 1))

-- | Chooses from a list of functions, using a pattern of floats (from 0 to 1).
selectF :: Pattern Double -> [Pattern a -> Pattern a] -> Pattern a -> Pattern a
selectF pf ps p = innerJoin $ (\f -> _selectF f ps p) <$> pf

_selectF :: Double -> [Pattern a -> Pattern a] -> Pattern a -> Pattern a
_selectF f ps p =  (ps !! floor (max 0 (min 0.999999 f) * fromIntegral (length ps))) p

-- | Chooses from a list of functions, using a pattern of integers.
pickF :: Pattern Int -> [Pattern a -> Pattern a] -> Pattern a -> Pattern a
pickF pInt fs pat = innerJoin $ (\i -> _pickF i fs pat) <$> pInt

_pickF :: Int -> [Pattern a -> Pattern a] -> Pattern a -> Pattern a
_pickF i fs p =  (fs !!! i) p

{- | @contrast f f' p p'@ splits the control pattern @p'@ in two, applying
  the function @f@ to one and @f'@ to the other. This depends on
  whether events in @p'@ contain values matching with those in @p@.
  For example, in

  > contrast (# crush 3) (# vowel "a") (n "1") $ n "0 1" # s "bd sn" # speed 3

  the first event will have the vowel effect applied and the second will have
  the crush applied.

  @contrast@ is like an if-else-statement over patterns. For @contrast t f p@
  you can think of @t@ as the true branch, @f@ as the false branch, and @p@ as
  the test.

  You can use any control pattern as a test of equality, e.g., @n "<0 1>", speed
  "0.5"@, or things like that. This lets you choose specific properties of the
  pattern you’re transforming for testing, like in the following example,

  > d1 $ contrast (|+ n 12) (|- n 12) (n "c") $ n (run 4) # s "superpiano"

  where every note that isn’t middle-c will be shifted down an octave but
  middle-c will be shifted up to c5.

  Since the test given to contrast is also a pattern, you can do things like have
  it alternate between options:

  > d1 $ contrast (|+ n 12) (|- n 12) (s "<superpiano superchip>")
  >    $ s "superpiano superchip" # n 0

  If you listen to this you’ll hear that which instrument is shifted up and which
  instrument is shifted down alternates between cycles.
-}
contrast :: (ControlPattern -> ControlPattern) -> (ControlPattern -> ControlPattern)
            -> ControlPattern -> ControlPattern -> ControlPattern
contrast = contrastBy (==)

{-|
  @contrastBy@ is contrastBy is the general version of 'contrast', in which you can specify an abritrary boolean function that will be used to compare the control patterns.

  > d2 $ contrastBy (>=) (|+ n 12) (|- n 12) (n "2") $ n "0 1 2 [3 4]" # s "superpiano"
-}
contrastBy :: (a -> Value -> Bool)
              -> (ControlPattern -> Pattern b)
              -> (ControlPattern -> Pattern b)
              -> Pattern (Map.Map String a)
              -> Pattern (Map.Map String Value)
              -> Pattern b
contrastBy comp f f' p p' = overlay (f matched) (f' unmatched)
  where matches = matchManyToOne (flip $ Map.isSubmapOfBy comp) p p'
        matched :: ControlPattern
        matched = filterJust $ (\(t, a) -> if t then Just a else Nothing) <$> matches
        unmatched :: ControlPattern
        unmatched = filterJust $ (\(t, a) -> if not t then Just a else Nothing) <$> matches

contrastRange
  :: (ControlPattern -> Pattern a)
     -> (ControlPattern -> Pattern a)
     -> Pattern (Map.Map String (Value, Value))
     -> ControlPattern
     -> Pattern a
contrastRange = contrastBy f
      where f (VI s, VI e) (VI v) = v >= s && v <= e
            f (VF s, VF e) (VF v) = v >= s && v <= e
            f (VN s, VN e) (VN v) = v >= s && v <= e
            f (VS s, VS e) (VS v) = v == s && v == e
            f _ _                 = False

{- |
  The @fix@ function applies another function to matching events in a pattern of
  controls. @fix@ is 'contrast' where the false-branching function is set to the
  identity 'id'. It is like 'contrast', but one function is given and applied to
  events with matching controls.

  For example, the following only adds the 'crush' control when the @n@ control
  is set to either 1 or 4:

  > d1 $ slow 2
  >    $ fix (# crush 3) (n "[1,4]")
  >    $ n "0 1 2 3 4 5 6"
  >    # sound "arpy"

  You can be quite specific; for example, the following applies the function
  @'hurry' 2@ to sample 1 of the drum sample set, and leaves the rest as they are:

  > fix (hurry 2) (s "drum" # n "1")
-}
fix :: (ControlPattern -> ControlPattern) -> ControlPattern -> ControlPattern -> ControlPattern
fix f = contrast f id

-- | Like 'contrast', but one function is given, and applied to events with
-- controls which don't match. @unfix@ is 'fix' but only applies when the
-- testing pattern is /not/ a match.
unfix :: (ControlPattern -> ControlPattern) -> ControlPattern -> ControlPattern -> ControlPattern
unfix = contrast id

{-|
  The @fixRange@ function isn’t very user-friendly at the moment, but you can
  create a @fix@ variant with a range condition. Any value of a 'ControlPattern'
  wich matches the values will apply the passed function.

  > d1 $ ( fixRange ( (# distort 1) . (# gain 0.8) )
  >                 ( pure $ Map.singleton "note" ((VN 0, VN 7)) )
  >      )
  >    $ s "superpiano"
  >   <| note "1 12 7 11"
-}
fixRange :: (ControlPattern -> Pattern ValueMap)
            -> Pattern (Map.Map String (Value, Value))
            -> ControlPattern
            -> ControlPattern
fixRange f = contrastRange f id

unfixRange :: (ControlPattern -> Pattern ValueMap)
              -> Pattern (Map.Map String (Value, Value))
              -> ControlPattern
              -> ControlPattern
unfixRange = contrastRange id

{- | @quantise@ limits values in a Pattern (or other Functor) to @n@ equally spaced
divisions of 1.

It is useful for rounding a collection of numbers to some particular base
fraction. For example,

> quantise 5 [0, 1.3 ,2.6,3.2,4.7,5]

It will round all the values to the nearest @(1/5)=0.2@ and thus will output
the list @[0.0,1.2,2.6,3.2,4.8,5.0]@. You can use this function to force a
continuous pattern like sine into specific values. In the following example:

> d1 $ s "superchip*8" # n (quantise 1 $ range (-10) (10) $ slow 8 $ cosine)
>                      # release (quantise 5 $ slow 8 $ sine + 0.1)

all the releases selected be rounded to the nearest @0.1@ and the notes selected
to the nearest @1@.

@quantise@ with fractional inputs does the consistent thing: @quantise 0.5@
rounds values to the nearest @2@, @quantise 0.25@ rounds the nearest @4@, etc.
-}
quantise :: (Functor f, RealFrac b) => b -> f b -> f b
quantise n = fmap ((/n) . (fromIntegral :: RealFrac b => Int -> b) . round . (*n))

-- | As 'quantise', but uses 'Prelude.floor' to calculate divisions.
qfloor :: (Functor f, RealFrac b) => b -> f b -> f b
qfloor n = fmap ((/n) . (fromIntegral :: RealFrac b => Int -> b) . floor . (*n))

-- | As 'quantise', but uses 'Prelude.ceiling' to calculate divisions.
qceiling :: (Functor f, RealFrac b) => b -> f b -> f b
qceiling n = fmap ((/n) . (fromIntegral :: RealFrac b => Int -> b) . ceiling . (*n))

-- | An alias for 'quantise'.
qround :: (Functor f, RealFrac b) => b -> f b -> f b
qround = quantise

-- | Inverts all the values in a boolean pattern
inv :: Functor f => f Bool -> f Bool
inv = (not <$>)

-- | Serialises a pattern so there's only one event playing at any one
-- time, making it /monophonic/. Events which start/end earlier are given priority.
mono :: Pattern a -> Pattern a
mono p = pattern $ \(State a cm) -> flatten $ query p (State a cm) where
  flatten :: [Event a] -> [Event a]
  flatten = mapMaybe constrainPart . truncateOverlaps . sortOn whole
  truncateOverlaps []     = []
  truncateOverlaps (e:es) = e : truncateOverlaps (mapMaybe (snip e) es)
  -- TODO - decide what to do about analog events..
  snip a b | start (wholeOrPart b) >= stop (wholeOrPart a) = Just b
           | stop (wholeOrPart b) <= stop (wholeOrPart a) = Nothing
           | otherwise = Just b {whole = Just $ Arc (stop $ wholeOrPart a) (stop $ wholeOrPart b)}
  constrainPart :: Event a -> Maybe (Event a)
  constrainPart e = do a <- subArc (wholeOrPart e) (part e)
                       return $ e {part = a}

{-|
@smooth@ receives a pattern of numbers and linearly goes from one to the next, passing through all of them. As time is cycle-based, after reaching the last number in the pattern, it will smoothly go to the first one again.

> d1 $ sound "bd*4" # pan (slow 4 $ smooth "0 1 0.5 1")

This sound will pan gradually from left to right, then to the center, then to the right again, and finally comes back to the left.
-}

-- serialize the given pattern
-- find the middle of the query's arc and use that to query the serialized pattern. We should get either no events or a single event back
-- if we don't get any events, return nothing
-- if we get an event, get the stop of its arc, and use that to query the serialized pattern, to see if there's an adjoining event
-- if there isn't, return the event as-is.
-- if there is, check where we are in the 'whole' of the event, and use that to tween between the values of the event and the next event
-- smooth :: Pattern Double -> Pattern Double

-- TODO - test this with analog events
smooth :: Fractional a => Pattern a -> Pattern a
smooth p = pattern $ \st@(State a cm) -> tween st a $ query monoP (State (midArc a) cm)
  where
    midArc a = Arc (mid (start a, stop a)) (mid (start a, stop a))
    tween _ _ [] = []
    tween st queryA (e:_) = maybe [e {whole = Just queryA, part = queryA}] (tween' queryA) (nextV st)
      where aStop = Arc (wholeStop e) (wholeStop e)
            nextEs st' = query monoP (st' {arc = aStop})
            nextV st' | null (nextEs st') = Nothing
                      | otherwise = Just $ value (head (nextEs st'))
            tween' queryA' v =
              [ Event
                { context = context e,
                  whole = Just queryA'
                , part = queryA'
                , value = value e + ((v - value e) * pc)}
              ]
            pc | delta' (wholeOrPart e) == 0 = 0
               | otherwise = fromRational $ (eventPartStart e - wholeStart e) / delta' (wholeOrPart e)
            delta' a = stop a - start a
    monoP = mono p

-- | Looks up values from a list of tuples, in order to swap values in the given pattern
swap :: Eq a => [(a, b)] -> Pattern a -> Pattern b
swap things p = filterJust $ (`lookup` things) <$> p

{-|
  @snowball@ takes a function that can combine patterns (like '+'),
  a function that transforms a pattern (like 'slow'),
  a depth, and a starting pattern,
  it will then transform the pattern and combine it with the last transformation until the depth is reached.
  This is like putting an effect (like a filter) in the feedback of a delay line; each echo is more affected.

  > d1 $ note ( scale "hexDorian"
  >           $ snowball 8 (+) (slow 2 . rev) "0 ~ . -1 . 5 3 4 . ~ -2"
  >           )
  >    # s "gtr"
-}
snowball :: Int -> (Pattern a -> Pattern a -> Pattern a) -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
snowball depth combinationFunction f pat = cat $ take depth $ scanl combinationFunction pat $ drop 1 $ iterate f pat

{- |
  Applies a function to a pattern and cats the resulting pattern, then continues
  applying the function until the depth is reached this can be used to create
  a pattern that wanders away from the original pattern by continually adding
  random numbers.

  > d1 $ note ( scale "hexDorian" mutateBy (+ (range -1 1 $ irand 2)) 8
  >           $ "0 1 . 2 3 4"
  >           )
  >    # s "gtr"
-}
soak ::  Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
soak depth f pat = cat $ take depth $ iterate f pat

-- | @construct n p@ breaks @p@ into pieces and then reassembles them
-- so that it fits into @n@ steps.
deconstruct :: Int -> Pattern String -> String
deconstruct n p = intercalate " " $ map showStep $ toList p
  where
    showStep :: [String] -> String
    showStep []  = "~"
    showStep [x] = x
    showStep xs  = "[" ++ (intercalate ", " xs) ++ "]"
    toList :: Pattern a -> [[a]]
    toList pat = map (\(s,e) -> map value $ queryArc (_segment n' pat) (Arc s e)) arcs
      where breaks = [0, (1/n') ..]
            arcs = zip (take n breaks) (drop 1 breaks)
            n' = fromIntegral n

{- | @bite n ipat pat@ slices a pattern @pat@ into @n@ pieces, then uses the
  @ipat@ pattern of integers to index into those slices. So @bite 4 "0 2*2" (run
  8)@ is the same as @"[0 1] [4 5]*2"@.

  I.e., it allows you to slice each cycle into a given number of equal sized
  bits, and then pattern those bits by number. It’s similar to @slice@, but is
  for slicing up patterns, rather than samples. The following slices the pattern
  into four bits, and then plays those bits in turn:

  > d1 $ bite 4 "0 1 2 3" $ n "0 .. 7" # sound "arpy"

  Of course that doesn’t actually change anything, but then you can reorder those bits:

  > d1 $ bite 4 "2 0 1 3" $ n "0 .. 7" # sound "arpy"

  The slices bits of pattern will be squeezed or contracted to fit:

  > d1 $ bite 4 "2 [0 3] 1*4 1" $ n "0 .. 7" # sound "arpy"
-}
bite :: Pattern Int -> Pattern Int -> Pattern a -> Pattern a
bite npat ipat pat = innerJoin $ (\n -> _bite n ipat pat) <$> npat

_bite :: Int -> Pattern Int -> Pattern a -> Pattern a
_bite n ipat pat = squeezeJoin $ zoompat <$> ipat
  where zoompat i = zoom (i'/(fromIntegral n), (i'+1)/(fromIntegral n)) pat
           where i' = fromIntegral $ i `mod` n

-- | Chooses from a list of patterns, using a pattern of integers.
squeeze :: Pattern Int -> [Pattern a] -> Pattern a
squeeze _ []      = silence
squeeze ipat pats = squeezeJoin $ (pats !!!) <$> ipat

squeezeJoinUp :: Pattern (ControlPattern) -> ControlPattern
squeezeJoinUp pp = pp {query = q, pureValue = Nothing}
  where q st = concatMap (f st) (query (filterDigital pp) st)
        f st (Event c (Just w) p v) =
          mapMaybe (munge c w p) $ query (compressArc (cycleArc w) (v |* P.speed (pure $ fromRational $ 1/(stop w - start w)))) st {arc = p}
        -- already ignoring analog events, but for completeness..
        f _ _ = []
        munge co oWhole oPart (Event ci (Just iWhole) iPart v) =
          do w' <- subArc oWhole iWhole
             p' <- subArc oPart iPart
             return (Event (combineContexts [ci,co]) (Just w') p' v)
        munge _ _ _ _ = Nothing

_chew :: Int -> Pattern Int -> ControlPattern  -> ControlPattern
_chew n ipat pat = (squeezeJoinUp $ zoompat <$> ipat) |/ P.speed (pure $ fromIntegral n)
  where zoompat i = zoom (i'/(fromIntegral n), (i'+1)/(fromIntegral n)) (pat)
           where i' = fromIntegral $ i `mod` n

{-|
  @chew@ works the same as 'bite', but speeds up\/slows down playback of sounds as
  well as squeezing\/contracting the slices of the provided pattern. Compare:

  > d1 $ 'bite' 4 "0 1*2 2*2 [~ 3]" $ n "0 .. 7" # sound "drum"
  > d1 $ chew 4 "0 1*2 2*2 [~ 3]" $ n "0 .. 7" # sound "drum"
-}

-- TODO maybe _chew could pattern the first parameter directly..
chew :: Pattern Int -> Pattern Int -> ControlPattern  -> ControlPattern
chew npat ipat pat = innerJoin $ (\n -> _chew n ipat pat) <$> npat

__binary :: Data.Bits.Bits b => Int -> b -> [Bool]
__binary n num = map (testBit num) $ reverse [0 .. n-1]

_binary :: Data.Bits.Bits b => Int -> b -> Pattern Bool
_binary n num = listToPat $ __binary n num

_binaryN :: Int -> Pattern Int -> Pattern Bool
_binaryN n p = squeezeJoin $ _binary n <$> p

binaryN :: Pattern Int -> Pattern Int -> Pattern Bool
binaryN n p = tParam _binaryN n p

binary :: Pattern Int -> Pattern Bool
binary = binaryN 8

ascii :: Pattern String -> Pattern Bool
ascii p = squeezeJoin $ (listToPat . concatMap (__binary 8 . ord)) <$> p

{- | Given a start point and a duration (both specified in cycles), this
  generates a control pattern that makes a sound begin at the start
  point and last the duration.

  The following are equivalent:

  > d1 $ slow 2 $ s "bev" # grain 0.2 0.1 # legato 1
  > d1 $ slow 2 $ s "bev" # begin 0.2 # end 0.3 # legato 1

  @grain@ is defined as:

  > grain s d = 'Sound.Tidal.Params.begin' s # 'Sound.Tidal.Params.end' (s+d)
-}
grain :: Pattern Double -> Pattern Double -> ControlPattern
grain s w = P.begin b # P.end e
  where b = s
        e = s + w

-- | For specifying a boolean pattern according to a list of offsets
-- (aka inter-onset intervals). For example @necklace 12 [4,2]@ is
-- the same as "t f f f t f t f f f t f". That is, 12 steps per cycle,
-- with true values alternating between every 4 and every 2 steps.
necklace :: Rational -> [Int] -> Pattern Bool
necklace perCycle xs = _slow ((toRational $ sum xs) / perCycle) $ listToPat $ list xs
  where list :: [Int] -> [Bool]
        list []      = []
        list (x:xs') = (True:(replicate (x-1) False)) ++ list xs'

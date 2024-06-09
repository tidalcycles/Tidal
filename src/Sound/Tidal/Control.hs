{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.Control where
{-
    Control.hs - Functions which concern control patterns, which are
    patterns of hashmaps, used for synth control values.

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

import           Prelude                  hiding ((*>), (<*))

import qualified Data.Map.Strict          as Map
import           Data.Maybe               (fromJust, fromMaybe, isJust)
import           Data.Ratio

import           Sound.Tidal.Core
import qualified Sound.Tidal.Params       as P
import           Sound.Tidal.Pattern
import           Sound.Tidal.Stream.Types (patternTimeID)
import           Sound.Tidal.UI
import           Sound.Tidal.Utils

{- | `spin` will "spin" and layer up a pattern the given number of times,
with each successive layer offset in time by an additional @1/n@ of a cycle,
and panned by an additional @1/n@. The result is a pattern that seems to spin
around. This function work well on multichannel systems.

> d1 $ slow 3
>    $ spin 4
>    $ sound "drum*3 tabla:4 [arpy:2 ~ arpy] [can:2 can:3]"
-}
spin :: Pattern Int -> ControlPattern -> ControlPattern
spin = patternify _spin

_spin :: Int -> ControlPattern -> ControlPattern
_spin copies p =
  stack $ map (\i -> let offset = toInteger i % toInteger copies in
                     offset `rotL` p
                     # P.pan (pure $ fromRational offset)
              )
          [0 .. (copies - 1)]



{- | `chop` granularises every sample in place as it is played, turning a
  pattern of samples into a pattern of sample parts. Can be used to explore
  granular synthesis.

  Use an integer value to specify how many granules each sample is chopped into:

  > d1 $ chop 16 $ sound "arpy arp feel*4 arpy*4"

  Different values of @chop@ can yield very different results, depending on the
  samples used:

  > d1 $ chop 16 $ sound (samples "arpy*8" (run 16))
  > d1 $ chop 32 $ sound (samples "arpy*8" (run 16))
  > d1 $ chop 256 $ sound "bd*4 [sn cp] [hh future]*2 [cp feel]"

  You can also use @chop@ (or 'striate') with very long samples to cut them into short
  chunks and pattern those chunks. The following cuts a sample into 32 parts, and
  plays it over 8 cycles:

  > d1 $ loopAt 8 $ chop 32 $ sound "bev"

  The 'loopAt' takes care of changing the speed of sample playback so that the
  sample fits in the given number of cycles perfectly. As a result, in the above
  the granules line up perfectly, so you can’t really hear that the sample has
  been cut into bits. Again, this becomes more apparent when you do further
  manipulations of the pattern, for example 'rev' to reverse the order of the cut
  up bits:

  > d1 $ loopAt 8 $ rev $ chop 32 $ sound "bev"
-}
chop :: Pattern Int -> ControlPattern -> ControlPattern
chop = patternify _chop

chopArc :: Arc -> Int -> [Arc]
chopArc (Arc s e) n = map (\i -> Arc (s + (e-s)*(fromIntegral i/fromIntegral n)) (s + (e-s)*(fromIntegral (i+1) / fromIntegral n))) [0 .. n-1]

_chop :: Int -> ControlPattern -> ControlPattern
_chop n pat = squeezeJoin $ f <$> pat
  where f v = fastcat $ map (pure . rangemap v) slices
        rangemap v (b, e) = Map.union (fromMaybe (makeMap (b,e)) $ merge v (b,e)) v
        merge :: ValueMap -> (Double, Double) -> Maybe ValueMap
        merge v (b, e) = do b' <- Map.lookup "begin" v >>= getF
                            e' <- Map.lookup "end" v >>= getF
                            let d = e' - b'
                            return $ makeMap (b' + b*d, b' + e*d)
        makeMap (b,e) = Map.fromList [("begin", VF b), ("end", VF $ e)]
        slices = map (\i -> (frac i, frac $ i + 1)) [0 .. n-1]
        frac i = fromIntegral i / fromIntegral n

{-| Striate is a kind of granulator, cutting samples into bits in a similar to
chop, but the resulting bits are organised differently. For example:

> d1 $ striate 3 $ sound "ho ho:2 ho:3 hc"

This plays the loop the given number of times, but triggers progressive portions
of each sample. So in this case it plays the loop three times, the first
time playing the first third of each sample, then the second time playing the
second third of each sample, and lastly playing the last third of each sample.
Replacing @striate@ with 'chop' above, one can hear that the ''chop' version
plays the bits from each chopped-up sample in turn, while @striate@ "interlaces"
the cut up bits of samples together.

You can also use @striate@ with very long samples, to cut them into short
chunks and pattern those chunks. This is where things get towards granular
synthesis. The following cuts a sample into 128 parts, plays it over 8 cycles
and manipulates those parts by reversing and rotating the loops:

> d1 $  slow 8 $ striate 128 $ sound "bev"
-}

striate :: Pattern Int -> ControlPattern -> ControlPattern
striate = patternify _striate

_striate :: Int -> ControlPattern -> ControlPattern
_striate n p = keepTactus (withTactus (* toRational n) p) $ fastcat $ map offset [0 .. n-1]
  where offset i = mergePlayRange (fromIntegral i / fromIntegral n, fromIntegral (i+1) / fromIntegral n) <$> p

mergePlayRange :: (Double, Double) -> ValueMap -> ValueMap
mergePlayRange (b,e) cm = Map.insert "begin" (VF ((b*d')+b')) $ Map.insert "end" (VF ((e*d')+b')) cm
  where b' = fromMaybe 0 $ Map.lookup "begin" cm >>= getF
        e' = fromMaybe 1 $ Map.lookup "end" cm >>= getF
        d' = e' - b'


{-|
The @striateBy@ function is a variant of `striate` with an extra
parameter which specifies the length of each part. The @striateBy@
function still scans across the sample over a single cycle, but if
each bit is longer, it creates a sort of stuttering effect. For
example the following will cut the @bev@ sample into 32 parts, but each
will be 1/16th of a sample long:

> d1 $ slow 32 $ striateBy 32 (1/16) $ sound "bev"

Note that `striate` and @striateBy@ use the `begin` and `end` parameters
internally. This means that you probably shouldn't also specify `begin` or
`end`.
-}
striateBy :: Pattern Int -> Pattern Double -> ControlPattern -> ControlPattern
striateBy = patternify2 _striateBy

-- | DEPRECATED, use 'striateBy' instead.
striate' :: Pattern Int -> Pattern Double -> ControlPattern -> ControlPattern
striate' = striateBy

_striateBy :: Int -> Double -> ControlPattern -> ControlPattern
_striateBy n f p = fastcat $ map (offset . fromIntegral) [0 .. n-1]
  where offset i = p # P.begin (pure (slot * i) :: Pattern Double) # P.end (pure ((slot * i) + f) :: Pattern Double)
        slot = (1 - f) / fromIntegral n


{- | `gap` is similar to `chop` in that it granualizes every sample in place as it is played,
but every other grain is silent. Use an integer value to specify how many granules
each sample is chopped into:

> d1 $ gap 8 $ sound "jvbass"
> d1 $ gap 16 $ sound "[jvbass drum:4]"
-}

gap :: Pattern Int -> ControlPattern -> ControlPattern
gap = patternify _gap

_gap :: Int -> ControlPattern -> ControlPattern
_gap n p = _fast (toRational n) (cat [pure 1, silence]) |>| _chop n p

{- |
  @weave@ applies one control pattern to a list of other control patterns, with
  a successive time offset. It uses an `OscPattern` to apply the function at
  different levels to each pattern, creating a weaving effect. For example:

  > d1 $ weave 16 (pan sine)
  >      [ sound "bd sn cp"
  >      , sound "casio casio:1"
  >      , sound "[jvbass*2 jvbass:2]/2"
  >      , sound "hc*4"
  >      ]

  In the above, the @pan sine@ control pattern is slowed down by the given
  number of cycles, in particular 16, and applied to all of the given sound
  patterns. What makes this interesting is that the @pan@ control pattern is
  successively offset for each of the given sound patterns; because the @pan@ is
  closed down by 16 cycles, and there are four patterns, they are ‘spread out’,
  i.e. with a gap of four cycles. For this reason, the four patterns seem to
  chase after each other around the stereo field. Try listening on headphones to
  hear this more clearly.

  You can even have it the other way round, and have the effect parameters chasing
  after each other around a sound parameter, like this:

  > d1 $ weave 16 (sound "arpy" >| n (run 8))
  >      [ vowel "a e i"
  >      , vowel "i [i o] o u"
  >      , vowel "[e o]/3 [i o u]/2"
  >      , speed "1 2 3"
  >      ]
-}
weave :: Time -> ControlPattern -> [ControlPattern] -> ControlPattern
weave t p ps = weave' t p (map (#) ps)


{-|
  @weaveWith@ is similar to the above, but weaves with a list of functions, rather
  than a list of controls. For example:

  > d1 $ weaveWith 3 (sound "bd [sn drum:2*2] bd*2 [sn drum:1]")
  >      [ fast 2
  >      , (# speed "0.5")
  >      , chop 16
  >      ]
-}
weaveWith :: Time -> Pattern a -> [Pattern a -> Pattern a] -> Pattern a
weaveWith t p fs | l == 0 = silence
              | otherwise = _slow t $ stack $ zipWith (\ i f -> (fromIntegral i % l) `rotL` _fast t (f (_slow t p))) [0 :: Int ..] fs
  where l = fromIntegral $ length fs

-- | An old alias for 'weaveWith'.
weave' :: Time -> Pattern a -> [Pattern a -> Pattern a] -> Pattern a
weave' = weaveWith

{- |
(A function that takes two ControlPatterns, and blends them together into
a new ControlPattern. An ControlPattern is basically a pattern of messages to
a synthesiser.)

Shifts between the two given patterns, using distortion.

Example:

> d1 $ interlace (sound  "bd sn kurt") (every 3 rev $ sound  "bd sn:2")
-}
interlace :: ControlPattern -> ControlPattern -> ControlPattern
interlace a b = weave 16 (P.shape (sine * 0.9)) [a, b]

{-
{- | Just like `striate`, but also loops each sample chunk a number of times specified in the second argument.
The primed version is just like `striateBy`, where the loop count is the third argument. For example:

> d1 $ striateL' 3 0.125 4 $ sound "feel sn:2"

Like `striate`, these use the `begin` and `end` parameters internally, as well as the `loop` parameter for these versions.
-}
striateL :: Pattern Int -> Pattern Int -> ControlPattern -> ControlPattern
striateL = tParam2 _striateL

striateL' :: Pattern Int -> Pattern Double -> Pattern Int -> ControlPattern -> ControlPattern
striateL' = tParam3 _striateL'

_striateL :: Int -> Int -> ControlPattern -> ControlPattern
_striateL n l p = _striate n p # loop (pure $ fromIntegral l)
_striateL' n f l p = _striateBy n f p # loop (pure $ fromIntegral l)


en :: [(Int, Int)] -> Pattern String -> Pattern String
en ns p = stack $ map (\(i, (k, n)) -> _e k n (samples p (pure i))) $ enumerate ns

-}

{-| @slice@ is similar to 'chop' and 'striate', in that it’s used to slice
  samples up into bits. The difference is that it allows you to rearrange those
  bits as a pattern.

  > d1 $ slice 8 "7 6 5 4 3 2 1 0"
  >    $ sound "breaks165"
  >    # legato 1

  The above slices the sample into eight bits, and then plays them backwards,
  equivalent of applying rev $ chop 8. Here’s a more complex example:

  > d1 $ slice 8 "[<0*8 0*2> 3*4 2 4] [4 .. 7]"
  >    $ sound "breaks165"
  >    # legato 1
-}
slice :: Pattern Int -> Pattern Int -> ControlPattern -> ControlPattern
slice pN pI p = P.begin b # P.end e # p
  where
    b = div' <$> pI <* pN
    e = b + pWidth
    pWidth = (\x -> 1.0 / fromIntegral x) <$> pN
    div' :: Int -> Int -> Double
    div' num den = fromIntegral (num `mod` den) / fromIntegral den


_slice :: Int -> Int -> ControlPattern -> ControlPattern
_slice n i p =
      p
      # P.begin (pure $ fromIntegral i / fromIntegral n)
      # P.end (pure $ fromIntegral (i+1) / fromIntegral n)

{-|
  @randslice@ chops the sample into the given number of pieces and then plays back
  a random one each cycle:

  > d1 $ randslice 32 $ sound "bev"

  Use 'fast' to get more than one per cycle:

  > d1 $ fast 4 $ randslice 32 $ sound "bev"
-}
randslice :: Pattern Int -> ControlPattern -> ControlPattern
randslice = patternify $ \n p -> keepTactus (withTactus (* (toRational n)) $ p) $ innerJoin $ (\i -> _slice n i p) <$> _irand n

_splice :: Int -> Pattern Int -> ControlPattern -> Pattern (Map.Map String Value)
_splice bits ipat pat = withEvent f (slice (pure bits) ipat pat) # P.unit (pure "c")
  where f ev = case Map.lookup "speed" (value ev) of
                        (Just (VF s)) -> ev {value = Map.insert "speed" (VF $ d*s) (value ev)}  -- if there is a speed parameter already present
                        _ -> ev {value = Map.insert "speed" (VF d) (value ev)}
          where d = sz / fromRational (wholeStop ev - wholeStart ev)
                sz = 1/fromIntegral bits

{-|
  @splice@ is similar to 'slice', but the slices are automatically pitched up or down
  to fit their ‘slot’.

  > d1 $ splice 8 "[<0*8 0*2> 3*4 2 4] [4 .. 7]" $ sound "breaks165"
-}
splice :: Pattern Int -> Pattern Int -> ControlPattern -> Pattern (Map.Map String Value)
splice bitpat ipat pat = setTactusFrom bitpat $ innerJoin $ (\bits -> _splice bits ipat pat) <$> bitpat

{-|
  @loopAt@ makes a sample fit the given number of cycles. Internally, it
  works by setting the `unit` parameter to @"c"@, changing the playback
  speed of the sample with the `speed` parameter, and setting setting
  the `density` of the pattern to match.

  > d1 $ loopAt 4 $ sound "breaks125"

  It’s a good idea to use this in conjuction with 'chop', so the break is chopped
  into pieces and you don’t have to wait for the whole sample to start/stop.

  > d1 $ loopAt 4 $ chop 32 $ sound "breaks125"

  Like all Tidal functions, you can mess about with this considerably. The below
  example shows how you can supply a pattern of cycle counts to @loopAt@:

  > d1 $ juxBy 0.6 (|* speed "2")
  >    $ slowspread (loopAt) [4,6,2,3]
  >    $ chop 12
  >    $ sound "fm:14"
-}
loopAt :: Pattern Time -> ControlPattern -> ControlPattern
loopAt n p = slow n p |* P.speed (fromRational <$> (1/n)) # P.unit (pure "c")

{-|
  @hurry@ is similiar to 'fast' in that it speeds up a pattern, but it also
  increases the speed control by the same factor. So, if you’re triggering
  samples, the sound gets higher in pitch. For example:

  > d1 $ every 2 (hurry 2) $ sound "bd sn:2 ~ cp"
-}
hurry :: Pattern Rational -> ControlPattern -> ControlPattern
hurry !x = (|* P.speed (fromRational <$> x)) . fast x

{- | @smash@ is a combination of `spread` and `striate` — it cuts the samples
into the given number of bits, and then cuts between playing the loop
at different speeds according to the values in the list. So this:

> d1 $ smash 3 [2,3,4] $ sound "ho ho:2 ho:3 hc"

is a bit like this:

> d1 $ spread (slow) [2,3,4] $ striate 3 $ sound "ho ho:2 ho:3 hc"

This is quite dancehall:

> d1 $ ( spread' slow "1%4 2 1 3"
>      $ spread (striate) [2,3,4,1]
>      $ sound "sn:2 sid:3 cp sid:4"
>      )
>    # speed "[1 2 1 1]/2"
-}

smash :: Pattern Int -> [Pattern Time] -> ControlPattern -> Pattern ValueMap
smash n xs p = slowcat $ map (`slow` p') xs
  where p' = striate n p

{- | An altenative form of `smash`, which uses `chop` instead of `striate`.

  Compare the following variations:

  > d1 $ smash 6 [2,3,4] $ sound "ho ho:2 ho:3 hc"
  > d1 $ smash' 6 [2,3,4] $ sound "ho ho:2 ho:3 hc"
  > d1 $ smash 12 [2,3,4] $ s "bev*4"
  > d1 $ smash' 12 [2,3,4] $ s "bev*4"
-}
smash' :: Int -> [Pattern Time] -> ControlPattern -> ControlPattern
smash' n xs p = slowcat $ map (`slow` p') xs
  where p' = _chop n p

{- |
    Applies a type of delay to a pattern.
    It has three parameters, which could be called @depth@, @time@ and @feedback@.
    @depth@ is and integer, and @time@ and @feedback@ are floating point numbers.

    This adds a bit of echo:

    > d1 $ echo 4 0.2 0.5 $ sound "bd sn"

    The above results in 4 echos, each one 50% quieter than the last, with 1/5th of a cycle between them.

    It is possible to reverse the echo:

    > d1 $ echo 4 (-0.2) 0.5 $ sound "bd sn"
-}
echo :: Pattern Integer -> Pattern Rational -> Pattern Double -> ControlPattern -> ControlPattern
echo = patternify3' _echo

_echo :: Integer -> Rational -> Double -> ControlPattern -> ControlPattern
_echo count time feedback p = _echoWith count time (|* P.gain (pure $ feedback)) p

{- |
  @echoWith@ is similar to 'echo', but instead of just decreasing volume to
  produce echoes, @echoWith@ applies a function each step and overlays the
  result delayed by the given time.

  > d1 $ echoWith 2 "1%3" (# vowel "{a e i o u}%2") $ sound "bd sn"

  In this case there are two _overlays_ delayed by 1/3 of a cycle, where each
  has the 'vowel' filter applied.

  > d1 $ echoWith 4 (1/6) (|* speed "1.5") $ sound "arpy arpy:2"

  In the above, three versions are put on top, with each step getting higher in
  pitch as @|* speed "1.5"@ is successively applied.
-}
echoWith :: Pattern Int -> Pattern Time -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
echoWith n t f p = innerJoin $ (\a b -> _echoWith a b f p) <$> n <* t

_echoWith :: (Num n, Ord n) => n -> Time -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
_echoWith count time f p | count <= 1 = p
                         | otherwise = overlay (f (time `rotR` _echoWith (count-1) time f p)) p

-- | DEPRECATED, use 'echo' instead
stut :: Pattern Integer -> Pattern Double -> Pattern Rational -> ControlPattern -> ControlPattern
stut = patternify3' _stut

_stut :: Integer -> Double -> Rational -> ControlPattern -> ControlPattern
_stut count feedback steptime p = stack (p:map (\x -> ((x%1)*steptime) `rotR` (p |* P.gain (pure $ scalegain (fromIntegral x)))) [1..(count-1)])
  where scalegain
          = (+feedback) . (*(1-feedback)) . (/ fromIntegral count) . (fromIntegral count -)

-- | DEPRECATED, use 'echoWith' instead
stutWith :: Pattern Int -> Pattern Time -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
stutWith n t f p = innerJoin $ (\a b -> _stutWith a b f p) <$> n <* t

_stutWith :: (Num n, Ord n) => n -> Time -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
_stutWith count steptime f p | count <= 1 = p
                             | otherwise = overlay (f (steptime `rotR` _stutWith (count-1) steptime f p)) p

-- | DEPRECATED, use 'echoWith' instead
stut' :: Pattern Int -> Pattern Time -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
stut' = stutWith

-- | Turns a pattern of seconds into a pattern of (rational) cycle durations
sec :: Fractional a => Pattern a -> Pattern a
sec p = (realToFrac <$> cF 1 "_cps") *| p

-- | Turns a pattern of milliseconds into a pattern of (rational)
-- cycle durations, according to the current cps.
msec :: Fractional a => Pattern a -> Pattern a
msec p = (realToFrac . (/1000) <$> cF 1 "_cps") *| p

-- | Align the start of a pattern with the time a pattern is evaluated,
-- rather than the global start time. Because of this, the pattern will
-- probably not be aligned to the pattern grid.
trigger :: Pattern a -> Pattern a
trigger = triggerWith id

-- | (Alias @__qt__@) Quantise trigger. Aligns the start of the pattern
-- with the next cycle boundary. For example, this pattern will fade in
-- starting with the next cycle after the pattern is evaluated:
--
-- > d1 $ qtrigger $ s "hh(5, 8)" # amp envL
--
-- Note that the pattern will start playing immediately. The /start/ of the
-- pattern aligns with the next cycle boundary, but events will play before
-- if the pattern has events at negative timestamps (which most loops do).
-- These events can be filtered out, for example:
--
-- > d1 $ qtrigger $ filterWhen (>= 0) $ s "hh(5, 8)"
--
-- Alternatively, you can use 'wait' to achieve the same result:
--
-- > wait 1 1 $ s "bd hh hh hh"
qtrigger :: Pattern a -> Pattern a
qtrigger = ctrigger

-- | Alias for 'qtrigger'.
qt :: Pattern a -> Pattern a
qt = qtrigger

-- | Ceiling trigger. Aligns the start of a pattern to the next cycle
-- boundary, just like 'qtrigger'.
ctrigger :: Pattern a -> Pattern a
ctrigger = triggerWith $ (fromIntegral :: Int -> Rational) . ceiling

-- | Rounded trigger. Aligns the start of a pattern to the nearest cycle
-- boundary, either next or previous.
rtrigger :: Pattern a -> Pattern a
rtrigger = triggerWith $ (fromIntegral :: Int -> Rational) . round

-- | Floor trigger. Aligns the start of a pattern to the previous cycle
-- boundary.
ftrigger :: Pattern a -> Pattern a
ftrigger = triggerWith $ (fromIntegral :: Int -> Rational) . floor

{- | (Alias @__mt__@) Mod trigger. Aligns the start of a pattern to the
  next cycle boundary where the cycle is evenly divisible by a given
  number. 'qtrigger' is equivalent to @mtrigger 1@.

  In the following example, when activating the @d1@ pattern, it will start at the
  same time as the next clap, even if it has to wait for 3 cycles. Once activated,
  the @arpy@ sound will play on every cycle, just like any other pattern:

  > do
  >   resetCycles
  >   d2 $ every 4 (# s "clap") $ s "bd"

  > d1 $ mtrigger 4 $ filterWhen (>=0) $ s "arpy"
-}
mtrigger :: Int -> Pattern a -> Pattern a
mtrigger n = triggerWith $ fromIntegral . nextMod
  where nextMod t = n * ceiling (t / (fromIntegral n))

-- | Alias for 'mtrigger'.
mt :: Int -> Pattern a -> Pattern a
mt = mtrigger

{- | This aligns the start of a pattern to some value relative to the
  time the pattern is evaluated. The provided function maps the evaluation
  time (on the global cycle clock) to a new time, and then @triggerWith@
  aligns the pattern's start to the time that's returned.

  This is a more flexible triggering function. In fact, all the other trigger
  functions are defined based on @triggerWith@. For example, 'trigger' is just
  @triggerWith id@.

  In the next example, use @d1@ as a metronome, and play with different values
  (from 0 to 1) on the @const@ expression. You’ll notice how the @clap@ is
  displaced from the beginning of each cycle to the end, as the number increases:

  > d1 $ s "bd hh!3"
  >
  > d2 $ triggerWith (const 0.1) $ s "clap"

  This last example is equivalent to this:

  > d2 $ rotR 0.1 $ s "clap"
-}
triggerWith :: (Time -> Time) -> Pattern a -> Pattern a
triggerWith f pat = pat {query = q}
  where q st = query (rotR (offset st) pat) st
        offset st = fromMaybe 0 $ f
                      <$> (Map.lookup patternTimeID (controls st) >>= getR)

splat :: Pattern Int -> ControlPattern -> ControlPattern -> ControlPattern
splat slices epat pat = chop slices pat # bite 1 (const 0 <$> pat) epat

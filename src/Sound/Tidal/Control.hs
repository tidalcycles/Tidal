{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}

module Sound.Tidal.Control where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, fromJust)
import Data.Ratio

import Sound.Tidal.Pattern
import Sound.Tidal.Core
import Sound.Tidal.UI
import Sound.Tidal.Params
import Sound.Tidal.Utils

{- | `spin` will "spin" a layer up a pattern the given number of times,
with each successive layer offset in time by an additional `1/n` of a
cycle, and panned by an additional `1/n`. The result is a pattern that
seems to spin around. This function works best on multichannel
systems.

@
d1 $ slow 3 $ spin 4 $ sound "drum*3 tabla:4 [arpy:2 ~ arpy] [can:2 can:3]"
@
-}
spin :: Pattern Int -> ControlPattern -> ControlPattern
spin = tParam _spin

_spin :: Int -> ControlPattern -> ControlPattern
_spin copies p =
  stack $ map (\n -> let offset = toInteger n % toInteger copies in
                     offset `rotL` p
                     # pan (pure $ fromRational offset)
              )
          [0 .. (copies - 1)]



{- | `chop` granualizes every sample in place as it is played, turning a pattern of samples into a pattern of sample parts. Use an integer value to specify how many granules each sample is chopped into:

@
d1 $ chop 16 $ sound "arpy arp feel*4 arpy*4"
@

Different values of `chop` can yield very different results, depending
on the samples used:


@
d1 $ chop 16 $ sound (samples "arpy*8" (run 16))
d1 $ chop 32 $ sound (samples "arpy*8" (run 16))
d1 $ chop 256 $ sound "bd*4 [sn cp] [hh future]*2 [cp feel]"
@
-}

chop :: Pattern Int -> ControlPattern -> ControlPattern
chop = tParam _chop

chopArc :: Arc -> Int -> [Arc]
chopArc (s, e) n = map (\i -> ((s + (e-s)*(fromIntegral i/fromIntegral n)), s + (e-s)*((fromIntegral $ i+1)/fromIntegral n))) [0 .. n-1]

_chop :: Int -> ControlPattern -> ControlPattern
_chop n p = withEvents (concatMap chopEvent) p
  where -- for each part,
        chopEvent :: Event ControlMap -> [Event ControlMap]
        chopEvent ((whole,part), v) = map (\a -> chomp part v (length $ chopArc whole n) a) $ arcs whole part
        -- cut whole into n bits, and number them
        arcs whole part = numberedArcs part $ chopArc whole n
        -- each bit is a new whole, with part that's the intersection of old part and new whole
        -- (discard new parts that don't intersect with the old part)
        numberedArcs :: Arc -> [Arc] -> [(Int, Part)]
        numberedArcs part as = map ((fromJust <$>) <$>) $ filter (isJust . snd . snd) $ enumerate $ map (\a -> (a, subArc part a)) as
        -- begin set to i/n, end set to i+1/n
        -- if the old event had a begin and end, then multiply the new
        -- begin and end values by the old difference (end-begin), and
        -- add the old begin
        chomp :: Arc -> ControlMap -> Int -> (Int, Part) -> Event ControlMap
        chomp part v n (i, pt) = (pt,
                                  Map.insert "begin" (VF b') $ Map.insert "end" (VF e') v
                                 )
          where b = fromMaybe 0 $ do v' <- Map.lookup "begin" v
                                     getF v'
                e = fromMaybe 1 $ do v' <- Map.lookup "end" v
                                     getF v'
                d = e-b
                b' = (((fromIntegral i)/(fromIntegral n)) * d) + b
                e' = (((fromIntegral $ i+1)/(fromIntegral n)) * d) + b

{-
-- A simpler definition than the above, but this version doesn't chop
-- with multiple chops, and only works with a single 'pure' event..
_chop' :: Int -> ControlPattern -> ControlPattern
_chop' n p = begin (fromList begins) # end (fromList ends) # p
  where step = 1/(fromIntegral n)
        begins = [0,step .. (1-step)]
        ends = (tail begins) ++ [1]
-}


{- | Striate is a kind of granulator, for example:

@
d1 $ striate 3 $ sound "ho ho:2 ho:3 hc"
@

This plays the loop the given number of times, but triggering
progressive portions of each sample. So in this case it plays the loop
three times, the first time playing the first third of each sample,
then the second time playing the second third of each sample, etc..
With the highhat samples in the above example it sounds a bit like
reverb, but it isn't really.

You can also use striate with very long samples, to cut it into short
chunks and pattern those chunks. This is where things get towards
granular synthesis. The following cuts a sample into 128 parts, plays
it over 8 cycles and manipulates those parts by reversing and rotating
the loops.

@
d1 $  slow 8 $ striate 128 $ sound "bev"
@
-}

striate :: Pattern Int -> ControlPattern -> ControlPattern
striate = tParam _striate

_striate :: Int -> ControlPattern -> ControlPattern
_striate n p = fastcat $ map (\x -> off (fromIntegral x) p) [0 .. n-1]
  where off i p = (mergePlayRange ((fromIntegral i / fromIntegral n), (fromIntegral (i+1) / fromIntegral n))) <$> p

mergePlayRange :: (Double, Double) -> ControlMap -> ControlMap
mergePlayRange (b,e) cm = Map.insert "begin" (VF $ (b*d')+b') $ Map.insert "end" (VF $ (e*d')+b') $ cm
  where b' = fromMaybe 0 $ Map.lookup "begin" cm >>= getF
        e' = fromMaybe 1 $ Map.lookup "end" cm >>= getF
        d' = e' - b'


{-|
The `striate'` function is a variant of `striate` with an extra
parameter, which specifies the length of each part. The `striate'`
function still scans across the sample over a single cycle, but if
each bit is longer, it creates a sort of stuttering effect. For
example the following will cut the bev sample into 32 parts, but each
will be 1/16th of a sample long:

@
d1 $ slow 32 $ striate' 32 (1/16) $ sound "bev"
@

Note that `striate` uses the `begin` and `end` parameters
internally. This means that if you're using `striate` (or `striate'`)
you probably shouldn't also specify `begin` or `end`. -}
striate' :: Pattern Int -> Pattern Double -> ControlPattern -> ControlPattern
striate' = tParam2 _striate'

_striate' :: Int -> Double -> ControlPattern -> ControlPattern
_striate' n f p = fastcat $ map (\x -> off (fromIntegral x) p) [0 .. n-1]
  where off i p = p # begin (pure (slot * i) :: Pattern Double) # end (pure ((slot * i) + f) :: Pattern Double)
        slot = (1 - f) / (fromIntegral n)

{- | `gap` is similar to `chop` in that it granualizes every sample in place as it is played,
but every other grain is silent. Use an integer value to specify how many granules
each sample is chopped into:

@
d1 $ gap 8 $ sound "jvbass"
d1 $ gap 16 $ sound "[jvbass drum:4]"
@-}

gap :: Pattern Int -> ControlPattern -> ControlPattern
gap = tParam _gap

_gap :: Int -> ControlPattern -> ControlPattern 
_gap n p = (_fast (toRational n) $ cat [pure 1, silence]) |>| ( _chop n p)

{- |
`weave` applies a function smoothly over an array of different patterns. It uses an `OscPattern` to
apply the function at different levels to each pattern, creating a weaving effect.

@
d1 $ weave 3 (shape $ sine1) [sound "bd [sn drum:2*2] bd*2 [sn drum:1]", sound "arpy*8 ~"]
@
-}
weave :: Rational -> ControlPattern -> [ControlPattern] -> ControlPattern
weave t p ps = weave' t p (map (\x -> (x #)) ps)


{- | `weave'` is similar in that it blends functions at the same time at different amounts over a pattern:

@
d1 $ weave' 3 (sound "bd [sn drum:2*2] bd*2 [sn drum:1]") [density 2, (# speed "0.5"), chop 16]
@
-}
weave' :: Rational -> Pattern a -> [Pattern a -> Pattern a] -> Pattern a
weave' t p fs | l == 0 = silence
              | otherwise = _slow t $ stack $ map (\(i, f) -> (fromIntegral i % l) `rotL` (_fast t $ f (_slow t p))) (zip [0 ..] fs)
  where l = fromIntegral $ length fs

{- |
(A function that takes two ControlPatterns, and blends them together into
a new ControlPattern. An ControlPattern is basically a pattern of messages to
a synthesiser.)

Shifts between the two given patterns, using distortion.

Example:

@
d1 $ interlace (sound  "bd sn kurt") (every 3 rev $ sound  "bd sn:2")
@
-}
interlace :: ControlPattern -> ControlPattern -> ControlPattern
interlace a b = weave 16 (shape $ ((* 0.9) <$> sine)) [a, b]

{-
{- | Just like `striate`, but also loops each sample chunk a number of times specified in the second argument.
The primed version is just like `striate'`, where the loop count is the third argument. For example:

@
d1 $ striateL' 3 0.125 4 $ sound "feel sn:2"
@

Like `striate`, these use the `begin` and `end` parameters internally, as well as the `loop` parameter for these versions.
-}
striateL :: Pattern Int -> Pattern Int -> ControlPattern -> ControlPattern
striateL = tParam2 _striateL

striateL' :: Pattern Int -> Pattern Double -> Pattern Int -> ControlPattern -> ControlPattern
striateL' = tParam3 _striateL'

_striateL :: Int -> Int -> ControlPattern -> ControlPattern
_striateL n l p = _striate n p # loop (pure $ fromIntegral l)
_striateL' n f l p = _striate' n f p # loop (pure $ fromIntegral l)


en :: [(Int, Int)] -> Pattern String -> Pattern String
en ns p = stack $ map (\(i, (k, n)) -> _e k n (samples p (pure i))) $ enumerate ns

-}

slice :: Pattern Int -> Pattern Int -> ControlPattern -> ControlPattern
slice pi pn p = begin b # end e # p
  where b = (\i n -> (div' i n)) <$> pi <*> pn
        e = (\i n -> (div' i n) + (div' 1 n)) <$> pi <*> pn
        div' a b = fromIntegral (a `mod` b) / fromIntegral b

_slice :: Int -> Int -> ControlPattern -> ControlPattern
_slice i n p =
      p
      # begin (pure $ fromIntegral i / fromIntegral n)
      # end (pure $ fromIntegral (i+1) / fromIntegral n)

randslice :: Int -> ControlPattern -> ControlPattern
randslice n p = unwrap $ (\i -> _slice i n p) <$> irand n

{- |
`loopAt` makes a sample fit the given number of cycles. Internally, it
works by setting the `unit` parameter to "c", changing the playback
speed of the sample with the `speed` parameter, and setting setting
the `density` of the pattern to match.

@
d1 $ loopAt 4 $ sound "breaks125"
d1 $ juxBy 0.6 (|*| speed "2") $ slowspread (loopAt) [4,6,2,3] $ chop 12 $ sound "fm:14"
@
-}
loopAt :: Pattern Time -> ControlPattern -> ControlPattern
loopAt n p = slow n p |*| speed (fromRational <$> (1/n)) # unit (pure "c")

hurry :: Pattern Rational -> ControlPattern -> ControlPattern
hurry x = (|*| speed (fromRational <$> x)) . fast x

{- | Smash is a combination of `spread` and `striate` - it cuts the samples
into the given number of bits, and then cuts between playing the loop
at different speeds according to the values in the list.

So this:

@
d1 $ smash 3 [2,3,4] $ sound "ho ho:2 ho:3 hc"
@

Is a bit like this:

@
d1 $ spread (slow) [2,3,4] $ striate 3 $ sound "ho ho:2 ho:3 hc"
@

This is quite dancehall:

@
d1 $ (spread' slow "1%4 2 1 3" $ spread (striate) [2,3,4,1] $ sound
"sn:2 sid:3 cp sid:4")
  # speed "[1 2 1 1]/2"
@
-}

smash n xs p = slowcat $ map (\n -> slow n p') xs
  where p' = striate n p

{- | an altenative form to `smash` is `smash'` which will use `chop` instead of `striate`.
-}
smash' n xs p = slowcat $ map (\n -> slow n p') xs
  where p' = _chop n p


{- | Stut applies a type of delay to a pattern. It has three parameters,
which could be called depth, feedback and time. Depth is an integer
and the others floating point. This adds a bit of echo:

@
d1 $ stut 4 0.5 0.2 $ sound "bd sn"
@

The above results in 4 echos, each one 50% quieter than the last,
with 1/5th of a cycle between them. It is possible to reverse the echo:

@
d1 $ stut 4 0.5 (-0.2) $ sound "bd sn"
@
-}

stut :: Pattern Integer -> Pattern Double -> Pattern Rational -> ControlPattern -> ControlPattern
stut = tParam3 _stut

_stut :: Integer -> Double -> Rational -> ControlPattern -> ControlPattern
_stut steps feedback time p = stack (p:(map (\x -> (((x%steps)*time) `rotR` (p |*| gain (pure $ scale (fromIntegral x))))) [1..(steps-1)]))
  where scale x
          = ((+feedback) . (*(1-feedback)) . (/(fromIntegral steps)) . ((fromIntegral steps)-)) x

{- | Instead of just decreasing volume to produce echoes, @stut'@ allows to apply a function for each step and overlays the result delayed by the given time.

@
d1 $ stut' 2 (1%3) (# vowel "{a e i o u}%2") $ sound "bd sn"
@

In this case there are two _overlays_ delayed by 1/3 of a cycle, where each has the @vowel@ filter applied.
-}
stut' :: Pattern Int -> Pattern Time -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
stut' n t f p = unwrap $ (\a b -> _stut' a b f p) <$> n <*> t

_stut' :: (Num n, Ord n) => n -> Time -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
_stut' steps steptime f p | steps <= 0 = p
                         | otherwise = overlay (f (steptime `rotR` _stut' (steps-1) steptime f p)) p

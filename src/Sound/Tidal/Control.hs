{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings, FlexibleContexts #-}

module Sound.Tidal.Control where

import           Prelude hiding ((<*), (*>))

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, fromJust)
import Data.Ratio

import Sound.Tidal.Pattern
import Sound.Tidal.Core
import Sound.Tidal.UI
import qualified Sound.Tidal.Params as P
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
  stack $ map (\i -> let offset = toInteger i % toInteger copies in
                     offset `rotL` p
                     # P.pan (pure $ fromRational offset)
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
chopArc (Arc s e) n = map (\i -> Arc (s + (e-s)*(fromIntegral i/fromIntegral n)) (s + (e-s)*(fromIntegral (i+1) / fromIntegral n))) [0 .. n-1]

_chop :: Int -> ControlPattern -> ControlPattern
_chop n = withEvents (concatMap chopEvent)
  where -- for each part,
        chopEvent :: Event ControlMap -> [Event ControlMap]
        chopEvent (Event c (Just w) p' v) = map (chomp c v (length $ chopArc w n)) $ arcs w p'
        -- ignoring 'analog' events (those without wholes),
        chopEvent _ = []
        -- cut whole into n bits, and number them
        arcs w' p' = numberedArcs p' $ chopArc w' n
        -- each bit is a new whole, with part that's the intersection of old part and new whole
        -- (discard new parts that don't intersect with the old part)
        numberedArcs :: Arc -> [Arc] -> [(Int, (Arc, Arc))]
        numberedArcs p' as = map ((fromJust <$>) <$>) $ filter (isJust . snd . snd) $ enumerate $ map (\a -> (a, subArc p' a)) as
        -- begin set to i/n, end set to i+1/n
        -- if the old event had a begin and end, then multiply the new
        -- begin and end values by the old difference (end-begin), and
        -- add the old begin
        chomp :: Context -> ControlMap -> Int -> (Int, (Arc, Arc)) -> Event ControlMap
        chomp c v n' (i, (w,p')) = Event c (Just w) p' (Map.insert "begin" (VF b') $ Map.insert "end" (VF e') v)
          where b = fromMaybe 0 $ do v' <- Map.lookup "begin" v
                                     getF v'
                e = fromMaybe 1 $ do v' <- Map.lookup "end" v
                                     getF v'
                d = e-b
                b' = ((fromIntegral i/fromIntegral n') * d) + b
                e' = ((fromIntegral (i+1) / fromIntegral n') * d) + b

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
_striate n p = fastcat $ map offset [0 .. n-1]
  where offset i = mergePlayRange (fromIntegral i / fromIntegral n, fromIntegral (i+1) / fromIntegral n) <$> p

mergePlayRange :: (Double, Double) -> ControlMap -> ControlMap
mergePlayRange (b,e) cm = Map.insert "begin" (VF $ (b*d')+b') $ Map.insert "end" (VF $ (e*d')+b') cm
  where b' = fromMaybe 0 $ Map.lookup "begin" cm >>= getF
        e' = fromMaybe 1 $ Map.lookup "end" cm >>= getF
        d' = e' - b'


{-|
The `striateBy` function is a variant of `striate` with an extra
parameter, which specifies the length of each part. The `striateBy`
function still scans across the sample over a single cycle, but if
each bit is longer, it creates a sort of stuttering effect. For
example the following will cut the bev sample into 32 parts, but each
will be 1/16th of a sample long:

@
d1 $ slow 32 $ striateBy 32 (1/16) $ sound "bev"
@

Note that `striate` uses the `begin` and `end` parameters
internally. This means that if you're using `striate` (or `striateBy`)
you probably shouldn't also specify `begin` or `end`. -}
striateBy :: Pattern Int -> Pattern Double -> ControlPattern -> ControlPattern
striateBy = tParam2 _striateBy

-- Old name for striateBy, here as a deprecated alias for now.
striate' :: Pattern Int -> Pattern Double -> ControlPattern -> ControlPattern
striate' = striateBy

_striateBy :: Int -> Double -> ControlPattern -> ControlPattern
_striateBy n f p = fastcat $ map (offset . fromIntegral) [0 .. n-1]
  where offset i = p # P.begin (pure (slot * i) :: Pattern Double) # P.end (pure ((slot * i) + f) :: Pattern Double)
        slot = (1 - f) / fromIntegral n



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
_gap n p = _fast (toRational n) (cat [pure 1, silence]) |>| _chop n p

{- |
`weave` applies a function smoothly over an array of different patterns. It uses an `OscPattern` to
apply the function at different levels to each pattern, creating a weaving effect.

@
d1 $ weave 3 (shape $ sine1) [sound "bd [sn drum:2*2] bd*2 [sn drum:1]", sound "arpy*8 ~"]
@
-}
weave :: Time -> ControlPattern -> [ControlPattern] -> ControlPattern
weave t p ps = weave' t p (map (#) ps)


{- | `weaveWith` is similar in that it blends functions at the same time at different amounts over a pattern:

@
d1 $ weaveWith 3 (sound "bd [sn drum:2*2] bd*2 [sn drum:1]") [density 2, (# speed "0.5"), chop 16]
@
-}
weaveWith :: Time -> Pattern a -> [Pattern a -> Pattern a] -> Pattern a
weaveWith t p fs | l == 0 = silence
              | otherwise = _slow t $ stack $ map (\(i, f) -> (fromIntegral i % l) `rotL` _fast t (f (_slow t p))) (zip [0 :: Int ..] fs)
  where l = fromIntegral $ length fs

weave' :: Time -> Pattern a -> [Pattern a -> Pattern a] -> Pattern a
weave' = weaveWith

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
interlace a b = weave 16 (P.shape (sine * 0.9)) [a, b]

{-
{- | Just like `striate`, but also loops each sample chunk a number of times specified in the second argument.
The primed version is just like `striateBy`, where the loop count is the third argument. For example:

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
_striateL' n f l p = _striateBy n f p # loop (pure $ fromIntegral l)


en :: [(Int, Int)] -> Pattern String -> Pattern String
en ns p = stack $ map (\(i, (k, n)) -> _e k n (samples p (pure i))) $ enumerate ns

-}

slice :: Pattern Int -> Pattern Int -> ControlPattern -> ControlPattern
slice pN pI p = P.begin b # P.end e # p
  where b = div' <$> pI <* pN
        e = (\i n -> div' i n + div' 1 n) <$> pI <* pN
        div' num den = fromIntegral (num `mod` den) / fromIntegral den

_slice :: Int -> Int -> ControlPattern -> ControlPattern
_slice n i p =
      p
      # P.begin (pure $ fromIntegral i / fromIntegral n)
      # P.end (pure $ fromIntegral (i+1) / fromIntegral n)

randslice :: Pattern Int -> ControlPattern -> ControlPattern
randslice = tParam $ \n p -> innerJoin $ (\i -> _slice n i p) <$> irand n

splice :: Pattern Int -> Pattern Int -> ControlPattern -> Pattern (Map.Map String Value)
splice bits ipat pat = withEvent f (slice bits ipat pat) # P.unit (pure "c")
  where f ev = ev {value = Map.insert "speed" (VF d) (value ev)}
          where d = sz / (fromRational $ (wholeStop ev) - (wholeStart ev))
                sz = 1/(fromIntegral bits)

{- |
`loopAt` makes a sample fit the given number of cycles. Internally, it
works by setting the `unit` parameter to "c", changing the playback
speed of the sample with the `speed` parameter, and setting setting
the `density` of the pattern to match.

@
d1 $ loopAt 4 $ sound "breaks125"
d1 $ juxBy 0.6 (|* speed "2") $ slowspread (loopAt) [4,6,2,3] $ chop 12 $ sound "fm:14"
@
-}
loopAt :: Pattern Time -> ControlPattern -> ControlPattern
loopAt n p = slow n p |* P.speed (fromRational <$> (1/n)) # P.unit (pure "c")

hurry :: Pattern Rational -> ControlPattern -> ControlPattern
hurry x = (|* P.speed (fromRational <$> x)) . fast x

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

smash :: Pattern Int -> [Pattern Time] -> ControlPattern -> Pattern ControlMap
smash n xs p = slowcat $ map (`slow` p') xs
  where p' = striate n p

{- | an altenative form to `smash` is `smash'` which will use `chop` instead of `striate`.
-}
smash' :: Int -> [Pattern Time] -> ControlPattern -> Pattern ControlMap
smash' n xs p = slowcat $ map (`slow` p') xs
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
_stut count feedback steptime p = stack (p:map (\x -> ((x%1)*steptime) `rotR` (p |* P.gain (pure $ scalegain (fromIntegral x)))) [1..(count-1)])
  where scalegain
          = (+feedback) . (*(1-feedback)) . (/ fromIntegral count) . (fromIntegral count -)

{- | Instead of just decreasing volume to produce echoes, @stut'@ allows to apply a function for each step and overlays the result delayed by the given time.

@
d1 $ stut' 2 (1%3) (# vowel "{a e i o u}%2") $ sound "bd sn"
@

In this case there are two _overlays_ delayed by 1/3 of a cycle, where each has the @vowel@ filter applied.
-}
stutWith :: Pattern Int -> Pattern Time -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
stutWith n t f p = innerJoin $ (\a b -> _stutWith a b f p) <$> n <* t

_stutWith :: (Num n, Ord n) => n -> Time -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
_stutWith count steptime f p | count <= 1 = p
                             | otherwise = overlay (f (steptime `rotR` _stutWith (count-1) steptime f p)) p

-- | The old name for stutWith
stut' :: Pattern Int -> Pattern Time -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
stut' = stutWith

-- | Turns a pattern of seconds into a pattern of (rational) cycle durations
sec :: Fractional a => Pattern a -> Pattern a
sec p = (realToFrac <$> cF 1 "_cps") *| p

-- | Turns a pattern of milliseconds into a pattern of (rational)
-- cycle durations, according to the current cps.
msec :: Fractional a => Pattern a -> Pattern a
msec p = ((realToFrac . (/1000)) <$> cF 1 "_cps") *| p

_trigger :: Show a => Bool -> a -> Pattern b -> Pattern b
_trigger quant k pat = pat {query = q}
  where q st = query ((offset st) ~> pat) st
        f | quant = (fromIntegral :: Int -> Rational) . round
          | otherwise = id
        offset st = fromMaybe (pure 0) $ do p <- Map.lookup ctrl (controls st)
                                            return $ ((f . fromMaybe 0 . getR) <$> p)
        ctrl = "_t_" ++ show k

trigger :: Show a => a -> Pattern b -> Pattern b
trigger = _trigger False

qtrigger :: Show a => a -> Pattern b -> Pattern b
qtrigger = _trigger True

qt :: Show a => a -> Pattern b -> Pattern b
qt = qtrigger

reset :: Show a => a -> Pattern b -> Pattern b
reset k pat = pat {query = q}
  where q st = query ((offset st) ~> (when (<=0) (const silence) pat)) st
        f = (fromIntegral :: Int -> Rational) . floor
        offset st = fromMaybe (pure 0) $ do p <- Map.lookup ctrl (controls st)
                                            return $ ((f . fromMaybe 0 . getR) <$> p)
        ctrl = "_t_" ++ show k

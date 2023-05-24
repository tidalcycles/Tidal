{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.Signal.Control where
{-
    Control.hs - Functions which concern control signals, which are
    signals of hashmaps, used for synth control values.

    Copyright (C) 2022, Alex McLean and contributors

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

import           Prelude                     hiding ((*>), (<*))

import qualified Data.Map.Strict             as Map
import           Data.Maybe                  (fromJust, fromMaybe, isJust,
                                              mapMaybe)
import           Data.Ratio

import           Sound.Tidal.Arc
import           Sound.Tidal.Compose         ((#), (*|), (|*), (|/), (|=|))
import qualified Sound.Tidal.Params          as P
import           Sound.Tidal.Pattern
import           Sound.Tidal.Signal.Base
import           Sound.Tidal.Signal.Event
import           Sound.Tidal.Signal.Random
import           Sound.Tidal.Signal.Waveform (sine)
import           Sound.Tidal.Stream          (patternTimeID)
import           Sound.Tidal.Types
import           Sound.Tidal.Utils
import           Sound.Tidal.Value

{- | `spin` will "spin" a layer up a signal the given number of times,
with each successive layer offset in time by an additional `1/n` of a
cycle, and panned by an additional `1/n`. The result is a signal that
seems to spin around. This function works best on multichannel
systems.

@
d1 $ slow 3 $ spin 4 $ sound "drum*3 tabla:4 [arpy:2 ~ arpy] [can:2 can:3]"
@
-}
spin :: Signal Int -> ControlSignal -> ControlSignal
spin = _patternify _spin

_spin :: Int -> ControlSignal -> ControlSignal
_spin copies p =
  stack $ map (\i -> let offset = toInteger i % toInteger copies in
                     offset `_early` p
                     # P.pan (pure $ fromRational offset)
              )
          [0 .. (copies - 1)]



{- | `chop` granualizes every sample in place as it is played, turning a signal of samples into a signal of sample parts. Use an integer value to specify how many granules each sample is chopped into:

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

chop :: Signal Int -> ControlSignal -> ControlSignal
chop = _patternify _chop

chopArc :: Arc -> Int -> [Arc]
chopArc (Arc s e) n = map (\i -> Arc (s + (e-s)*(fromIntegral i/fromIntegral n)) (s + (e-s)*(fromIntegral (i+1) / fromIntegral n))) [0 .. n-1]

_chop :: Int -> ControlSignal -> ControlSignal
_chop n = withEvents (concatMap chopEvent)
  where -- for each part,
        chopEvent :: Event ValueMap -> [Event ValueMap]
        chopEvent (Event c (Just w) p' v) = map (chomp c v (length $ chopArc w n)) $ arcs w p'
        -- ignoring 'analog' events (those without wholes),
        chopEvent _ = []
        -- cut whole into n bits, and number them
        arcs w' p' = numberedArcs p' $ chopArc w' n
        -- each bit is a new whole, with part that's the intersection of old part and new whole
        -- (discard new parts that don't intersect with the old part)
        numberedArcs :: Arc -> [Arc] -> [(Int, (Arc, Arc))]
        numberedArcs p' as = map ((fromJust <$>) <$>) $ filter (isJust . snd . snd) $ enumerate $ map (\a -> (a, maybeSect p' a)) as
        -- begin set to i/n, end set to i+1/n
        -- if the old event had a begin and end, then multiply the new
        -- begin and end values by the old difference (end-begin), and
        -- add the old begin
        chomp :: Metadata -> ValueMap -> Int -> (Int, (Arc, Arc)) -> Event ValueMap
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
_chop' :: Int -> ControlSignal -> ControlSignal
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
chunks and signal those chunks. This is where things get towards
granular synthesis. The following cuts a sample into 128 parts, plays
it over 8 cycles and manipulates those parts by reversing and rotating
the loops.

@
d1 $  slow 8 $ striate 128 $ sound "bev"
@
-}

striate :: Signal Int -> ControlSignal -> ControlSignal
striate = _patternify _striate

_striate :: Int -> ControlSignal -> ControlSignal
_striate n p = fastcat $ map offset [0 .. n-1]
  where offset i = mergePlayRange (fromIntegral i / fromIntegral n, fromIntegral (i+1) / fromIntegral n) <$> p
        mergePlayRange :: (Double, Double) -> ValueMap -> ValueMap
        mergePlayRange (b,e) cm = Map.insert "begin" (VF ((b*d')+b')) $ Map.insert "end" (VF ((e*d')+b')) cm
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
striateBy :: Signal Int -> Signal Double -> ControlSignal -> ControlSignal
striateBy = _patternify_p_p _striateBy

-- | DEPRECATED, use 'striateBy' instead.
striate' :: Signal Int -> Signal Double -> ControlSignal -> ControlSignal
striate' = striateBy

_striateBy :: Int -> Double -> ControlSignal -> ControlSignal
_striateBy n f p = fastcat $ map (offset . fromIntegral) [0 .. n-1]
  where offset i = p # P.begin (pure (slot * i) :: Signal Double) # P.end (pure ((slot * i) + f) :: Signal Double)
        slot = (1 - f) / fromIntegral n

{- | `chopgap` is similar to `chop` in that it granualizes every sample in place as it is played,
but every other grain is silent. Use an integer value to specify how many granules
each sample is chopped into:

@
d1 $ chopgap 8 $ sound "jvbass"
d1 $ chopgap 16 $ sound "[jvbass drum:4]"
@-}

chopgap :: Signal Int -> ControlSignal -> ControlSignal
chopgap = _patternify _chopgap

_chopgap :: Int -> ControlSignal -> ControlSignal
_chopgap n p = _fast (toRational n) (cat [pure 1, silence]) |=| _chop n p

{- |
`weave` applies a function smoothly over an array of different signals. It uses an `OscSignal` to
apply the function at different levels to each signal, creating a weaving effect.

@
d1 $ weave 3 (shape $ sine1) [sound "bd [sn drum:2*2] bd*2 [sn drum:1]", sound "arpy*8 ~"]
@
-}
weave :: Time -> ControlSignal -> [ControlSignal] -> ControlSignal
weave t p ps = weave' t p (map (#) ps)

{- | `weaveWith` is similar in that it blends functions at the same time at different amounts over a signal:

@
d1 $ weaveWith 3 (sound "bd [sn drum:2*2] bd*2 [sn drum:1]") [density 2, (# speed "0.5"), chop 16]
@
-}
weaveWith :: Time -> Signal a -> [Signal a -> Signal a] -> Signal a
weaveWith t p fs | l == 0 = silence
              | otherwise = _slow t $ stack $ zipWith (\ i f -> (fromIntegral i % l) `_early` _fast t (f (_slow t p))) [0 :: Int ..] fs
  where l = fromIntegral $ length fs

weave' :: Time -> Signal a -> [Signal a -> Signal a] -> Signal a
weave' = weaveWith

{- |
(A function that takes two ControlSignals, and blends them together into
a new ControlSignal. An ControlSignal is basically a signal of messages to
a synthesiser.)

Shifts between the two given signals, using distortion.

Example:

@
d1 $ interlace (sound  "bd sn kurt") (every 3 rev $ sound  "bd sn:2")
@
-}
interlace :: ControlSignal -> ControlSignal -> ControlSignal
interlace a b = weave 16 (P.shape (sine * 0.9)) [a, b]

{-
{- | Just like `striate`, but also loops each sample chunk a number of times specified in the second argument.
The primed version is just like `striateBy`, where the loop count is the third argument. For example:

@
d1 $ striateL' 3 0.125 4 $ sound "feel sn:2"
@

Like `striate`, these use the `begin` and `end` parameters internally, as well as the `loop` parameter for these versions.
-}
striateL :: Signal Int -> Signal Int -> ControlSignal -> ControlSignal
striateL = _patternify_p_p _striateL

striateL' :: Signal Int -> Signal Double -> Signal Int -> ControlSignal -> ControlSignal
striateL' = _patternify_p_p_p _striateL'

_striateL :: Int -> Int -> ControlSignal -> ControlSignal
_striateL n l p = _striate n p # loop (pure $ fromIntegral l)
_striateL' n f l p = _striateBy n f p # loop (pure $ fromIntegral l)


en :: [(Int, Int)] -> Signal String -> Signal String
en ns p = stack $ map (\(i, (k, n)) -> _e k n (samples p (pure i))) $ enumerate ns

-}

slice :: Signal Int -> Signal Int -> ControlSignal -> ControlSignal
slice pN pI p = P.begin b # P.end e # p
  where b = div' <$> pI <* pN
        e = (\i n -> div' i n + div' 1 n) <$> pI <* pN
        div' num den = fromIntegral (num `mod` den) / fromIntegral den

_slice :: Int -> Int -> ControlSignal -> ControlSignal
_slice n i p =
      p
      # P.begin (pure $ fromIntegral i / fromIntegral n)
      # P.end (pure $ fromIntegral (i+1) / fromIntegral n)

randslice :: Signal Int -> ControlSignal -> ControlSignal
randslice = _patternify $ \n p -> innerJoin $ (\i -> _slice n i p) <$> _irand n

_splice :: Int -> Signal Int -> ControlSignal -> Signal (Map.Map String Value)
_splice bits ipat pat = withEvent f (slice (pure bits) ipat pat) # P.unit (pure "c")
  where f ev = case Map.lookup "speed" (value ev) of
                        (Just (VF s)) -> ev {value = Map.insert "speed" (VF $ d*s) (value ev)}  -- if there is a speed parameter already present
                        _ -> ev {value = Map.insert "speed" (VF d) (value ev)}
          where d = sz / fromRational (wholeEnd ev - wholeBegin ev)
                sz = 1/fromIntegral bits

splice :: Signal Int -> Signal Int -> ControlSignal -> Signal (Map.Map String Value)
splice bitpat ipat pat = innerJoin $ (\bits -> _splice bits ipat pat) <$> bitpat

{- |
`loopAt` makes a sample fit the given number of cycles. Internally, it
works by setting the `unit` parameter to "c", changing the playback
speed of the sample with the `speed` parameter, and setting setting
the `density` of the signal to match.

@
d1 $ loopAt 4 $ sound "breaks125"
d1 $ juxBy 0.6 (|* speed "2") $ slowspread (loopAt) [4,6,2,3] $ chop 12 $ sound "fm:14"
@
-}
loopAt :: Signal Time -> ControlSignal -> ControlSignal
loopAt n p = slow n p |* P.speed (fromRational <$> (1/n)) # P.unit (pure "c")

hurry :: Signal Rational -> ControlSignal -> ControlSignal
hurry !x = (|* P.speed (fromRational <$> x)) . fast x

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

smash :: Signal Int -> [Signal Time] -> ControlSignal -> Signal ValueMap
smash n xs p = slowcat $ map (`slow` p') xs
  where p' = striate n p

{- | an altenative form to `smash` is `smash'` which will use `chop` instead of `striate`.
-}
smash' :: Int -> [Signal Time] -> ControlSignal -> ControlSignal
smash' n xs p = slowcat $ map (`slow` p') xs
  where p' = _chop n p

{- |
    Applies a type of delay to a signal.
    It has three parameters, which could be called depth, time and feedback.

    This adds a bit of echo:
    @
    d1 $ echo 4 0.2 0.5 $ sound "bd sn"
    @

    The above results in 4 echos, each one 50% quieter than the last, with 1/5th of a cycle between them.

    It is possible to reverse the echo:
    @
    d1 $ echo 4 (-0.2) 0.5 $ sound "bd sn"
    @
-}
echo :: Signal Integer -> Signal Rational -> Signal Double -> ControlSignal -> ControlSignal
echo = _patternify_p_p_p _echo

_echo :: Integer -> Rational -> Double -> ControlSignal -> ControlSignal
_echo count time feedback p = _echoWith count time (|* P.gain (pure feedback)) p

{- |
    Allows to apply a function for each step and overlays the result delayed by the given time.

    @
    d1 $ echoWith 2 "1%3" (# vowel "{a e i o u}%2") $ sound "bd sn"
    @

    In this case there are two _overlays_ delayed by 1/3 of a cycle, where each has the @vowel@ filter applied.
-}
echoWith :: Signal Int -> Signal Time -> (Signal a -> Signal a) -> Signal a -> Signal a
echoWith n t f p = innerJoin $ (\a b -> _echoWith a b f p) <$> n <* t

_echoWith :: (Num n, Ord n) => n -> Time -> (Signal a -> Signal a) -> Signal a -> Signal a
_echoWith count time f p | count <= 1 = p
                         | otherwise = overlay (f (time `_late` _echoWith (count-1) time f p)) p

{-

-- | DEPRECATED, use 'echo' instead
stut :: Signal Integer -> Signal Double -> Signal Rational -> ControlSignal -> ControlSignal
stut = _patternify_p_p_p _stut

_stut :: Integer -> Double -> Rational -> ControlSignal -> ControlSignal
_stut count feedback steptime p = stack (p:map (\x -> ((x%1)*steptime) `_late` (p |* P.gain (pure $ scalegain (fromIntegral x)))) [1..(count-1)])
  where scalegain
          = (+feedback) . (*(1-feedback)) . (/ fromIntegral count) . (fromIntegral count -)

-- | DEPRECATED, use 'echoWith' instead
stutWith :: Signal Int -> Signal Time -> (Signal a -> Signal a) -> Signal a -> Signal a
stutWith n t f p = innerJoin $ (\a b -> _stutWith a b f p) <$> n <* t

_stutWith :: (Num n, Ord n) => n -> Time -> (Signal a -> Signal a) -> Signal a -> Signal a
_stutWith count steptime f p | count <= 1 = p
                             | otherwise = overlay (f (steptime `_late` _stutWith (count-1) steptime f p)) p

-- | DEPRECATED, use 'echoWith' instead
stut' :: Signal Int -> Signal Time -> (Signal a -> Signal a) -> Signal a -> Signal a
stut' = stutWith

-}

-- | Turns a signal of seconds into a signal of (rational) cycle durations
sec :: Fractional a => Signal a -> Signal a
sec p = (realToFrac <$> cF 1 "_cps") *| p

-- | Turns a signal of milliseconds into a signal of (rational)
-- cycle durations, according to the current cps.
msec :: Fractional a => Signal a -> Signal a
msec p = (realToFrac . (/1000) <$> cF 1 "_cps") *| p

-- | Align the start of a pattern with the time a pattern is evaluated,
-- rather than the global start time. Because of this, the pattern will
-- probably not be aligned to the cycle grid.
trigger :: Signal a -> Signal a
trigger = triggerWith id

-- | (Alias @__qt__@) Quantise trigger. Aligns the start of the pattern
-- with the next cycle boundary. For example, this pattern will fade in
-- starting with the next cycle after the pattern is evaluated:
--
-- @
-- d1 $ qtrigger $ s "hh(5, 8)" # amp envL
-- @
--
-- Note that the pattern will start playing immediately. The /start/ of the
-- pattern aligns with the next cycle boundary, but events will play before
-- if the pattern has events at negative timestamps (which most loops do).
-- These events can be filtered out, for example:
--
-- @
-- d1 $ qtrigger $ filterWhen (>= 0) $ s "hh(5, 8)"
-- @
qtrigger :: Signal a -> Signal a
qtrigger = ctrigger

qt :: Signal a -> Signal a
qt = qtrigger

-- | Ceiling trigger. Aligns the start of a pattern to the next cycle
-- boundary, just like 'qtrigger'.
ctrigger :: Signal a -> Signal a
ctrigger = triggerWith $ (fromIntegral :: Int -> Rational) . ceiling

-- | Rounded trigger. Aligns the start of a pattern to the nearest cycle
-- boundary, either next or previous.
rtrigger :: Signal a -> Signal a
rtrigger = triggerWith $ (fromIntegral :: Int -> Rational) . round

-- | Floor trigger. Aligns the start of a pattern to the previous cycle
-- boundary.
ftrigger :: Signal a -> Signal a
ftrigger = triggerWith $ (fromIntegral :: Int -> Rational) . floor

-- | (Alias @__mt__@) Mod trigger. Aligns the start of a pattern to the
-- next cycle boundary where the cycle is evenly divisible by a given
-- number. 'qtrigger' is equivalent to @mtrigger 1@.
mtrigger :: Int -> Signal a -> Signal a
mtrigger n = triggerWith $ fromIntegral . nextMod
  where nextMod t = n * ceiling (t / fromIntegral n)

mt :: Int -> Signal a -> Signal a
mt = mtrigger

-- | This aligns the start of a pattern to some value relative to the
-- time the pattern is evaluated. The provided function maps the evaluation
-- time (on the global cycle clock) to a new time, and then @triggerWith@
-- aligns the pattern's start to the time that's returned.
triggerWith :: (Time -> Time) -> Signal a -> Signal a
triggerWith f pat = pat {query = q}
  where q st = query (_early (offset st) pat) st
        offset st = maybe 0 f $ Map.lookup patternTimeID (sControls st) >>= getR

splat :: Signal Int -> ControlSignal -> ControlSignal -> ControlSignal
splat slices epat pat = chop slices pat # bite 1 (const 0 <$> pat) epat


-- | @contrast p f f' p'@ splits controlpattern @p'@ in two, applying
-- the function @f@ to one and @f'@ to the other. This depends on
-- whether events in it contains values matching with those in @p@.
-- For example in @contrast (# crush 3) (# vowel "a") (n "1") $ n "0 1" # s "bd sn" # speed 3@,
-- the first event will have the vowel effect applied and the second
-- will have the crush applied.
contrast :: (ControlSignal -> ControlSignal) -> (ControlSignal -> ControlSignal)
            -> ControlSignal -> ControlSignal -> ControlSignal
contrast = contrastBy (==)

contrastBy :: (a -> Value -> Bool)
              -> (ControlSignal -> Signal b)
              -> (ControlSignal -> Signal b)
              -> Signal (Map.Map String a)
              -> Signal (Map.Map String Value)
              -> Signal b
contrastBy comp f f' p p' = overlay (f matched) (f' unmatched)
  where matches = matchManyToOne (flip $ Map.isSubmapOfBy comp) p p'
        matched :: ControlSignal
        matched = filterJusts $ (\(t, a) -> if t then Just a else Nothing) <$> matches
        unmatched :: ControlSignal
        unmatched = filterJusts $ (\(t, a) -> if not t then Just a else Nothing) <$> matches

        -- | Mark values in the first pattern which match with at least one
        -- value in the second pattern.
        matchManyToOne :: (b -> a -> Bool) -> Signal a -> Signal b -> Signal (Bool, b)
        matchManyToOne f'' pa pb = pa {query = q}
          where q st = map match $ query pb st
                  where
                    match ex@(Event xContext xWhole xActive x) =
                      Event (mconcat $ xContext:map metadata as') xWhole xActive (any (f'' x . value) as', x)
                      where as' = as $ aBegin $ wholeOrActive ex
                    as s = query pa $ fQuery s
                    fQuery s = st {sArc = Arc s s}

contrastRange
  :: (ControlSignal -> Signal a)
     -> (ControlSignal -> Signal a)
     -> Signal (Map.Map String (Value, Value))
     -> ControlSignal
     -> Signal a
contrastRange = contrastBy f
      where f (VI s, VI e) (VI v) = v >= s && v <= e
            f (VF s, VF e) (VF v) = v >= s && v <= e
            f (VN s, VN e) (VN v) = v >= s && v <= e
            f (VS s, VS e) (VS v) = v == s && v == e
            f _ _                 = False

-- | Like @contrast@, but one function is given, and applied to events with matching controls.
fix :: (ControlSignal -> ControlSignal) -> ControlSignal -> ControlSignal -> ControlSignal
fix f = contrast f id

-- | Like @contrast@, but one function is given, and applied to events
-- with controls which don't match.
unfix :: (ControlSignal -> ControlSignal) -> ControlSignal -> ControlSignal -> ControlSignal
unfix = contrast id

fixRange :: (ControlSignal -> Signal ValueMap)
            -> Signal (Map.Map String (Value, Value))
            -> ControlSignal
            -> ControlSignal
fixRange f = contrastRange f id

unfixRange :: (ControlSignal -> Signal ValueMap)
              -> Signal (Map.Map String (Value, Value))
              -> ControlSignal
              -> ControlSignal
unfixRange = contrastRange id

squeezeJoinUp :: Signal ControlSignal -> ControlSignal
squeezeJoinUp pp = pp {query = q}
  where q st = concatMap (f st) (query (discreteOnly pp) st)
        f st (Event meta (Just w) p v) =
          mapMaybe (munge meta w p) $ query (_compressArc (cycleArc w) (v |* P.speed (pure $ fromRational $ 1/(aEnd w - aBegin w)))) st {sArc = p}
        -- already ignoring analog events, but for completeness..
        f _ _ = []
        munge oMeta oWhole oActive (Event iMeta (Just iWhole) iActive v) =
          do w' <- maybeSect oWhole iWhole
             p' <- maybeSect oActive iActive
             return (Event (iMeta <> oMeta) (Just w') p' v)
        munge _ _ _ _ = Nothing

_chew :: Int -> Signal Int -> ControlSignal  -> ControlSignal
_chew n ipat pat = squeezeJoinUp (zoompat <$> ipat) |/ P.speed (pure $ fromIntegral n)
  where zoompat i = _zoomArc (Arc (i' / fromIntegral n) ((i'+1) / fromIntegral n)) pat
           where i' = fromIntegral $ i `mod` n

-- TODO maybe _chew could signal the first parameter directly..
chew :: Signal Int -> Signal Int -> ControlSignal  -> ControlSignal
chew npat ipat pat = innerJoin $ (\n -> _chew n ipat pat) <$> npat

grain :: Signal Double -> Signal Double -> ControlSignal
grain s w = P.begin b # P.end e
  where b = s
        e = s + w

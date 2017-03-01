{-# LANGUAGE DeriveDataTypeable #-}

module Sound.Tidal.Pattern where

import Control.Applicative
import Data.Monoid
import Data.Fixed
import Data.List
import Data.Maybe
import Data.Ord
import Data.Ratio
import Debug.Trace
import Data.Typeable
import Data.Function
import System.Random.Mersenne.Pure64
import Data.Char
import qualified Data.Text as T

import Sound.Tidal.Time
import Sound.Tidal.Utils
import Sound.Tidal.Bjorklund

-- | The pattern datatype, a function from a time @Arc@ to @Event@
-- values. For discrete patterns, this returns the events which are
-- active during that time. For continuous patterns, events with
-- values for the midpoint of the given @Arc@ is returned.
data Pattern a = Pattern {arc :: Arc -> [Event a]}
  deriving Typeable

-- | @show (p :: Pattern)@ returns a text string representing the
-- event values active during the first cycle of the given pattern.
instance (Show a) => Show (Pattern a) where
  show p@(Pattern _) = intercalate " " $ map showEvent $ arc p (0, 1)

-- | converts a ratio into human readable string, e.g. @1/3@
showTime :: (Show a, Integral a) => Ratio a -> String
showTime t | denominator t == 1 = show (numerator t)
           | otherwise = show (numerator t) ++ ('/':show (denominator t))

-- | converts a time arc into human readable string, e.g. @1/3 3/4@
showArc :: Arc -> String
showArc a = concat[showTime $ fst a, (' ':showTime (snd a))]

-- | converts an event into human readable string, e.g. @("bd" 1/4 2/3)@
showEvent :: (Show a) => Event a -> String
showEvent (a, b, v) | a == b = concat["(",show v,
                                      (' ':showArc a),
                                      ")"
                                     ]
                    | otherwise = show v

instance Functor Pattern where
  fmap f (Pattern a) = Pattern $ fmap (fmap (mapThd' f)) a

-- | @pure a@ returns a pattern with an event with value @a@, which
-- has a duration of one cycle, and repeats every cycle.
instance Applicative Pattern where
  pure x = Pattern $ \(s, e) -> map
                                (\t -> ((t%1, (t+1)%1),
                                        (t%1, (t+1)%1),
                                        x
                                       )
                                )
                                [floor s .. ((ceiling e) - 1)]
  (Pattern fs) <*> (Pattern xs) =
    Pattern $ \a -> concatMap applyX (fs a)
    where applyX ((s,e), (s', e'), f) =
            map (\(_, _, x) -> ((s,e), (s', e'), f x))
                (filter
                 (\(_, a', _) -> isIn a' s)
                 (xs (s',e'))
                )

-- | @mempty@ is a synonym for @silence@.
-- | @mappend@ is a synonym for @overlay@.
instance Monoid (Pattern a) where
    mempty = silence
    mappend = overlay

instance Monad Pattern where
  return = pure
  -- Pattern a -> (a -> Pattern b) -> Pattern b
  -- Pattern Char -> (Char -> Pattern String) -> Pattern String

  p >>= f = unwrap (f <$> p)
{-Pattern (\a -> concatMap
                   (\((s,e), (s',e'), x) -> map (\ev -> ((s,e), (s',e'), thd' ev)) $
                                            filter
                                            (\(a', _, _) -> isIn a' s)
                                            (arc (f x) (s,e))
                   )
                   (arc p a)
             )
-}
-- join x = x >>= id


-- Take a pattern, and function from elements in the pattern to another pattern,
-- and then return that pattern
--bind :: Pattern a -> (a -> Pattern b) -> Pattern b
--bind p f =

-- this is actually join
unwrap :: Pattern (Pattern a) -> Pattern a
unwrap p = Pattern $ \a -> concatMap ((\p' -> arc p' a) . thd') (arc p a)

-- | @atom@ is a synonym for @pure@.
atom :: a -> Pattern a
atom = pure

-- | @silence@ returns a pattern with no events.
silence :: Pattern a
silence = Pattern $ const []

-- | @withQueryArc f p@ returns a new @Pattern@ with function @f@
-- applied to the @Arc@ values passed to the original @Pattern@ @p@.
withQueryArc :: (Arc -> Arc) -> Pattern a -> Pattern a
withQueryArc f p = Pattern $ \a -> arc p (f a)

-- | @withQueryTime f p@ returns a new @Pattern@ with function @f@
-- applied to the both the start and end @Time@ of the @Arc@ passed to
-- @Pattern@ @p@.
withQueryTime :: (Time -> Time) -> Pattern a -> Pattern a
withQueryTime = withQueryArc . mapArc

-- | @withResultArc f p@ returns a new @Pattern@ with function @f@
-- applied to the @Arc@ values in the events returned from the
-- original @Pattern@ @p@.
withResultArc :: (Arc -> Arc) -> Pattern a -> Pattern a
withResultArc f p = Pattern $ \a -> mapArcs f $ arc p a

-- | @withResultTime f p@ returns a new @Pattern@ with function @f@
-- applied to the both the start and end @Time@ of the @Arc@ values in
-- the events returned from the original @Pattern@ @p@.
withResultTime :: (Time -> Time) -> Pattern a -> Pattern a
withResultTime = withResultArc . mapArc

-- | @overlay@ combines two @Pattern@s into a new pattern, so that
-- their events are combined over time. This is the same as the infix
-- operator `<>`.
overlay :: Pattern a -> Pattern a -> Pattern a
overlay p p' = Pattern $ \a -> (arc p a) ++ (arc p' a)

-- | @stack@ combines a list of @Pattern@s into a new pattern, so that
-- their events are combined over time.
stack :: [Pattern a] -> Pattern a
stack ps = foldr overlay silence ps

-- | @append@ combines two patterns @Pattern@s into a new pattern, so
-- that the events of the second pattern are appended to those of the
-- first pattern, within a single cycle

append :: Pattern a -> Pattern a -> Pattern a
append a b = cat [a,b]

-- | @append'@ does the same as @append@, but over two cycles, so that
-- the cycles alternate between the two patterns.
append' :: Pattern a -> Pattern a -> Pattern a
append' a b  = slow 2 $ cat [a,b]

-- | @cat@ returns a new pattern which interlaces the cycles of the
-- given patterns, within a single cycle. It's the equivalent of
-- @append@, but with a list of patterns.
cat :: [Pattern a] -> Pattern a
cat ps = density (fromIntegral $ length ps) $ slowcat ps


splitAtSam :: Pattern a -> Pattern a
splitAtSam p =
  splitQueries $ Pattern $ \(s,e) -> mapSnds' (trimArc (sam s)) $ arc p (s,e)
  where trimArc s' (s,e) = (max (s') s, min (s'+1) e)

-- | @slowcat@ does the same as @cat@, but maintaining the duration of
-- the original patterns. It is the equivalent of @append'@, but with
-- a list of patterns.

slowcat :: [Pattern a] -> Pattern a
slowcat [] = silence
slowcat ps = splitQueries $ Pattern f
  where ps' = map splitAtSam ps
        l = length ps'
        f (s,e) = arc (withResultTime (+offset) p) (s',e')
          where p = ps' !! n
                r = (floor s) :: Int
                n = (r `mod` l) :: Int
                offset = (fromIntegral $ r - ((r - n) `div` l)) :: Time
                (s', e') = (s-offset, e-offset)

-- | @listToPat@ turns the given list of values to a Pattern, which
-- cycles through the list.
listToPat :: [a] -> Pattern a
listToPat = cat . map atom

-- | @maybeListToPat@ is similar to @listToPat@, but allows values to
-- be optional using the @Maybe@ type, so that @Nothing@ results in
-- gaps in the pattern.
maybeListToPat :: [Maybe a] -> Pattern a
maybeListToPat = cat . map f
  where f Nothing = silence
        f (Just x) = atom x

-- | @run@ @n@ returns a pattern representing a cycle of numbers from @0@ to @n-1@.
run n = listToPat [0 .. n-1]
scan n = cat $ map run [1 .. n]

-- | @density@ returns the given pattern with density increased by the
-- given @Time@ factor. Therefore @density 2 p@ will return a pattern
-- that is twice as fast, and @density (1%3) p@ will return one three
-- times as slow.
density :: Time -> Pattern a -> Pattern a
density 0 _ = silence
density 1 p = p
density r p = withResultTime (/ r) $ withQueryTime (* r) p


-- | @densityGap@ is similar to @density@ but maintains its cyclic
-- alignment. For example, @densityGap 2 p@ would squash the events in
-- pattern @p@ into the first half of each cycle (and the second
-- halves would be empty).
densityGap :: Time -> Pattern a -> Pattern a
densityGap 0 _ = silence
densityGap r p = splitQueries $ withResultArc (\(s,e) -> (sam s + ((s - sam s)/r), (sam s + ((e - sam s)/r)))) $ Pattern (\a -> arc p $ mapArc (\t -> sam t + (min 1 (r * cyclePos t))) a)

-- | @slow@ does the opposite of @density@, i.e. @slow 2 p@ will
-- return a pattern that is half the speed.
slow :: Time -> Pattern a -> Pattern a
slow 0 = id
slow t = density (1/t)

-- | The @<~@ operator shifts (or rotates) a pattern to the left (or
-- counter-clockwise) by the given @Time@ value. For example
-- @(1%16) <~ p@ will return a pattern with all the events moved
-- one 16th of a cycle to the left.
(<~) :: Time -> Pattern a -> Pattern a
(<~) t p = withResultTime (subtract t) $ withQueryTime (+ t) p

-- | The @~>@ operator does the same as @<~@ but shifts events to the
-- right (or clockwise) rather than to the left.
(~>) :: Time -> Pattern a -> Pattern a
(~>) = (<~) . (0-)

{- | (The above means that `brak` is a function from patterns of any type,
to a pattern of the same type.)

Make a pattern sound a bit like a breakbeat

Example:

@
d1 $ sound (brak "bd sn kurt")
@
-}
brak :: Pattern a -> Pattern a
brak = when ((== 1) . (`mod` 2)) (((1%4) ~>) . (\x -> cat [x, silence]))

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
iter :: Int -> Pattern a -> Pattern a
iter n p = slowcat $ map (\i -> ((fromIntegral i)%(fromIntegral n)) <~ p) [0 .. (n-1)]

-- | @iter'@ is the same as @iter@, but decrements the starting
-- subdivision instead of incrementing it.
iter' :: Int -> Pattern a -> Pattern a
iter' n p = slowcat $ map (\i -> ((fromIntegral i)%(fromIntegral n)) ~> p) [0 .. (n-1)]

-- | @rev p@ returns @p@ with the event positions in each cycle
-- reversed (or mirrored).
rev :: Pattern a -> Pattern a
rev p = splitQueries $ Pattern $ \a -> mapArcs mirrorArc (arc p (mirrorArc a))

-- | @palindrome p@ applies @rev@ to @p@ every other cycle, so that
-- the pattern alternates between forwards and backwards.
palindrome p = append' p (rev p)

{-|
Only `when` the given test function returns `True` the given pattern transformation is applied. The test function will be called with the current cycle as a number.

@
d1 $ when ((elem '4').show)
  (striate 4)
  $ sound "hh hc"
@

The above will only apply `striate 4` to the pattern if the current cycle number contains the number 4. So the fourth cycle will be striated and the fourteenth and so on. Expect lots of striates after cycle number 399.
-}
when :: (Int -> Bool) -> (Pattern a -> Pattern a) ->  Pattern a -> Pattern a
when test f p = splitQueries $ Pattern apply
  where apply a | test (floor $ fst a) = (arc $ f p) a
                | otherwise = (arc p) a

whenT :: (Time -> Bool) -> (Pattern a -> Pattern a) ->  Pattern a -> Pattern a
whenT test f p = splitQueries $ Pattern apply
  where apply a | test (fst a) = (arc $ f p) a
                | otherwise = (arc p) a

playWhen :: (Time -> Bool) -> Pattern a -> Pattern a
playWhen test (Pattern f) = Pattern $ (filter (\e -> test (eventOnset e))) . f

playFor :: Time -> Time -> Pattern a -> Pattern a
playFor s e = playWhen (\t -> and [t >= s, t < e])

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
seqP ps = stack $ map (\(s, e, p) -> playFor s e ((sam s) ~> p)) ps

-- | @every n f p@ applies the function @f@ to @p@, but only affects
-- every @n@ cycles.
every :: Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
every 0 _ p = p
every n f p = when ((== 0) . (`mod` n)) f p

-- | @every n o f'@ is like @every n f@ with an offset of @o@ cycles
every' :: Int -> Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
every' n o f = when ((== o) . (`mod` n)) f

-- | @foldEvery ns f p@ applies the function @f@ to @p@, and is applied for
-- each cycle in @ns@.
foldEvery :: [Int] -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
foldEvery ns f p = foldr ($) p (map (\x -> every x f) ns)

-- | @sig f@ takes a function from time to values, and turns it into a
-- @Pattern@.
sig :: (Time -> a) -> Pattern a
sig f = Pattern f'
  where f' (s,e) | s > e = []
                 | otherwise = [((s,e), (s,e), f s)]

-- | @sinewave@ returns a @Pattern@ of continuous @Double@ values following a
-- sinewave with frequency of one cycle, and amplitude from -1 to 1.
sinewave :: Pattern Double
sinewave = sig $ \t -> sin $ pi * 2 * (fromRational t)
-- | @sine@ is a synonym for @sinewave@.
sine = sinewave
-- | @sinerat@ is equivalent to @sinewave@ for @Rational@ values,
-- suitable for use as @Time@ offsets.
sinerat = fmap toRational sine
-- | @ratsine@ is a synonym for @sinerat@.
ratsine = sinerat

-- | @sinewave1@ is equivalent to @sinewave@, but with amplitude from 0 to 1.
sinewave1 :: Pattern Double
sinewave1 = fmap ((/ 2) . (+ 1)) sinewave

-- | @sine1@ is a synonym for @sinewave1@.
sine1 = sinewave1

-- | @sinerat1@ is equivalent to @sinerat@, but with amplitude from 0 to 1.
sinerat1 = fmap toRational sine1

-- | @sineAmp1 d@ returns @sinewave1@ with its amplitude offset by @d@.
sineAmp1 :: Double -> Pattern Double
sineAmp1 offset = (+ offset) <$> sinewave1

-- | @sawwave@ is the equivalent of @sinewave@ for (ascending) sawtooth waves.
sawwave :: Pattern Double
sawwave = ((subtract 1) . (* 2)) <$> sawwave1

-- | @saw@ is a synonym for @sawwave@.
saw = sawwave

-- | @sawrat@ is the same as @sawwave@ but returns @Rational@ values
-- suitable for use as @Time@ offsets.
sawrat = fmap toRational saw

-- | @sawwave1@ is the equivalent of @sinewave1@ for (ascending) sawtooth waves.
sawwave1 :: Pattern Double
sawwave1 = sig $ \t -> mod' (fromRational t) 1

-- | @saw1@ is a synonym for @sawwave1@.
saw1 = sawwave1

-- | @sawrat1@ is the same as @sawwave1@ but returns @Rational@ values
-- suitable for use as @Time@ offsets.
sawrat1 = fmap toRational saw1

-- | @triwave@ is the equivalent of @sinewave@ for triangular waves.
triwave :: Pattern Double
triwave = ((subtract 1) . (* 2)) <$> triwave1

-- | @tri@ is a synonym for @triwave@.
tri = triwave

-- | @trirat@ is the same as @triwave@ but returns @Rational@ values
-- suitable for use as @Time@ offsets.
trirat = fmap toRational tri

-- | @triwave1@ is the equivalent of @sinewave1@ for triangular waves.
triwave1 :: Pattern Double
triwave1 = append sawwave1 (rev sawwave1)

-- | @tri1@ is a synonym for @triwave1@.
tri1 = triwave1

-- | @trirat1@ is the same as @triwave1@ but returns @Rational@ values
-- suitable for use as @Time@ offsets.
trirat1 = fmap toRational tri1

-- | @squarewave1@ is the equivalent of @sinewave1@ for square waves.
squarewave1 :: Pattern Double
squarewave1 = sig $
              \t -> fromIntegral $ floor $ (mod' (fromRational t) 1) * 2

-- | @square1@ is a synonym for @squarewave1@.
square1 = squarewave1

-- | @squarewave@ is the equivalent of @sinewave@ for square waves.
squarewave :: Pattern Double
squarewave = ((subtract 1) . (* 2)) <$> squarewave1

-- | @square@ is a synonym for @squarewave@.
square = squarewave

-- | @envL@ is a @Pattern@ of continuous @Double@ values, representing
-- a linear interpolation between 0 and 1 during the first cycle, then
-- staying constant at 1 for all following cycles. Possibly only
-- useful if you're using something like the retrig function defined
-- in tidal.el.
envL :: Pattern Double
envL = sig $ \t -> max 0 $ min (fromRational t) 1

-- like envL but reversed.
envLR :: Pattern Double
envLR = (1-) <$> envL

-- 'Equal power' for gain-based transitions
envEq :: Pattern Double
envEq = sig $ \t -> sqrt (sin (pi/2 * (max 0 $ min (fromRational (1-t)) 1)))
-- Equal power reversed
envEqR = sig $ \t -> sqrt (cos (pi/2 * (max 0 $ min (fromRational (1-t)) 1)))

fadeOut :: Time -> Pattern a -> Pattern a
fadeOut n = spread' (degradeBy) (slow n $ envL)

-- Alternate versions where you can provide the time from which the fade starts
fadeOut' :: Time -> Time -> Pattern a -> Pattern a
fadeOut' from dur p = spread' (degradeBy) (from ~> slow dur envL) p

-- The 1 <~ is so fade ins and outs have different degredations
fadeIn' :: Time -> Time -> Pattern a -> Pattern a
fadeIn' from dur p = spread' (\n p -> 1 <~ degradeBy n p) (from ~> slow dur ((1-) <$> envL)) p

fadeIn :: Time -> Pattern a -> Pattern a
fadeIn n = spread' (degradeBy) (slow n $ (1-) <$> envL)

{- | (The above is difficult to describe, if you don't understand Haskell,
just read the description and examples..)

The `spread` function allows you to take a pattern transformation
which takes a parameter, such as `slow`, and provide several
parameters which are switched between. In other words it 'spreads' a
function across several values.

Taking a simple high hat loop as an example:

@
d1 $ sound "ho ho:2 ho:3 hc"
@

We can slow it down by different amounts, such as by a half:

@
d1 $ slow 2 $ sound "ho ho:2 ho:3 hc"
@

Or by four thirds (i.e. speeding it up by a third; `4%3` means four over
three):

@
d1 $ slow (4%3) $ sound "ho ho:2 ho:3 hc"
@

But if we use `spread`, we can make a pattern which alternates between
the two speeds:

@
d1 $ spread slow [2,4%3] $ sound "ho ho:2 ho:3 hc"
@

Note that if you pass ($) as the function to spread values over, you
can put functions as the list of values. For example:

@
d1 $ spread ($) [density 2, rev, slow 2, striate 3, (# speed "0.8")]
    $ sound "[bd*2 [~ bd]] [sn future]*2 cp jvbass*4"
@

Above, the pattern will have these transforms applied to it, one at a time, per cycle:

* cycle 1: `density 2` - pattern will increase in speed
* cycle 2: `rev` - pattern will be reversed
* cycle 3: `slow 2` - pattern will decrease in speed
* cycle 4: `striate 3` - pattern will be granualized
* cycle 5: `(# speed "0.8")` - pattern samples will be played back more slowly

After `(# speed "0.8")`, the transforms will repeat and start at `density 2` again.
-}
spread :: (a -> t -> Pattern b) -> [a] -> t -> Pattern b
spread f xs p = slowcat $ map (\x -> f x p) xs

slowspread = spread

{- | @fastspread@ works the same as @spread@, but the result is squashed into a single cycle. If you gave four values to @spread@, then the result would seem to speed up by a factor of four. Compare these two:

d1 $ spread chop [4,64,32,16] $ sound "ho ho:2 ho:3 hc"

d1 $ fastspread chop [4,64,32,16] $ sound "ho ho:2 ho:3 hc"

There is also @slowspread@, which is an alias of @spread@.
-}
fastspread :: (a -> t -> Pattern b) -> [a] -> t -> Pattern b
fastspread f xs p = cat $ map (\x -> f x p) xs

{- | There's a version of this function, `spread'` (pronounced "spread prime"), which takes a *pattern* of parameters, instead of a list:

@
d1 $ spread' slow "2 4%3" $ sound "ho ho:2 ho:3 hc"
@

This is quite a messy area of Tidal - due to a slight difference of
implementation this sounds completely different! One advantage of
using `spread'` though is that you can provide polyphonic parameters, e.g.:

@
d1 $ spread' slow "[2 4%3, 3]" $ sound "ho ho:2 ho:3 hc"
@
-}
spread' :: (a -> Pattern b -> Pattern c) -> Pattern a -> Pattern b -> Pattern c
spread' f vpat pat = do v <- vpat
                        f v pat

{- | `spreadChoose f xs p` is similar to `slowspread` but picks values from
`xs` at random, rather than cycling through them in order. It has a
shorter alias `spreadr`.
-}
spreadChoose :: (t -> t1 -> Pattern b) -> [t] -> t1 -> Pattern b
spreadChoose f vs p = do v <- discretise 1 (choose vs)
                         f v p
spreadr = spreadChoose

filterValues :: (a -> Bool) -> Pattern a -> Pattern a
filterValues f (Pattern x) = Pattern $ (filter (f . thd')) . x

-- Filter out events that have had their onsets cut off
filterOnsets :: Pattern a -> Pattern a
filterOnsets (Pattern f) =
  Pattern $ (filter (\e -> eventOnset e >= eventStart e)) . f

-- Filter events which have onsets, which are within the given range
-- TODO - what about < e ??
filterStartInRange :: Pattern a -> Pattern a
filterStartInRange (Pattern f) =
  Pattern $ \(s,e) -> filter ((isIn (s,e)) . eventOnset) $ f (s,e)

filterOnsetsInRange = filterOnsets . filterStartInRange

-- Samples some events from a pattern, returning a list of onsets
-- (relative to the given arc), deltas (durations) and values.
seqToRelOnsetDeltas :: Arc -> Pattern a -> [(Double, Double, a)]
seqToRelOnsetDeltas (s, e) p = map (\((s', e'), _, x) -> (fromRational $ (s'-s) / (e-s), fromRational $ (e'-s) / (e-s), x)) $ arc (filterOnsetsInRange p) (s, e)

segment :: Pattern a -> Pattern [a]
segment p = Pattern $ \(s,e) -> filter (\(_,(s',e'),_) -> s' < e && e' > s) $ groupByTime (segment' (arc p (s,e)))

segment' :: [Event a] -> [Event a]
segment' es = foldr split es pts
  where pts = nub $ points es

split :: Time -> [Event a] -> [Event a]
split _ [] = []
split t ((ev@(a,(s,e), v)):es) | t > s && t < e = (a,(s,t),v):(a,(t,e),v):(split t es)
                               | otherwise = ev:split t es

points :: [Event a] -> [Time]
points [] = []
points ((_,(s,e), _):es) = s:e:(points es)

groupByTime :: [Event a] -> [Event [a]]
groupByTime es = map mrg $ groupBy ((==) `on` snd') $ sortBy (compare `on` snd') es
  where mrg es@((a, a', _):_) = (a, a', map thd' es)


{-| Decide whether to apply one or another function depending on the result of a test function that is passed the current cycle as a number.

@
d1 $ ifp ((== 0).(flip mod 2))
  (striate 4)
  (# coarse "24 48") $
  sound "hh hc"
@

This will apply `striate 4` for every _even_ cycle and aply `# coarse "24 48"` for every _odd_.

Detail: As you can see the test function is arbitrary and does not rely on anything tidal specific. In fact it uses only plain haskell functionality, that is: it calculates the modulo of 2 of the current cycle which is either 0 (for even cycles) or 1. It then compares this value against 0 and returns the result, which is either `True` or `False`. This is what the `ifp` signature's first part signifies `(Int -> Bool)`, a function that takes a whole number and returns either `True` or `False`.
-}
ifp :: (Int -> Bool) -> (Pattern a -> Pattern a) -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
ifp test f1 f2 p = splitQueries $ Pattern apply
  where apply a | test (floor $ fst a) = (arc $ f1 p) a
                | otherwise = (arc $ f2 p) a

{-|

`rand` generates a continuous pattern of (pseudo-)random, floating point numbers between `0` and `1`.

@
d1 $ sound "bd*8" # pan rand
@

pans bass drums randomly

@
d1 $ sound "sn sn ~ sn" # gain rand
@

makes the snares' randomly loud and quiet.

Numbers coming from this pattern are random, but dependent on time. So if you reset time via `cps (-1)` the random pattern will emit the exact same _random_ numbers again.

In cases where you need two different random patterns, you can shift one of them around to change the time from which the _random_ pattern is read, note the difference:

@
d1 $ jux (|+| gain rand) $ sound "sn sn ~ sn" # gain rand
@

and with the juxed version shifted backwards for 1024 cycles:

@
d1 $ jux (|+| ((1024 <~) $ gain rand)) $ sound "sn sn ~ sn" # gain rand
@
-}
rand :: Pattern Double
rand = Pattern $ \a -> [(a, a, timeToRand $ (midPoint a))]

timeToRand t = fst $ randomDouble $ pureMT $ floor $ (*1000000) t

{- | Just like `rand` but for integers, `irand n` generates a pattern of (pseudo-)random integers between `0` to `n-1` inclusive. Notably used to pick a random
samples from a folder:

@
d1 $ sound (samples "drum*4" (irand 5))
@
-}
irand :: Int -> Pattern Int
irand i = (floor . (* (fromIntegral i))) <$> rand

{- | Randomly picks an element from the given list

@
d1 $ sound (samples "xx(3,8)" (tom $ choose ["a", "e", "g", "c"]))
@

plays a melody randomly choosing one of the four notes \"a\", \"e\", \"g\", \"c\".
-}
choose :: [a] -> Pattern a
choose xs = (xs !!) <$> (irand $ length xs)

{- |
Similar to `degrade` `degradeBy` allows you to control the percentage of events that
are removed. For example, to remove events 90% of the time:

@
d1 $ slow 2 $ degradeBy 0.9 $ sound "[[[feel:5*8,feel*3] feel:3*8], feel*4]"
   # accelerate "-6"
   # speed "2"
@

-}
degradeBy :: Double -> Pattern a -> Pattern a
degradeBy x p = unMaybe $ (\a f -> toMaybe (f > x) a) <$> p <*> rand
    where toMaybe False _ = Nothing
          toMaybe True a  = Just a
          unMaybe = (fromJust <$>) . filterValues isJust

unDegradeBy :: Double -> Pattern a -> Pattern a
unDegradeBy x p = unMaybe $ (\a f -> toMaybe (f <= x) a) <$> p <*> rand
    where toMaybe False _ = Nothing
          toMaybe True a  = Just a
          unMaybe = (fromJust <$>) . filterValues isJust

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
sometimesBy :: Double -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
sometimesBy x f p = overlay (degradeBy x p) (f $ unDegradeBy x p)

-- | @sometimes@ is an alias for sometimesBy 0.5.
sometimes = sometimesBy 0.5
-- | @often@ is an alias for sometimesBy 0.75.
often = sometimesBy 0.75
-- | @rarely@ is an alias for sometimesBy 0.25.
rarely = sometimesBy 0.25
-- | @almostNever@ is an alias for sometimesBy 0.1
almostNever = sometimesBy 0.1
-- | @almostAlways@ is an alias for sometimesBy 0.9
almostAlways = sometimesBy 0.9
never = flip const
always = id

{- | @someCyclesBy@ is a cycle-by-cycle version of @sometimesBy@. It has a
`someCycles = someCyclesBy 0.5` alias -}
someCyclesBy x = when (test x)
  where test x c = (timeToRand $ fromIntegral c) < x

somecyclesBy = someCyclesBy

someCycles = someCyclesBy 0.5

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
degrade :: Pattern a -> Pattern a
degrade = degradeBy 0.5

-- | @wedge t p p'@ combines patterns @p@ and @p'@ by squashing the
-- @p@ into the portion of each cycle given by @t@, and @p'@ into the
-- remainer of each cycle.
wedge :: Time -> Pattern a -> Pattern a -> Pattern a
wedge t p p' = overlay (densityGap (1/t) p) (t ~> densityGap (1/(1-t)) p')

{- | @whenmod@ has a similar form and behavior to `every`, but requires an
additional number. Applies the function to the pattern, when the
remainder of the current loop number divided by the first parameter,
is greater or equal than the second parameter.

For example the following makes every other block of four loops twice
as dense:

@
d1 $ whenmod 8 4 (density 2) (sound "bd sn kurt")
@
-}
whenmod :: Int -> Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
whenmod a b = Sound.Tidal.Pattern.when ((\t -> (t `mod` a) >= b ))

{- |
@
superimpose f p = stack [p, f p]
@

`superimpose` plays a modified version of a pattern at the same time as the original pattern,
resulting in two patterns being played at the same time.

@
d1 $ superimpose (density 2) $ sound "bd sn [cp ht] hh"
d1 $ superimpose ((# speed "2") . (0.125 <~)) $ sound "bd sn cp hh"
@

-}
superimpose f p = stack [p, f p]

-- | @splitQueries p@ wraps `p` to ensure that it does not get
-- queries that span arcs. For example `arc p (0.5, 1.5)` would be
-- turned into two queries, `(0.5,1)` and `(1,1.5)`, and the results
-- combined. Being able to assume queries don't span cycles often
-- makes transformations easier to specify.
splitQueries :: Pattern a -> Pattern a
splitQueries p = Pattern $ \a -> concatMap (arc p) $ arcCycles a

{- | Truncates a pattern so that only a fraction of the pattern is played.
The following example plays only the first three quarters of the pattern:

@
d1 $ trunc 0.75 $ sound "bd sn*2 cp hh*4 arpy bd*2 cp bd*2"
@
-}
trunc :: Time -> Pattern a -> Pattern a
trunc t = compress (0,t) . zoom (0,t)

{- | Plays a portion of a pattern, specified by a beginning and end arc of time.
The new resulting pattern is played over the time period of the original pattern:

@
d1 $ zoom (0.25, 0.75) $ sound "bd*2 hh*3 [sn bd]*2 drum"
@

In the pattern above, `zoom` is used with an arc from 25% to 75%. It is equivalent to this pattern:

@
d1 $ sound "hh*3 [sn bd]*2"
@
-}
zoom :: Arc -> Pattern a -> Pattern a
zoom a@(s,e) p = splitQueries $ withResultArc (mapCycle ((/d) . (subtract s))) $ withQueryArc (mapCycle ((+s) . (*d))) p
     where d = e-s

compress :: Arc -> Pattern a -> Pattern a
compress a@(s,e) p | s >= e = silence
                   | otherwise = s ~> densityGap (1/(e-s)) p

sliceArc :: Arc -> Pattern a -> Pattern a
sliceArc a@(s,e) p | s >= e = silence
                   | otherwise = compress a $ zoom a p

{- |
Use `within` to apply a function to only a part of a pattern. For example, to
apply `density 2` to only the first half of a pattern:

@
d1 $ within (0, 0.5) (density 2) $ sound "bd*2 sn lt mt hh hh hh hh"
@

Or, to apply `(# speed "0.5") to only the last quarter of a pattern:

@
d1 $ within (0.75, 1) (# speed "0.5") $ sound "bd*2 sn lt mt hh hh hh hh"
@
-}
within :: Arc -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
within (s,e) f p = stack [sliceArc (0,s) p,
                           compress (s,e) $ f $ zoom (s,e) p,
                           sliceArc (e,1) p
                          ]

revArc a = within a rev

{- | You can use the @e@ function to apply a Euclidean algorithm over a
complex pattern, although the structure of that pattern will be lost:

@
d1 $ e 3 8 $ sound "bd*2 [sn cp]"
@

In the above, three sounds are picked from the pattern on the right according
to the structure given by the `e 3 8`. It ends up picking two `bd` sounds, a
`cp` and missing the `sn` entirely.

These types of sequences use "Bjorklund's algorithm", which wasn't made for
music but for an application in nuclear physics, which is exciting. More
exciting still is that it is very similar in structure to the one of the first
known algorithms written in Euclid's book of elements in 300 BC. You can read
more about this in the paper
[The Euclidean Algorithm Generates Traditional Musical Rhythms](http://cgm.cs.mcgill.ca/~godfried/publications/banff.pdf)
by Toussaint. Some examples from this paper are included below,
including rotation in some cases.

@
- (2,5) : A thirteenth century Persian rhythm called Khafif-e-ramal.
- (3,4) : The archetypal pattern of the Cumbia from Colombia, as well as a Calypso rhythm from Trinidad.
- (3,5,2) : Another thirteenth century Persian rhythm by the name of Khafif-e-ramal, as well as a Rumanian folk-dance rhythm.
- (3,7) : A Ruchenitza rhythm used in a Bulgarian folk-dance.
- (3,8) : The Cuban tresillo pattern.
- (4,7) : Another Ruchenitza Bulgarian folk-dance rhythm.
- (4,9) : The Aksak rhythm of Turkey.
- (4,11) : The metric pattern used by Frank Zappa in his piece titled Outside Now.
- (5,6) : Yields the York-Samai pattern, a popular Arab rhythm.
- (5,7) : The Nawakhat pattern, another popular Arab rhythm.
- (5,8) : The Cuban cinquillo pattern.
- (5,9) : A popular Arab rhythm called Agsag-Samai.
- (5,11) : The metric pattern used by Moussorgsky in Pictures at an Exhibition.
- (5,12) : The Venda clapping pattern of a South African children’s song.
- (5,16) : The Bossa-Nova rhythm necklace of Brazil.
- (7,8) : A typical rhythm played on the Bendir (frame drum).
- (7,12) : A common West African bell pattern.
- (7,16,14) : A Samba rhythm necklace from Brazil.
- (9,16) : A rhythm necklace used in the Central African Republic.
- (11,24,14) : A rhythm necklace of the Aka Pygmies of Central Africa.
- (13,24,5) : Another rhythm necklace of the Aka Pygmies of the upper Sangha.
@
-}
e :: Int -> Int -> Pattern a -> Pattern a
e n k p = (flip const) <$> (filterValues (== True) $ listToPat $ bjorklund (n,k)) <*> p

e' :: Int -> Int -> Pattern a -> Pattern a
e' n k p = cat $ map (\x -> if x then p else silence) (bjorklund (n,k))


index :: Real b => b -> Pattern b -> Pattern c -> Pattern c
index sz indexpat pat = spread' (zoom' $ toRational sz) (toRational . (*(1-sz)) <$> indexpat) pat
  where zoom' sz start = zoom (start, start+sz)

-- | @prrw f rot (blen, vlen) beatPattern valuePattern@: pattern rotate/replace.
prrw :: (a -> b -> c) -> Int -> (Time, Time) -> Pattern a -> Pattern b -> Pattern c
prrw f rot (blen, vlen) beatPattern valuePattern =
  let
    ecompare (_,e1,_) (_,e2,_) = compare (fst e1) (fst e2)
    beats  = sortBy ecompare $ arc beatPattern (0, blen)
    values = fmap thd' . sortBy ecompare $ arc valuePattern (0, vlen)
    cycles = blen * (fromIntegral $ lcm (length beats) (length values) `div` (length beats))
  in
    slow cycles $ stack $ zipWith
    (\( _, (start, end), v') v -> (start ~>) $ densityGap (1 / (end - start)) $ pure (f v' v))
    (sortBy ecompare $ arc (density cycles $ beatPattern) (0, blen))
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
prep = preplace

preplace1 :: Pattern String -> Pattern b -> Pattern b
preplace1 = preplace (1, 1)

preplaceWith :: (a -> b -> c) -> (Time, Time) -> Pattern a -> Pattern b -> Pattern c
preplaceWith f (blen, plen) = prrw f 0 (blen, plen)

prw = preplaceWith

preplaceWith1 :: (a -> b -> c) -> Pattern a -> Pattern b -> Pattern c
preplaceWith1 f = prrw f 0 (1, 1)

prw1 = preplaceWith1

(<~>) :: Pattern String -> Pattern b -> Pattern b
(<~>) = preplace (1, 1)

-- | @protate len rot p@ rotates pattern @p@ by @rot@ beats to the left.
-- @len@: length of the pattern, in cycles.
-- Example: @d1 $ every 4 (protate 2 (-1)) $ slow 2 $ sound "bd hh hh hh"@
protate :: Time -> Int -> Pattern a -> Pattern a
protate len rot p = prrw (flip const) rot (len, len) p p

prot = protate
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

-- | @discretise n p@: 'samples' the pattern @p@ at a rate of @n@
-- events per cycle. Useful for turning a continuous pattern into a
-- discrete one.
discretise :: Time -> Pattern a -> Pattern a
discretise n p = density n $ (atom (id)) <*> p

-- | @randcat ps@: does a @slowcat@ on the list of patterns @ps@ but
-- randomises the order in which they are played.
randcat :: [Pattern a] -> Pattern a
randcat ps = spread' (<~) (discretise 1 $ ((%1) . fromIntegral) <$> irand (length ps - 1)) (slowcat ps)

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

@
d1 $ sound (fit 3 ["bd", "sn", "arpy", "arpy:1", "casio"] "0 [~ 1] 2 1")
@

The above fits three samples into the pattern, i.e. for the first cycle this will be `"bd"`, `"sn"` and `"arpy"`, giving the result `"bd [~ sn] arpy sn"` (note that we start counting at zero, so that `0` picks the first value). The following cycle the *next* three values in the list will be picked, i.e. `"arpy:1"`, `"casio"` and `"bd"`, giving the pattern `"arpy:1 [~ casio] bd casio"` (note that the list wraps round here).

-}
fit :: Int -> [a] -> Pattern Int -> Pattern a
fit perCycle xs p = (xs !!!) <$> (Pattern $ \a -> map ((\e -> (mapThd' (+ (cyclePos perCycle e)) e))) (arc p a))
  where cyclePos perCycle e = perCycle * (floor $ eventStart e)

permstep :: RealFrac b => Int -> [a] -> Pattern b -> Pattern a
permstep steps things p = unwrap $ (\n -> listToPat $ concatMap (\x -> replicate (fst x) (snd x)) $ zip (ps !! (floor (n * (fromIntegral $ (length ps - 1))))) things) <$> (discretise 1 p)
      where ps = permsort (length things) steps
            deviance avg xs = sum $ map (abs . (avg-) . fromIntegral) xs
            permsort n total = map fst $ sortBy (comparing snd) $ map (\x -> (x,deviance (fromIntegral total/ fromIntegral n) x)) $ perms n total
            perms 0 _ = []
            perms 1 n = [[n]]
            perms n total = concatMap (\x -> map (x:) $ perms (n-1) (total-x)) [1 .. (total-(n-1))]

-- | @struct a b@: structures pattern @b@ in terms of @a@.
struct :: Pattern String -> Pattern a -> Pattern a
struct ps pv = (flip const) <$> ps <*> pv


-- | @substruct a b@: similar to @struct@, but each event in pattern @a@ gets replaced with pattern @b@, compressed to fit the timespan of the event.
substruct :: Pattern String -> Pattern b -> Pattern b
substruct s p = filterStartInRange $ Pattern $ f
  where f a = concatMap (\a' -> arc (compressTo a' p) a') $ (map fst' $ arc s a)
        compressTo (s,e) p = compress (cyclePos s, e-(sam s)) p

-- Lindenmayer patterns, these go well with the step sequencer
-- general rule parser (strings map to strings)
parseLMRule :: String -> [(String,String)]
parseLMRule s = map (splitOn ':') (commaSplit s)
  where splitOn sep str = splitAt (fromJust $ elemIndex sep str)
                            $ filter (/= sep) str
        commaSplit s = map T.unpack $ T.splitOn (T.pack ",") $ T.pack s

-- specific parser for step sequencer (chars map to string)
-- ruleset in form "a:b,b:ab"
parseLMRule' :: String -> [(Char, String)]
parseLMRule' str = map fixer $ parseLMRule str
  where fixer (c,r) = (head c, r)

{- | returns the `n`th iteration of a [Lindenmayer System](https://en.wikipedia.org/wiki/L-system) with given start sequence.

for example:

@
lindenmayer 1 "a:b,b:ab" "ab" -> "bab"
@
-}
lindenmayer :: Int -> String -> String -> String
lindenmayer _ _ [] = []
lindenmayer 1 r (c:cs) = (fromMaybe [c] $ lookup c $ parseLMRule' r) 
                         ++ (lindenmayer 1 r cs)
lindenmayer n r s = iterate (lindenmayer 1 r) s !! n

-- support for fit'
unwrap' :: Pattern (Pattern a) -> Pattern a
unwrap' pp = Pattern $ \a -> arc (stack $ map scalep (arc pp a)) a
  where scalep ev = compress (fst' ev) $ thd' ev

{-|
Removes events from second pattern that don't start during an event from first.

Consider this, kind of messy rhythm without any rests.

@
d1 $ sound (slowcat ["sn*8", "[cp*4 bd*4, hc*5]"]) # n (run 8)
@

If we apply a mask to it

@
d1 $ s (mask ("1 1 1 ~ 1 1 ~ 1" :: Pattern Bool)
  (slowcat ["sn*8", "[cp*4 bd*4, bass*5]"] ))
  # n (run 8) 
@

Due to the use of `slowcat` here, the same mask is first applied to `"sn*8"` and in the next cycle to `"[cp*4 bd*4, hc*5]".

You could achieve the same effect by adding rests within the `slowcat` patterns, but mask allows you to do this more easily. It kind of keeps the rhythmic structure and you can change the used samples independently, e.g.

@
d1 $ s (mask ("1 ~ 1 ~ 1 1 ~ 1" :: Pattern Bool)
  (slowcat ["can*8", "[cp*4 sn*4, jvbass*16]"] ))
  # n (run 8) 
@

Detail: It is currently needed to explicitly _tell_ Tidal that the mask itself is a `Pattern Bool` as it cannot infer this by itself, otherwise it will complain as it does not know how to interpret your input.
-}
mask :: Pattern a -> Pattern b -> Pattern b
mask pa pb = Pattern $ \a -> concat [filterOns (subArc a $ eventArc i) (arc pb a) | i <- arc pa a]
     where filterOns Nothing _ = []
           filterOns (Just arc) es = filter (onsetIn arc) es

enclosingArc :: [Arc] -> Arc
enclosingArc [] = (0,1)
enclosingArc as = (minimum (map fst as), maximum (map snd as))

stretch :: Pattern a -> Pattern a
stretch p = splitQueries $ Pattern $ \a@(s,_e) -> arc
              (zoom (enclosingArc $ map eventArc $ arc p (sam s,nextSam s)) p)
              a

{- | `fit'` is a generalization of `fit`, where the list is instead constructed by using another integer pattern to slice up a given pattern.  The first argument is the number of cycles of that latter pattern to use when slicing.  It's easier to understand this with a few examples:

@
d1 $ sound (fit' 1 2 "0 1" "1 0" "bd sn")
@

So what does this do?  The first `1` just tells it to slice up a single cycle of `"bd sn"`. The `2` tells it to select two values each cycle, just like the first argument to `fit`.  The next pattern `"0 1"` is the "from" pattern which tells it how to slice, which in this case means `"0"` maps to `"bd"`, and `"1"` maps to `"sn"`.  The next pattern `"1 0"` is the "to" pattern, which tells it how to rearrange those slices.  So the final result is the pattern `"sn bd"`.

A more useful example might be something like

@
d1 $ fit' 1 4 (run 4) "[0 3*2 2 1 0 3*2 2 [1*8 ~]]/2" $ chop 4 $ (sound "breaks152" # unit "c")
@

which uses `chop` to break a single sample into individual pieces, which `fit'` then puts into a list (using the `run 4` pattern) and reassembles according to the complicated integer pattern.

-}
fit' cyc n from to p = unwrap' $ fit n (mapMasks n from' p') to
  where mapMasks n from p = [stretch $ mask (filterValues (== i) from) p
                             | i <- [0..n-1]]
        p' = density cyc $ p
        from' = density cyc $ from

{-| @runWith n f p@ treats the given pattern @p@ as having @n@ sections, and applies the function @f@ to one of those sections per cycle, running from left to right.

@
d1 $ runWith 4 (density 4) $ sound "cp sn arpy [mt lt]"
@
-}
runWith :: Integral a => a -> (Pattern b -> Pattern b) -> Pattern b -> Pattern b
runWith n f p = do i <- slow (toRational n) $ run (fromIntegral n)
                   within (i%(fromIntegral n),(i+)1%(fromIntegral n)) f p


{-| @runWith'@ works much the same as `runWith`, but runs from right to left.
-}
runWith' :: Integral a => a -> (Pattern b -> Pattern b) -> Pattern b -> Pattern b
runWith' n f p = do i <- slow (toRational n) $ rev $ run (fromIntegral n)
                    within (i%(fromIntegral n),(i+)1%(fromIntegral n)) f p

inside :: Time -> (Pattern a1 -> Pattern a) -> Pattern a1 -> Pattern a
inside n f p = density n $ f (slow n p)

outside :: Time -> (Pattern a1 -> Pattern a) -> Pattern a1 -> Pattern a
outside n = inside (1/n)

loopFirst p = splitQueries $ Pattern f
  where f a@(s,e) = mapSnds' plus $ mapFsts' plus $ arc p (minus a)
          where minus = mapArc (subtract (sam s))
                plus = mapArc (+ (sam s))

timeLoop :: Time -> Pattern a -> Pattern a
timeLoop n = outside n loopFirst

seqPLoop :: [(Time, Time, Pattern a)] -> Pattern a
seqPLoop ps = timeLoop (maxT - minT) $ minT <~ seqP ps
  where minT = minimum $ map fst' ps
        maxT = maximum $ map snd' ps

{- | @toScale@ lets you turn a pattern of notes within a scale (expressed as a
list) to note numbers.  For example `toScale [0, 4, 7] "0 1 2 3"` will turn
into the pattern `"0 4 7 12"`.  It assumes your scale fits within an octave,
to change this use `toScale' size`.  Example:
`toscale' 24 [0,4,7,10,14,17] (run 8)` turns into `"0 4 7 10 14 17 24 28"`
-}
toScale'::Int -> [Int] -> Pattern Int -> Pattern Int
toScale' o s p = (+) <$> fmap (s!!) notep <*> fmap (o*) octp
  where notep = fmap (`mod` (length s)) p
        octp  = fmap (`div` (length s)) p

toScale::[Int] -> Pattern Int -> Pattern Int
toScale = toScale' 12

{- | `swingBy x n` divides a cycle into `n` slices and delays the notes in
the second half of each slice by `x` fraction of a slice . @swing@ is an alias
for `swingBy (1%3)`
-}
swingBy::Time -> Time -> Pattern a -> Pattern a 
swingBy x n = inside n (within (0.5,1) (x ~>))

swing = swingBy (1%3)

{- | `cycleChoose` is like `choose` but only picks a new item from the list
once each cycle -}
cycleChoose::[a] -> Pattern a
cycleChoose xs = Pattern $ \(s,e) -> [((s,e),(s,e), xs!!(floor $ (dlen xs)*(ctrand s) ))]
  where dlen xs = fromIntegral $ length xs
        ctrand s = timeToRand $ fromIntegral $ floor $ sam s

{- | `shuffle n p` evenly divides one cycle of the pattern `p` into `n` parts,
and returns a random permutation of the parts each cycle.  For example, 
`shuffle 3 "a b c"` could return `"a b c"`, `"a c b"`, `"b a c"`, `"b c a"`,
`"c a b"`, or `"c b a"`.  But it will **never** return `"a a a"`, because that
is not a permutation of the parts.
-}
shuffle::Int -> Pattern a -> Pattern a
shuffle n = fit' 1 n (run n) (unwrap $ cycleChoose $ map listToPat $ 
  permutations [0..n-1])

{- | `scramble n p` is like `shuffle` but randomly selects from the parts
of `p` instead of making permutations. 
For example, `scramble 3 "a b c"` will randomly select 3 parts from
`"a"` `"b"` and `"c"`, possibly repeating a single part.
-}
scramble::Int -> Pattern a -> Pattern a
scramble n = fit' 1 n (run n) (density (fromIntegral n) $ 
  liftA2 (+) (pure 0) $ irand n)

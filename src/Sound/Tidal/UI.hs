{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Sound.Tidal.UI where

import Prelude hiding ((<*), (*>))

import Sound.Tidal.Pattern
import Sound.Tidal.Params

import Data.Fixed (mod')
import Data.Ord (comparing)
import Data.Char (digitToInt, isDigit)
import System.Random.Mersenne.Pure64 (randomDouble, pureMT)
import qualified Data.Map.Strict as Map
import Data.Ratio (numerator, denominator, (%))
import Data.List (delete, findIndex, sort, sortBy, intercalate, findIndices, elemIndex, groupBy, transpose)
import Data.Maybe (isJust, fromJust, catMaybes, fromMaybe, mapMaybe)
import qualified Data.Text as T
import Control.Applicative (liftA2)

import Sound.Tidal.Bjorklund (bjorklund)
import Sound.Tidal.Params
import Sound.Tidal.Utils

------------------------------------------------------------------------
-- * UI

-- ** Pattern algebra

-- class for types that support a left-biased union
class Unionable a where
  union :: a -> a -> a

-- default union is just to take the left hand side..
instance Unionable a where
  union = const

instance {-# OVERLAPPING #-} Unionable ControlMap where
  union = Map.union

(|+|) :: (Applicative a, Num b) => a b -> a b -> a b
a |+| b = (+) <$> a <*> b
(|+ ) :: Num a => Pattern a -> Pattern a -> Pattern a
a |+  b = (+) <$> a <* b
( +|) :: Num a => Pattern a -> Pattern a -> Pattern a
a  +| b = (+) <$> a *> b

(|/|) :: (Applicative a, Fractional b) => a b -> a b -> a b
a |/| b = (/) <$> a <*> b
(|/ ) :: Fractional a => Pattern a -> Pattern a -> Pattern a
a |/  b = (/) <$> a <* b
( /|) :: Fractional a => Pattern a -> Pattern a -> Pattern a
a  /| b = (/) <$> a *> b

(|*|) :: (Applicative a, Num b) => a b -> a b -> a b
a |*| b = (*) <$> a <*> b
(|* ) :: Num a => Pattern a -> Pattern a -> Pattern a
a |*  b = (*) <$> a <* b
( *|) :: Num a => Pattern a -> Pattern a -> Pattern a
a  *| b = (*) <$> a *> b

(|-|) :: (Applicative a, Num b) => a b -> a b -> a b
a |-| b = (-) <$> a <*> b
(|- ) :: Num a => Pattern a -> Pattern a -> Pattern a
a |-  b = (-) <$> a <* b
( -|) :: Num a => Pattern a -> Pattern a -> Pattern a
a  -| b = (-) <$> a *> b

(|%|) :: (Applicative a, Real b) => a b -> a b -> a b
a |%| b = mod' <$> a <*> b
(|% ) :: Real a => Pattern a -> Pattern a -> Pattern a
a |%  b = mod' <$> a <* b
( %|) :: Real a => Pattern a -> Pattern a -> Pattern a
a  %| b = mod' <$> a *> b

(|>|) :: (Applicative a, Unionable b) => a b -> a b -> a b
a |>| b = (flip union) <$> a <*> b
(|> ) :: Unionable a => Pattern a -> Pattern a -> Pattern a
a |>  b = (flip union) <$> a <* b
( >|) :: Unionable a => Pattern a -> Pattern a -> Pattern a
a  >| b = (flip union) <$> a *> b

(|<|) :: (Applicative a, Unionable b) => a b -> a b -> a b
a |<| b = union <$> a <*> b
(|< ) :: Unionable a => Pattern a -> Pattern a -> Pattern a
a |<  b = union <$> a <* b
( <|) :: Unionable a => Pattern a -> Pattern a -> Pattern a
a  <| b = union <$> a *> b

-- Backward compatibility - structure from left, values from right.
(#) :: Unionable b => Pattern b -> Pattern b -> Pattern b
(#) = (|>)

-- ** Elemental patterns

-- | An empty pattern
silence :: Pattern a
silence = empty

-- | Takes a function from time to values, and turns it into a 'Pattern'.
sig :: (Time -> a) -> Pattern a
sig f = Pattern Analog q
  where q (State (s,e) _) | s > e = []
                          | otherwise = [(((s,e), (s,e)), f (s+((e-s)/2)))]

-- | @sine@ returns a 'Pattern' of continuous 'Fractional' values following a
-- sinewave with frequency of one cycle, and amplitude from 0 to 1.
sine :: Fractional a => Pattern a
sine = sig $ \t -> ((sin_rat $ (pi :: Double) * 2 * (fromRational t)) + 1) / 2
  where sin_rat = fromRational . toRational . sin

-- | @cosine@ is a synonym for @0.25 ~> sine@.
cosine :: Fractional a => Pattern a
cosine = 0.25 `rotR` sine

-- | @saw@ is the equivalent of 'sine' for (ascending) sawtooth waves.
saw :: (Fractional a, Real a) => Pattern a
saw = sig $ \t -> mod' (fromRational t) 1

-- | @isaw@ is the equivalent of 'sine' for inverse (descending) sawtooth waves.
isaw :: (Fractional a, Real a) => Pattern a
isaw = (1-) <$> saw

-- | @tri@ is the equivalent of 'sine' for triangular waves.
tri :: (Fractional a, Real a) => Pattern a
tri = append saw (rev saw)

-- | @square@ is the equivalent of 'sine' for square waves.
square :: (Fractional a, Real a) => Pattern a
square = sig $
         \t -> fromIntegral $ ((floor $ (mod' (fromRational t :: Double) 1) * 2) :: Integer)

-- | @envL@ is a 'Pattern' of continuous 'Double' values, representing
-- a linear interpolation between 0 and 1 during the first cycle, then
-- staying constant at 1 for all following cycles. Possibly only
-- useful if you're using something like the retrig function defined
-- in tidal.el.
envL :: Pattern Double
envL = sig $ \t -> max 0 $ min (fromRational t) 1

-- | like 'envL' but reversed.
envLR :: Pattern Double
envLR = (1-) <$> envL

-- | 'Equal power' version of 'env', for gain-based transitions
envEq :: Pattern Double
envEq = sig $ \t -> sqrt (sin (pi/2 * (max 0 $ min (fromRational (1-t)) 1)))

-- | Equal power reversed
envEqR :: Pattern Double
envEqR = sig $ \t -> sqrt (cos (pi/2 * (max 0 $ min (fromRational (1-t)) 1)))

-- ** Constructing patterns

-- | Turns a list of values into a pattern, playing through them once per cycle.
fromList :: [a] -> Pattern a
fromList = fastCat . map pure

-- | A synonym for 'fromList'
listToPat :: [a] -> Pattern a
listToPat = fromList

-- | 'fromMaybes; is similar to 'fromList', but allows values to
-- be optional using the 'Maybe' type, so that 'Nothing' results in
-- gaps in the pattern.
fromMaybes :: [Maybe a] -> Pattern a
fromMaybes = fastcat . map f
  where f Nothing = silence
        f (Just x) = pure x

-- | A pattern of whole numbers from 0 to the given number, in a single cycle.
run :: (Enum a, Num a) => Pattern a -> Pattern a
run = (>>= _run)

_run :: (Enum a, Num a) => a -> Pattern a
_run n = fromList [0 .. n-1]

-- | From @1@ for the first cycle, successively adds a number until it gets up to @n@
scan :: (Enum a, Num a) => Pattern a -> Pattern a
scan = (>>= _scan)

_scan :: (Enum a, Num a) => a -> Pattern a
_scan n = slowcat $ map _run [1 .. n]

-- ** Combining patterns

-- | Alternate between cycles of the two given patterns
append :: Pattern a -> Pattern a -> Pattern a
append a b = cat [a,b]

-- | Like 'append', but for a list of patterns. Interlaces them, playing the first cycle from each
-- in turn, then the second cycle from each, and so on.
cat :: [Pattern a] -> Pattern a
cat [] = silence
-- I *guess* it would be digital..
cat ps = Pattern Digital q
  where n = length ps
        q st = concatMap (f st) $ arcCyclesZW (arc st)
        f st a = query (withResultTime (+offset) p) $ st {arc = mapBoth (subtract offset) a}
          where p = ps !! i
                cyc = (floor $ fst a) :: Int
                i = cyc `mod` n
                offset = (fromIntegral $ cyc - ((cyc - i) `div` n)) :: Time

-- | Alias for 'cat'
slowCat :: [Pattern a] -> Pattern a
slowCat = cat
slowcat :: [Pattern a] -> Pattern a
slowcat = slowCat

-- | Alias for 'append'
slowAppend :: Pattern a -> Pattern a -> Pattern a
slowAppend = append

-- | Like 'append', but twice as fast
fastAppend :: Pattern a -> Pattern a -> Pattern a
fastAppend a b = _fast 2 $ append a b

-- | The same as 'cat', but speeds up the result by the number of
-- patterns there are, so the cycles from each are squashed to fit a
-- single cycle.
fastCat :: [Pattern a] -> Pattern a
fastCat ps = _fast (toTime $ length ps) $ cat ps

fastcat :: [Pattern a] -> Pattern a
fastcat = fastCat

-- | Similar to @fastCat@, but each pattern is given a relative duration
timeCat :: [(Time, Pattern a)] -> Pattern a
timeCat tps = stack $ map (\(s,e,p) -> compress (s/total, e/total) p) $ arrange 0 tps
    where total = sum $ map fst tps
          arrange :: Time -> [(Time, Pattern a)] -> [(Time, Time, Pattern a)]
          arrange _ [] = []
          arrange t ((t',p):tps) = (t,t+t',p):(arrange (t+t') tps)

-- | 'overlay' combines two 'Pattern's into a new pattern, so that
-- their events are combined over time. 
overlay :: Pattern a -> Pattern a -> Pattern a
-- Analog if they're both analog
overlay p@(Pattern Analog _) p'@(Pattern Analog _) = Pattern Analog $ \st -> (query p st) ++ (query p' st)
-- Otherwise digital. Won't really work to have a mixture.. Hmm
overlay p p' = Pattern Digital $ \st -> (query p st) ++ (query p' st)

-- | An infix operator, an alias of overlay
(<>) :: Pattern a -> Pattern a -> Pattern a
(<>) = overlay

-- | 'stack' combines a list of 'Pattern's into a new pattern, so that
-- their events are combined over time.
stack :: [Pattern a] -> Pattern a
stack = foldr overlay silence

-- ** Manipulating time

-- | Shifts a pattern back in time by the given amount, expressed in cycles
(<~) :: Pattern Time -> Pattern a -> Pattern a
(<~) = tParam rotL

-- | Shifts a pattern forward in time by the given amount, expressed in cycles
(~>) :: Pattern Time -> Pattern a -> Pattern a
(~>) = tParam rotR

-- | Speed up a pattern by the given time pattern
fast :: Pattern Time -> Pattern a -> Pattern a
fast = tParam _fast

-- | Slow down a pattern by the factors in the given time pattern, 'squeezing'
-- the pattern to fit the slot given in the time pattern
fastSqueeze :: Pattern Time -> Pattern a -> Pattern a
fastSqueeze = tParamSqueeze _fast

-- | An alias for @fast@
density :: Pattern Time -> Pattern a -> Pattern a
density = fast

_fast :: Time -> Pattern a -> Pattern a
_fast r p | r == 0 = silence
          | r < 0 = rev $ _fast (0-r) p
          | otherwise = withResultTime (/ r) $ withQueryTime (* r) p

-- | Slow down a pattern by the given time pattern
slow :: Pattern Time -> Pattern a -> Pattern a
slow = tParam _slow
_slow :: Time -> Pattern a -> Pattern a
_slow r p = _fast (1/r) p

-- | Slow down a pattern by the factors in the given time pattern, 'squeezing'
-- the pattern to fit the slot given in the time pattern
slowSqueeze :: Pattern Time -> Pattern a -> Pattern a
slowSqueeze = tParamSqueeze _slow

-- | An alias for @slow@
sparsity :: Pattern Time -> Pattern a -> Pattern a
sparsity = slow

-- | @rev p@ returns @p@ with the event positions in each cycle
-- reversed (or mirrored).
rev :: Pattern a -> Pattern a
rev p = splitQueries $ p {query = \st -> map makeWholeAbsolute $ mapParts (mirrorArc (midCycle $ arc st)) $ map makeWholeRelative (query p st {arc = (mirrorArc (midCycle $ arc st) (arc st))})}
  where makeWholeRelative (((s,e), part@(s',e')), v) = (((s'-s, e-e'), part), v)
        makeWholeAbsolute (((s,e), part@(s',e')), v) = (((s'-e, e'+s), part), v)
        midCycle (s,_) = (sam s) + 0.5
        mapParts f es = map (mapFst (mapSnd f)) es
        -- | Returns the `mirror image' of a 'Arc' around the given point in time
        mirrorArc :: Time -> Arc -> Arc
        mirrorArc mid' (s, e) = (mid' - (e-mid'), mid'+(mid'-s))

{- | Plays a portion of a pattern, specified by a time arc (start and end time).
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
zoom (s,e) p = splitQueries $ withResultArc (mapCycle ((/d) . (subtract s))) $ withQueryArc (mapCycle ((+s) . (*d))) p
     where d = e-s

-- | @fastGap@ is similar to 'fast' but maintains its cyclic
-- alignment. For example, @fastGap 2 p@ would squash the events in
-- pattern @p@ into the first half of each cycle (and the second
-- halves would be empty). The factor should be at least 1
fastGap :: Pattern Time -> Pattern a -> Pattern a
fastGap = tParam _fastGap

-- | An alias for @fastGap@
densityGap = fastGap

compress :: Arc -> Pattern a -> Pattern a
compress = __compress

compressTo :: Arc -> Pattern a -> Pattern a
compressTo = __compressTo

repeatCycles :: Int -> Pattern a -> Pattern a
repeatCycles n p = fastcat (replicate n p)

-- | * Higher order functions

-- | Functions which work on other functions (higher order functions)

-- | @every n f p@ applies the function @f@ to @p@, but only affects
-- every @n@ cycles.
every :: Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
every tp f p = tp >>= \t -> _every t f p

_every :: Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
_every 0 _ p = p
_every n f p = when ((== 0) . (`mod` n)) f p

-- | @every n o f'@ is like @every n f@ with an offset of @o@ cycles
every' :: Pattern Int -> Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
every' np op f p = do { n <- np; o <- op; _every' n o f p }

_every' :: Int -> Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
_every' n o f = when ((== o) . (`mod` n)) f

-- | @foldEvery ns f p@ applies the function @f@ to @p@, and is applied for
-- each cycle in @ns@.
foldEvery :: [Int] -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
foldEvery ns f p = foldr ($) p (map (\x -> _every x f) ns)

{-|
Only `when` the given test function returns `True` the given pattern
transformation is applied. The test function will be called with the
current cycle as a number.

@
d1 $ when ((elem '4').show)
  (striate 4)
  $ sound "hh hc"
@

The above will only apply `striate 4` to the pattern if the current
cycle number contains the number 4. So the fourth cycle will be
striated and the fourteenth and so on. Expect lots of striates after
cycle number 399.
-}
when :: (Int -> Bool) -> (Pattern a -> Pattern a) ->  Pattern a -> Pattern a
when test f p = splitQueries $ p {query = apply}
  where apply st | test (floor $ fst $ arc st) = query (f p) st
                 | otherwise = query p st

-- | Like 'when', but works on continuous time values rather than cycle numbers.
whenT :: (Time -> Bool) -> (Pattern a -> Pattern a) ->  Pattern a -> Pattern a
whenT test f p = splitQueries $ p {query = apply}
  where apply st | test (fst $ arc st) = query (f p) st
                 | otherwise = query p st


--eoff :: Int -> Int -> Integer -> Pattern a -> Pattern a
--eoff n k s p = ((s%(fromIntegral k)) `rotL`) (_e n k p)
   -- TPat_ShiftL (s%(fromIntegral k)) (TPat_E n k p)


controlI :: String -> Pattern Int
controlI s = Pattern Analog $ \(State a m) -> maybe [] (f a) $ Map.lookup s m
  where f a (VI v) = [((a,a),v)]
        f a (VF v) = [((a,a),floor v)]
        f a (VS v) = maybe [] (\v' -> [((a,a),v')]) (readMaybe v)

controlF :: String -> Pattern Double
controlF s = Pattern Analog $ \(State a m) -> maybe [] (f a) $ Map.lookup s m
  where f a (VI v) = [((a,a),fromIntegral v)]
        f a (VF v) = [((a,a),v)]
        f a (VS v) = maybe [] (\v' -> [((a,a),v')]) (readMaybe v)

controlS :: String -> Pattern String
controlS s = Pattern Analog $ \(State a m) -> maybe [] (f a) $ Map.lookup s m
  where f a (VI v) = [((a,a),show v)]
        f a (VF v) = [((a,a),show v)]
        f a (VS v) = [((a,a),v)]

timeToRand :: RealFrac r => r -> Double
timeToRand t = fst $ randomDouble $ pureMT $ floor $ (*1000000) t

-- | Randomisation

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

rand :: Pattern Double
rand = Pattern Analog (\(State a _) -> [((a, a), timeToRand $ mid a)])

{- | Just like `rand` but for whole numbers, `irand n` generates a pattern of (pseudo-) random whole numbers between `0` to `n-1` inclusive. Notably used to pick a random
samples from a folder:

@
d1 $ n (irand 5) # sound "drum"
@
-}
irand :: Num a => Int -> Pattern a
irand i = (fromIntegral . (floor :: Double -> Int) . (* (fromIntegral i))) <$> rand

{- | Randomly picks an element from the given list

@
sound "superpiano(3,8)" # note (choose ["a", "e", "g", "c"])
@

plays a melody randomly choosing one of the four notes \"a\", \"e\", \"g\", \"c\".
-}
choose :: [a] -> Pattern a
choose = chooseBy rand

chooseBy :: Pattern Double -> [a] -> Pattern a
chooseBy _ [] = silence
chooseBy f xs = ((xs !!) . floor) <$> (scale 0 (fromIntegral $ length xs) f)

{- | Like @choose@, but works on an a list of tuples of values and weights

@
sound "superpiano(3,8)" # note (choose [("a",1), ("e",0.5), ("g",2), ("c",1)])
@

In the above example, the "a" and "c" notes are twice as likely to
play as the "e" note, and half as likely to play as the "g" note.

-}
wchoose :: [(a,Double)] -> Pattern a
wchoose = wchooseBy rand

wchooseBy :: Pattern Double -> [(a,Double)] -> Pattern a
wchooseBy pat pairs = match <$> pat
  where
    match r = values !! ((findIndices (> r) cweights) !! 0)
    cweights = scanl1 (+) (map snd pairs)
    values = map fst pairs

{- |
Similar to `degrade` `degradeBy` allows you to control the percentage of events that
are removed. For example, to remove events 90% of the time:

@
d1 $ slow 2 $ degradeBy 0.9 $ sound "[[[feel:5*8,feel*3] feel:3*8], feel*4]"
   # accelerate "-6"
   # speed "2"
@

-}

degradeBy :: Pattern Double -> Pattern a -> Pattern a
degradeBy = tParam _degradeBy

_degradeBy :: Double -> Pattern a -> Pattern a
_degradeBy x p = fmap fst $ filterValues ((> x) . snd) $ (,) <$> p <*> rand

unDegradeBy :: Pattern Double -> Pattern a -> Pattern a
unDegradeBy = tParam _unDegradeBy

_unDegradeBy :: Double -> Pattern a -> Pattern a
_unDegradeBy x p = fmap fst $ filterValues ((<= x) . snd) $ (,) <$> p <*> rand

degradeOverBy :: Int -> Pattern Double -> Pattern a -> Pattern a
degradeOverBy i tx p = unwrap $ (\x -> (fmap fst $ filterValues ((> x) . snd) $ (,) <$> p <*> repeatCycles i rand)) <$> (slow (fromIntegral i) tx)


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
sometimesBy x f p = overlay (_degradeBy x p) (f $ _unDegradeBy x p)

-- | @sometimes@ is an alias for sometimesBy 0.5.
sometimes :: (Pattern a -> Pattern a) -> Pattern a -> Pattern a
sometimes = sometimesBy 0.5

-- | @often@ is an alias for sometimesBy 0.75.
often :: (Pattern a -> Pattern a) -> Pattern a -> Pattern a
often = sometimesBy 0.75

-- | @rarely@ is an alias for sometimesBy 0.25.
rarely :: (Pattern a -> Pattern a) -> Pattern a -> Pattern a
rarely = sometimesBy 0.25

-- | @almostNever@ is an alias for sometimesBy 0.1
almostNever :: (Pattern a -> Pattern a) -> Pattern a -> Pattern a
almostNever = sometimesBy 0.1

-- | @almostAlways@ is an alias for sometimesBy 0.9
almostAlways :: (Pattern a -> Pattern a) -> Pattern a -> Pattern a
almostAlways = sometimesBy 0.9

never :: (Pattern a -> Pattern a) -> Pattern a -> Pattern a
never = flip const

always :: (Pattern a -> Pattern a) -> Pattern a -> Pattern a
always = id

{- | @someCyclesBy@ is a cycle-by-cycle version of @sometimesBy@. It has a
`someCycles = someCyclesBy 0.5` alias -}
someCyclesBy :: Double -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
someCyclesBy x = when test
  where test c = (timeToRand (fromIntegral c :: Double)) < x

somecyclesBy :: Double -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
somecyclesBy = someCyclesBy

someCycles :: (Pattern a -> Pattern a) -> Pattern a -> Pattern a
someCycles = someCyclesBy 0.5

somecycles :: (Pattern a -> Pattern a) -> Pattern a -> Pattern a
somecycles = someCycles

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
degrade = _degradeBy 0.5



{- | (The above means that `brak` is a function from patterns of any type,
to a pattern of the same type.)

Make a pattern sound a bit like a breakbeat

Example:

@
d1 $ sound (brak "bd sn kurt")
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
iter = tParam _iter

_iter :: Int -> Pattern a -> Pattern a
_iter n p = slowcat $ map (\i -> ((fromIntegral i)%(fromIntegral n)) `rotL` p) [0 .. (n-1)]

-- | @iter'@ is the same as @iter@, but decrements the starting
-- subdivision instead of incrementing it.
iter' :: Pattern Int -> Pattern c -> Pattern c
iter' = tParam _iter'

_iter' :: Int -> Pattern a -> Pattern a
_iter' n p = slowcat $ map (\i -> ((fromIntegral i)%(fromIntegral n)) `rotR` p) [0 .. (n-1)]

-- | @palindrome p@ applies @rev@ to @p@ every other cycle, so that
-- the pattern alternates between forwards and backwards.
palindrome :: Pattern a -> Pattern a
palindrome p = slowAppend p (rev p)

-- | Composing patterns

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
seqP ps = stack $ map (\(s, e, p) -> playFor s e ((sam s) `rotR` p)) ps

-- | Degrades a pattern over the given time.
fadeOut :: Time -> Pattern a -> Pattern a
fadeOut dur p = do slope <- _slow dur envL
                   _degradeBy slope p

-- | Alternate version to @fadeOut@ where you can provide the time from which the fade starts
fadeOutFrom :: Time -> Time -> Pattern a -> Pattern a
fadeOutFrom from dur p = do slope <- (from `rotR` _slow dur envL)
                            _degradeBy slope p


-- | 'Undegrades' a pattern over the given time.
fadeIn :: Time -> Pattern a -> Pattern a
fadeIn dur p = do slope <- _slow dur ((1-) <$> envL)
                  _degradeBy slope p

-- | Alternate version to @fadeIn@ where you can provide the time from
-- which the fade in starts
fadeInFrom :: Time -> Time -> Pattern a -> Pattern a
fadeInFrom from dur p = do slope <- (from `rotR` _slow dur ((1-) <$> envL))
                           _degradeBy slope p


{- | The 'spread' function allows you to take a pattern transformation
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
spread f xs p = slowcat $ map (`f` p) xs

slowspread :: (a -> t -> Pattern b) -> [a] -> t -> Pattern b
slowspread = spread

{- | @fastspread@ works the same as @spread@, but the result is squashed into a single cycle. If you gave four values to @spread@, then the result would seem to speed up by a factor of four. Compare these two:

d1 $ spread chop [4,64,32,16] $ sound "ho ho:2 ho:3 hc"

d1 $ fastspread chop [4,64,32,16] $ sound "ho ho:2 ho:3 hc"

There is also @slowspread@, which is an alias of @spread@.
-}
fastspread :: (a -> t -> Pattern b) -> [a] -> t -> Pattern b
fastspread f xs p = fastcat $ map (\x -> f x p) xs

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
spread' :: Monad m => (a -> b -> m c) -> m a -> b -> m c
spread' f vpat pat = vpat >>= \v -> f v pat

{- | `spreadChoose f xs p` is similar to `slowspread` but picks values from
`xs` at random, rather than cycling through them in order. It has a
shorter alias `spreadr`.
-}
spreadChoose :: (t -> t1 -> Pattern b) -> [t] -> t1 -> Pattern b
spreadChoose f vs p = do v <- _discretise 1 (choose vs)
                         f v p

spreadr :: (t -> t1 -> Pattern b) -> [t] -> t1 -> Pattern b
spreadr = spreadChoose



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
ifp test f1 f2 p = splitQueries $ p {query = q}
  where q a | test (floor $ fst $ arc a) = query (f1 p) a
            | otherwise = query (f2 p) a

-- | @wedge t p p'@ combines patterns @p@ and @p'@ by squashing the
-- @p@ into the portion of each cycle given by @t@, and @p'@ into the
-- remainer of each cycle.
wedge :: Time -> Pattern a -> Pattern a -> Pattern a
wedge t p p' = overlay (_fastGap (1/t) p) (t `rotR` _fastGap (1/(1-t)) p')

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
whenmod a b = Sound.Tidal.UI.when ((\t -> (t `mod` a) >= b ))

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
superimpose :: (Pattern a -> Pattern a) -> Pattern a -> Pattern a
superimpose f p = stack [p, f p]

{- | @trunc@ truncates a pattern so that only a fraction of the pattern is played.
The following example plays only the first quarter of the pattern:

@
d1 $ trunc 0.25 $ sound "bd sn*2 cp hh*4 arpy bd*2 cp bd*2"
@
-}
trunc :: Pattern Time -> Pattern a -> Pattern a
trunc = tParam _trunc

_trunc :: Time -> Pattern a -> Pattern a
_trunc t = compress (0,t) . zoom (0,t)

{- | @linger@ is similar to `trunc` but the truncated part of the pattern loops until the end of the cycle

@
d1 $ linger 0.25 $ sound "bd sn*2 cp hh*4 arpy bd*2 cp bd*2"
@
-}
linger :: Pattern Time -> Pattern a -> Pattern a
linger = tParam _linger

_linger :: Time -> Pattern a -> Pattern a
_linger n p = _fast (1/n) $ zoom (0,n) p

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
within (s,e) f p = stack [filterWhen (\t -> cyclePos t >= s && cyclePos t < e) $ f p,
                          filterWhen (\t -> not $ cyclePos t >= s && cyclePos t < e) $ p
                         ]

{- |
For many cases, @within'@ will function exactly as within.  
The difference between the two occurs when applying functions that change the timing of notes such as 'fast' or '<~'. 
within first applies the function to all notes in the cycle, then keeps the results in the specified interval, and then combines it with the old cycle (an "apply split combine" paradigm). 
within' first keeps notes in the specified interval, then applies the function to these notes, and then combines it with the old cycle (a "split apply combine" paradigm).


For example, whereas using the standard version of within

@
d1 $ within (0, 0.25) (fast 2) $ sound "bd hh cp sd"
@

sounds like:

@
d1 $ sound "[bd hh] hh cp sd"
@

using this alternative version, within'

@
d1 $ within' (0, 0.25) (fast 2) $ sound "bd hh cp sd"
@

sounds like: 

@
d1 $ sound "[bd bd] hh cp sd"
@

-}

within' :: Arc -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
within' (s,e) f p = stack [filterWhen (\t -> cyclePos t >= s && cyclePos t < e) $ compress (s,e) $ f $ zoom (s,e) $ p, 
                           filterWhen (\t -> not $ cyclePos t >= s && cyclePos t < e) $ p 
                          ]


revArc :: Arc -> Pattern a -> Pattern a
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
euler :: Pattern Int -> Pattern Int -> Pattern a -> Pattern a
euler = tParam2 _euler

_euler :: Int -> Int -> Pattern a -> Pattern a
_euler n k p = (flip const) <$> (filterValues (== True) $ listToPat $ bjorklund (n,k)) <*> p

-- euler' :: Pattern Int -> Pattern Int -> Pattern a -> Pattern a
-- euler' = tParam2 _eulerq'

_euler' :: Int -> Int -> Pattern a -> Pattern a
_euler' n k p = fastcat $ map (\x -> if x then p else silence) (bjorklund (n,k))


distrib :: [Pattern Int] -> Pattern a -> Pattern a
distrib steps p = do steps' <- sequence steps
                     _distrib steps' p

_distrib :: [Int] -> Pattern a -> Pattern a
_distrib xs p = boolsToPat (foldr (distrib') (replicate (last xs) True) (reverse $ layers xs)) p
  where
    distrib' :: [Bool] -> [Bool] -> [Bool]
    distrib' [] _ = []
    distrib' (_:a) [] = False:(distrib' a [])
    distrib' (True:a) (x:b) = x:(distrib' a b)
    distrib' (False:a) (b) = False:(distrib' a b)
    layers = map bjorklund . (zip<*>tail)
    boolsToPat p p' = (flip const) <$> (filterValues (== True) $ listToPat $ p) <*> p'

{- | `einv` fills in the blanks left by `e`
 -
 @e 3 8 "x"@ -> @"x ~ ~ x ~ ~ x ~"@

 @einv 3 8 "x"@ -> @"~ x x ~ x x ~ x"@
-}
eulerInv :: Pattern Int -> Pattern Int -> Pattern a -> Pattern a
eulerInv = tParam2 _eulerInv

_eulerInv :: Int -> Int -> Pattern a -> Pattern a
_eulerInv n k p = (flip const) <$> (filterValues (== False) $ listToPat $ bjorklund (n,k)) <*> p

{- | `eulerfull n k pa pb` stacks @e n k pa@ with @einv n k pb@ -}
eulerFull :: Pattern Int -> Pattern Int -> Pattern a -> Pattern a -> Pattern a
eulerFull n k pa pb = stack [ euler n k pa, eulerInv n k pb ]

index :: Real b => b -> Pattern b -> Pattern c -> Pattern c
index sz indexpat pat = spread' (zoom' $ toRational sz) (toRational . (*(1-sz)) <$> indexpat) pat
  where zoom' sz start = zoom (start, start+sz)

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
    (sortBy ecompare $ arc (_density cycles $ beatPattern) (0, blen))
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

-- | @discretise n p@: 'samples' the pattern @p@ at a rate of @n@
-- events per cycle. Useful for turning a continuous pattern into a
-- discrete one.
discretise :: Time -> Pattern a -> Pattern a
discretise = _discretise

discretise' :: Pattern Time -> Pattern a -> Pattern a
discretise' n p = (density n $ pure (id)) <*> p

_discretise :: Time -> Pattern a -> Pattern a
_discretise n p = (_fast n $ pure (id)) <*> p

-- | @randcat ps@: does a @slowcat@ on the list of patterns @ps@ but
-- randomises the order in which they are played.
randcat :: [Pattern a] -> Pattern a
randcat ps = spread' (rotL) (_discretise 1 $ ((%1) . fromIntegral) <$> (irand (length ps) :: Pattern Int)) (slowcat ps)

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
fit perCycle xs p = (xs !!!) <$> (p {query = \st -> map ((\e -> (fmap (+ (cyclePos perCycle e)) e))) (query p st)})
  where cyclePos perCycle e = perCycle * (floor $ fst $ snd $ fst e)

permstep :: RealFrac b => Int -> [a] -> Pattern b -> Pattern a
permstep steps things p = unwrap $ (\n -> listToPat $ concatMap (\x -> replicate (fst x) (snd x)) $ zip (ps !! (floor (n * (fromIntegral $ (length ps - 1))))) things) <$> (_discretise 1 p)
      where ps = permsort (length things) steps
            deviance avg xs = sum $ map (abs . (avg-) . fromIntegral) xs
            permsort n total = map fst $ sortBy (comparing snd) $ map (\x -> (x,deviance (fromIntegral total / (fromIntegral n :: Double)) x)) $ perms n total
            perms 0 _ = []
            perms 1 n = [[n]]
            perms n total = concatMap (\x -> map (x:) $ perms (n-1) (total-x)) [1 .. (total-(n-1))]

-- | @struct a b@: structures pattern @b@ in terms of @a@.
struct :: Pattern String -> Pattern a -> Pattern a
struct ps pv = (flip const) <$> ps <*> pv

-- | @substruct a b@: similar to @struct@, but each event in pattern @a@ gets replaced with pattern @b@, compressed to fit the timespan of the event.
substruct :: Pattern String -> Pattern b -> Pattern b
substruct s p = p {query = f}
  where f st =
          concatMap (\a' -> queryArc (compressTo a' p) a') $ (map eventWhole $ query s st)

randArcs :: Int -> Pattern [Arc]
randArcs n =
  do rs <- mapM (\x -> (pure $ (toRational x)/(toRational n)) <~ choose [1,2,3]) [0 .. (n-1)]
     let rats = map toRational rs
         total = sum rats
         pairs = pairUp $ accumulate $ map ((/total)) rats
     return $ pairs
       where pairUp [] = []
             pairUp xs = (0,head xs):(pairUp' xs)
             pairUp' [] = []
             pairUp' (_:[]) = []
             pairUp' (a:_:[]) = [(a,1)]
             pairUp' (a:b:xs) = (a,b):(pairUp' (b:xs))

-- TODO - what does this do? Something for @stripe@ ..
randStruct :: Int -> Pattern Int
randStruct n = splitQueries $ Pattern {nature = Digital, query = f}
  where f st = map (\((a,b),c) -> ((a,fromJust b),c)) $ filter (\((_,x),_) -> isJust x) $ as
          where as = map (\(n, (s',e')) -> (((s' + sam s, e' + sam s),
                                             subArc (s,e) (s' + sam s, e' + sam s)),
                                            n
                                          )
                         ) $ enumerate $ eventValue $ head $ queryArc (randArcs n) (sam s, nextSam s)
                (s,e) = arc st

-- TODO - what does this do?
substruct' :: Pattern Int -> Pattern a -> Pattern a
substruct' s p = p {query = \st -> concatMap (\((a', _), i) -> queryArc (compressTo a' (inside (pure $ 1/toRational(length (queryArc s (sam (fst $ arc st), nextSam (fst $ arc st))))) (rotR (toRational i)) p)) a') (query s st)}

-- | @stripe n p@: repeats pattern @p@, @n@ times per cycle. So
-- similar to @fast@, but with random durations. The repetitions will
-- be continguous (touching, but not overlapping) and the durations
-- will add up to a single cycle. @n@ can be supplied as a pattern of
-- integers.
stripe :: Pattern Int -> Pattern a -> Pattern a
stripe = tParam _stripe

_stripe :: Int -> Pattern a -> Pattern a
_stripe = substruct' . randStruct

-- | @slowstripe n p@: The same as @stripe@, but the result is also
-- @n@ times slower, so that the mean average duration of the stripes
-- is exactly one cycle, and every @n@th stripe starts on a cycle
-- boundary (in indian classical terms, the @sam@).
slowstripe :: Pattern Int -> Pattern a -> Pattern a
slowstripe n = slow (toRational <$> n) . stripe n

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

{- | @lindenmayerI@ converts the resulting string into a a list of integers
with @fromIntegral@ applied (so they can be used seamlessly where floats or
rationals are required) -}
lindenmayerI n r s = fmap fromIntegral $ fmap digitToInt $ lindenmayer n r s

-- support for fit'
unwrap' :: Pattern (Pattern a) -> Pattern a
unwrap' pp = pp {query = \st -> query (stack $ map scalep (query pp st)) st}
  where scalep ev = compress (eventWhole ev) $ eventValue ev

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
d1 $ s (mask ("1 ~ 1 ~ 1 1 ~ 1")
  (slowcat ["can*8", "[cp*4 sn*4, jvbass*16]"] ))
  # n (run 8)
@
-}

mask :: Pattern Bool -> Pattern b -> Pattern b
-- TODO - should that be eventPart or eventWhole?
mask pa pb = pb {query = \st -> concat [filterOns (subArc (arc st) $ eventPart i) (query pb st) | i <- query pa st]}
     where filterOns Nothing _ = []
           filterOns (Just arc) es = filter (onsetIn arc) es

enclosingArc :: [Arc] -> Arc
enclosingArc [] = (0,1)
enclosingArc as = (minimum (map fst as), maximum (map snd as))

stretch :: Pattern a -> Pattern a
-- TODO - should that be eventWhole or eventPart?
stretch p = splitQueries $ p {query = q}
  where q st = query (zoom (enclosingArc $ map eventWhole $ query p (st {arc = (sam s,nextSam s)})) p) st
          where s = fst $ arc st

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
fit' :: Pattern Time -> Int -> Pattern Int -> Pattern Int -> Pattern a -> Pattern a
fit' cyc n from to p = unwrap' $ fit n (mapMasks n from' p') to
  where mapMasks n from p = [stretch $ mask (const True <$> filterValues (== i) from) p
                             | i <- [0..n-1]]
        p' = density cyc $ p
        from' = density cyc $ from

{-| @chunk n f p@ treats the given pattern @p@ as having @n@ chunks, and applies the function @f@ to one of those sections per cycle, running from left to right.

@
d1 $ chunk 4 (density 4) $ sound "cp sn arpy [mt lt]"
@
-}
chunk :: Integer -> (Pattern b -> Pattern b) -> Pattern b -> Pattern b
chunk n f p = cat [within (i%(fromIntegral n),(i+1)%(fromIntegral n)) f p | i <- [0..n-1]]

{-
chunk n f p = do i <- _slow (toRational n) $ run (fromIntegral n)
                 within (i%(fromIntegral n),(i+)1%(fromIntegral n)) f p
-}

-- deprecated (renamed to chunk)
runWith :: Integer -> (Pattern b -> Pattern b) -> Pattern b -> Pattern b
runWith = chunk

{-| @chunk'@ works much the same as `chunk`, but runs from right to left.
-}
chunk' :: Integral a => a -> (Pattern b -> Pattern b) -> Pattern b -> Pattern b
chunk' n f p = do i <- _slow (toRational n) $ rev $ run (fromIntegral n)
                  within (i%(fromIntegral n),(i+)1%(fromIntegral n)) f p

-- deprecated (renamed to chunk')
runWith' :: Integral a => a -> (Pattern b -> Pattern b) -> Pattern b -> Pattern b
runWith' = chunk'

inside :: Pattern Time -> (Pattern a1 -> Pattern a) -> Pattern a1 -> Pattern a
inside n f p = density n $ f (slow n p)

outside :: Pattern Time -> (Pattern a1 -> Pattern a) -> Pattern a1 -> Pattern a
outside n = inside (1/n)

loopFirst :: Pattern a -> Pattern a
loopFirst p = splitQueries $ p {query = f}
  where f st = map (\((w,p),v) -> ((plus w, plus p),v)) $ query p (st {arc = minus $ arc st})
          where minus = mapArc (subtract (sam s))
                plus = mapArc (+ (sam s))
                s = fst $ arc st

timeLoop :: Pattern Time -> Pattern a -> Pattern a
timeLoop n = outside n loopFirst

seqPLoop :: [(Time, Time, Pattern a)] -> Pattern a
seqPLoop ps = timeLoop (pure $ maxT - minT) $ minT `rotL` seqP ps
  where minT = minimum $ map (\(x,_,_) -> x) ps
        maxT = maximum $ map (\(_,x,_) -> x) ps

{- | @toScale@ lets you turn a pattern of notes within a scale (expressed as a
list) to note numbers.  For example `toScale [0, 4, 7] "0 1 2 3"` will turn
into the pattern `"0 4 7 12"`.  It assumes your scale fits within an octave;
to change this use `toScale' size`.  Example:
`toScale' 24 [0,4,7,10,14,17] (run 8)` turns into `"0 4 7 10 14 17 24 28"`
-}
toScale' :: Num a => Int -> [a] -> Pattern Int -> Pattern a
toScale' o s = fmap noteInScale
  where octave x = x `div` length s
        noteInScale x = (s !!! x) + (fromIntegral $ o * octave x)

toScale :: Num a => [a] -> Pattern Int -> Pattern a
toScale = toScale' 12

{- | `swingBy x n` divides a cycle into `n` slices and delays the notes in
the second half of each slice by `x` fraction of a slice . @swing@ is an alias
for `swingBy (1%3)`
-}
swingBy :: Pattern Time -> Pattern Time -> Pattern a -> Pattern a
swingBy x n = inside n (within (0.5,1) (x ~>))

swing :: Pattern Time -> Pattern a -> Pattern a
swing = swingBy (pure $ 1%3)

{- | `cycleChoose` is like `choose` but only picks a new item from the list
once each cycle -}
cycleChoose::[a] -> Pattern a
cycleChoose xs = Pattern {nature = Digital, query = q}
  where q (State {arc = (s,e)}) = [(((s,e),(s,e)), xs!!(floor $ (dlen xs)*(ctrand s) ))]
        dlen xs = fromIntegral $ length xs
        ctrand s = (timeToRand :: Time -> Double) $ fromIntegral $ (floor :: Time -> Int) $ sam s

{- | `shuffle n p` evenly divides one cycle of the pattern `p` into `n` parts,
and returns a random permutation of the parts each cycle.  For example,
`shuffle 3 "a b c"` could return `"a b c"`, `"a c b"`, `"b a c"`, `"b c a"`,
`"c a b"`, or `"c b a"`.  But it will **never** return `"a a a"`, because that
is not a permutation of the parts.
-}
shuffle::Int -> Pattern a -> Pattern a
shuffle n = fit' 1 n (_run n) (randpat n)
  where randpat n = Pattern {nature = Digital,
                             query = \(State {arc = (s,e)}) -> queryArc (p n $ sam s) (s,e)
                            }
        p n c = listToPat $ map snd $ sort $ zip
                  [timeToRand (c+i/n') | i <- [0..n'-1]] [0..n-1]
        n' :: Time
        n' = fromIntegral n

{- | `scramble n p` is like `shuffle` but randomly selects from the parts
of `p` instead of making permutations.
For example, `scramble 3 "a b c"` will randomly select 3 parts from
`"a"` `"b"` and `"c"`, possibly repeating a single part.
-}
scramble::Int -> Pattern a -> Pattern a
scramble n = fit' 1 n (_run n) (_fast (fromIntegral n) $
  liftA2 (+) (pure 0) $ irand n)

ur :: Time -> Pattern String -> [(String, Pattern a)] -> [(String, Pattern a -> Pattern a)] -> Pattern a
ur t outer_p ps fs = _slow t $ unwrap $ adjust <$> (timedValues $ (getPat . split) <$> outer_p)
  where split s = wordsBy (==':') s
        getPat (s:xs) = (match s, transform xs)
        match s = fromMaybe silence $ lookup s ps'
        ps' = map (fmap (_fast t)) ps
        adjust (a, (p, f)) = f a p
        transform (x:_) a = transform' x a
        transform _ _ = id
        transform' str (s,e) p = s `rotR` (inside (pure $ 1/(e-s)) (matchF str) p)
        matchF str = fromMaybe id $ lookup str fs
        timedValues = withEvent (\((a,a'),v) -> ((a,a'),(a,v)))

inhabit :: [(String, Pattern a)] -> Pattern String -> Pattern a
inhabit ps p = unwrap' $ (\s -> fromMaybe silence $ lookup s ps) <$> p

{- | @spaceOut xs p@ repeats a pattern @p@ at different durations given by the list of time values in @xs@ -}
spaceOut :: [Time] -> Pattern a -> Pattern a
spaceOut xs p = _slow (toRational $ sum xs) $ stack $ map (\a -> compress a p) $ spaceArcs xs
  where markOut :: Time -> [Time] -> [(Time, Time)]
        markOut _ [] = []
        markOut offset (x:xs) = (offset,offset+x):(markOut (offset+x) xs)
        spaceArcs xs = map (\(a,b) -> (a/s,b/s)) $ markOut 0 xs
        s = sum xs

-- | @flatpat@ takes a Pattern of lists and pulls the list elements as
-- separate Events
flatpat :: Pattern [a] -> Pattern a
flatpat p = p {query = \st -> (concatMap (\((b,b'),xs) -> map (\x -> ((b,b'),x)) xs) $ query p st)}

-- | @layer@ takes a Pattern of lists and pulls the list elements as
-- separate Events
layer :: [a -> Pattern b] -> a -> Pattern b
layer fs p = stack $ map ($ p) fs

-- | @breakUp@ finds events that share the same timespan, and spreads
-- them out during that timespan, so for example @breakUp "[bd,sn]"@
-- gets turned into @"bd sn"@

-- TODO - does this need a sort before groupBy ?
breakUp :: Pattern a -> Pattern a
breakUp p = withEvents munge p
  where munge es = concatMap spreadOut (groupBy (\a b -> eventWhole a == eventWhole b) es)
        spreadOut xs = mapMaybe (\(n, x) -> shiftIt n (length xs) x) $ enumerate xs
        shiftIt n d (((s,e), a'), v) = do a'' <- subArc (newS, newE) a'
                                          return (((newS, newE), a''), v)
          where newS = s + (dur*(fromIntegral n))
                newE = newS + dur
                dur = (e - s) / (fromIntegral d)

{- TODO !

-- | @fill@ 'fills in' gaps in one pattern with events from another. For example @fill "bd" "cp ~ cp"@ would result in the equivalent of `"~ bd ~"`. This only finds gaps in a resulting pattern, in other words @"[bd ~, sn]"@ doesn't contain any gaps (because @sn@ covers it all), and @"bd ~ ~ sn"@ only contains a single gap that bridges two steps.
fill :: Pattern a -> Pattern a -> Pattern a
fill p' p = struct (splitQueries $ p {query = q}) p'
  where
    q st = removeTolerance (s,e) $ invert (s-tolerance, e+tolerance) $ query p (st {arc = (s-tolerance, e+tolerance)})
      where (s,e) = arc st
    invert (s,e) es = map arcToEvent $ foldr remove [(s,e)] (map eventPart es)
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

-- Repeats each event @n@ times within its arc
ply :: Pattern Int -> Pattern a -> Pattern a
ply = tParam _ply

_ply :: Int -> Pattern a -> Pattern a
_ply n p = breakUp $ stack (replicate n p)

-- Uses the first (binary) pattern to switch between the following two
-- patterns. 
sew :: Pattern Bool -> Pattern a -> Pattern a -> Pattern a
sew stitch p1 p2 = overlay (const <$> p1 <*> a) (const <$> p2 <*> b)
  where a = filterValues (id) stitch
        b = filterValues (not . id) stitch


stutter :: Integral i => i -> Time -> Pattern a -> Pattern a
stutter n t p = stack $ map (\i -> (t * (fromIntegral i)) `rotR` p) [0 .. (n-1)]

echo, triple, quad, double :: Time -> Pattern a -> Pattern a
echo   = stutter 2
triple = stutter 3
quad   = stutter 4
double = echo

{- | The `jux` function creates strange stereo effects, by applying a
function to a pattern, but only in the right-hand channel. For
example, the following reverses the pattern on the righthand side:

@
d1 $ slow 32 $ jux (rev) $ striate' 32 (1/16) $ sound "bev"
@

When passing pattern transforms to functions like [jux](#jux) and [every](#every),
it's possible to chain multiple transforms together with `.`, for
example this both reverses and halves the playback speed of the
pattern in the righthand channel:

@
d1 $ slow 32 $ jux ((# speed "0.5") . rev) $ striate' 32 (1/16) $ sound "bev"
@
-}
jux = juxBy 1
juxcut f p = stack [p     # pan (pure 0) # cut (pure (-1)),
                    f $ p # pan (pure 1) # cut (pure (-2))
                   ]

juxcut' fs p = stack $ map (\n -> ((fs !! n) p |+ cut (pure $ 1-n)) # pan (pure $ fromIntegral n / fromIntegral l)) [0 .. l-1]
  where l = length fs

{- | In addition to `jux`, `jux'` allows using a list of pattern transform. resulting patterns from each transformation will be spread via pan from left to right.

For example:

@
d1 $ jux' [iter 4, chop 16, id, rev, palindrome] $ sound "bd sn"
@

will put `iter 4` of the pattern to the far left and `palindrome` to the far right. In the center the original pattern will play and mid left mid right the chopped and the reversed version will appear.

One could also write:

@
d1 $ stack [
    iter 4 $ sound "bd sn" # pan "0",
    chop 16 $ sound "bd sn" # pan "0.25",
    sound "bd sn" # pan "0.5",
    rev $ sound "bd sn" # pan "0.75",
    palindrome $ sound "bd sn" # pan "1",
    ]
@

-}
jux' fs p = stack $ map (\n -> ((fs !! n) p) |+ pan (pure $ fromIntegral n / fromIntegral l)) [0 .. l-1]
  where l = length fs

-- | Multichannel variant of `jux`, _not sure what it does_
jux4 f p = stack [p # pan (pure (5/8)), f $ p # pan (pure (1/8))]

{- |
With `jux`, the original and effected versions of the pattern are
panned hard left and right (i.e., panned at 0 and 1). This can be a
bit much, especially when listening on headphones. The variant `juxBy`
has an additional parameter, which brings the channel closer to the
centre. For example:

@
d1 $ juxBy 0.5 (density 2) $ sound "bd sn:1"
@

In the above, the two versions of the pattern would be panned at 0.25
and 0.75, rather than 0 and 1.
-}
juxBy n f p = stack [p |+ pan 0.5 |- pan (n/2), f $ p |+ pan 0.5 |+ pan (n/2)]

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

{-
smash n xs p = slowcat $ map (\n -> slow n p') xs
  where p' = striate n p
-}

{- | an altenative form to `smash` is `smash'` which will use `chop` instead of `striate`.
-}
smash' n xs p = slowcat $ map (\n -> slow n p') xs
  where p' = _chop n p

pick :: String -> Int -> String
pick name n = name ++ ":" ++ (show n)

-- samples "jvbass [~ latibro] [jvbass [latibro jvbass]]" ((1%2) `rotL` slow 6 "[1 6 8 7 3]")

samples :: Applicative f => f String -> f Int -> f String
samples p p' = pick <$> p <*> p'

samples' :: Applicative f => f String -> f Int -> f String
samples' p p' = (flip pick) <$> p' <*> p

{-
scrumple :: Time -> Pattern a -> Pattern a -> Pattern a
scrumple o p p' = p'' -- overlay p (o `rotR` p'')
  where p'' = Pattern $ \a -> concatMap
                              (\((s,d), vs) -> map (\x -> ((s,d),
                                                           snd x
                                                          )
                                                   )
                                                   (arc p' (s,s))
                              ) (arc p a)
-}

--rev :: Pattern a -> Pattern a
--rev p = Pattern $ \a -> concatMap
--                        (\a' -> mapFsts mirrorArc $
--                                (arc p (mirrorArc a')))
--                        (arcCycles a)

--spreadf :: [Pattern a -> Pattern b] -> Pattern a -> Pattern b
spreadf ts p = spread ($)

{- | `spin` will "spin" a layer up a pattern the given number of times, with each successive layer offset in time by an additional `1/n` of a cycle, and panned by an additional `1/n`. The result is a pattern that seems to spin around. This function works best on multichannel systems.

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

{-stripe :: Arc -> Pattern a -> Pattern a
stripe (stripeS, stripeE) p = slow t $ Pattern $ \a -> concatMap f $ arcCycles a
  where f a = mapFsts (stretch . stripe') $ arc p (stripe' a)
        trunc' (s,e) = (min s ((sam s) + t), min e ((sam s) + t))
        stretch (s,e) = (sam s + ((s - sam s) / t), sam s + ((e - sam s) / t))
-}

stackwith p ps | null ps = silence
               | otherwise = stack $ map (\(i, p') -> p' # (((fromIntegral i) % l) `rotL` p)) (zip [0 ..] ps)
  where l = fromIntegral $ length ps

{-
cross f p p' = Pattern $ \t -> concat [filter flt $ arc p t,
                                       filter (not . flt) $ arc p' t
                                      ]
]  where flt = f . cyclePos . fst . fst
-}

{- | `scale` will take a pattern which goes from 0 to 1 (like `sine1`), and scale it to a different range - between the first and second arguments. In the below example, `scale 1 1.5` shifts the range of `sine1` from 0 - 1 to 1 - 1.5.

@
d1 $ jux (iter 4) $ sound "arpy arpy:2*2"
  |+ speed (slow 4 $ scale 1 1.5 sine1)
@
-}
scale :: (Functor f, Num b) => b -> b -> f b -> f b
scale from to p = ((+ from) . (* (to-from))) <$> p

{- | `scalex` is an exponential version of `scale`, good for using with
frequencies.  Do *not* use negative numbers or zero as arguments! -}
scalex :: (Functor f, Floating b) => b -> b -> f b -> f b
scalex from to p = exp <$> scale (log from) (log to) p

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

{-
{- | like `striate`, but with an offset to the begin and end values -}
striateO :: Pattern Int -> Pattern Double -> ControlPattern -> ControlPattern
striateO = tParam2 _striateO

_striateO :: Int -> Double -> ControlPattern -> ControlPattern
_striateO n o p = _striate n p |+ begin (pure o :: Pattern Double) |+ end (pure o :: Pattern Double)

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
_gap n p = Pattern $ \queryA -> concatMap (f queryA) $ arcCycles queryA
     where f queryA a = concatMap (chopEvent queryA) (arc p a)
           chopEvent (queryS, queryE) (a,_a',v) = map (newEvent v) $ filter (\(_, (s,e)) -> not $ or [e < queryS, s >= queryE]) (enumerate $ everyOther $ chopArc a n)
           newEvent :: ControlMap -> (Int, Arc) -> Event ControlMap
           newEvent v (i, a) = (a,a,Map.insert "end" (VF ((fromIntegral $ i+1)/(fromIntegral n))) $ Map.insert "begin" (VF ((fromIntegral i)/(fromIntegral n))) v)
           everyOther (x:_:xs) = x:everyOther xs
           everyOther xs = xs

{-
normEv :: Event a -> Event a -> Event a
normEv ev@(_, (s,e), _) ev'@(_, (s',e'), _)
       | not on && not off = [] -- shouldn't happen
       | on && off = splitEv ev'
       | not on && s' > sam s = []
       | not off && e' < nextSam s = [(fst' ev, mapSnd' (mapSnd (min $ nextSam s)) ev, thd' ev)]
  where on = onsetIn (sam s, nextSam s) ev
        off = offsetIn (sam s, nextSam s) ev
        eplitEv
-}
--mapCycleEvents :: Pattern a -> ([Event a] -> [Event a]) -> Pattern a
--mapCycleEvents p f = splitQueries $ Pattern $ \(s,e) -> filter (\ev -> isJust $ subArc (s,e) (eventArc ev)) $ f $ arc p (sam s, nextSam s)

--off :: Time -> Pattern a -> Pattern a
--off t p = mapCycleEvents p (mapArcs (mapSnd wrappedPlus . mapFst wrappedPlus))
--               where wrapAtCycle f t' = sam t' + cyclePos (f t')
--                     wrappedPlus = wrapAtCycle (+t)


en :: [(Int, Int)] -> Pattern String -> Pattern String
en ns p = stack $ map (\(i, (k, n)) -> _e k n (samples p (pure i))) $ enumerate ns

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
              | otherwise = _slow t $ stack $ map (\(i, f) -> (fromIntegral i % l) `rotL` (_density t $ f (_slow t p))) (zip [0 ..] fs)
  where l = fromIntegral $ length fs

{- |
(A function that takes two OscPatterns, and blends them together into
a new OscPattern. An OscPattern is basically a pattern of messages to
a synthesiser.)

Shifts between the two given patterns, using distortion.

Example:

@
d1 $ interlace (sound  "bd sn kurt") (every 3 rev $ sound  "bd sn:2")
@
-}
interlace :: ControlPattern -> ControlPattern -> ControlPattern
interlace a b = weave 16 (shape $ ((* 0.9) <$> sine)) [a, b]

-- | Step sequencing
step :: String -> String -> Pattern String
step s steps = fastcat $ map f steps
    where f c | c == 'x' = pure s
              | isDigit c = pure $ s ++ ":" ++ [c]
              | otherwise = silence

steps :: [(String, String)] -> Pattern String
steps = stack . map (\(a,b) -> step a b)

-- | like `step`, but allows you to specify an array of strings to use for 0,1,2...
step' :: [String] -> String -> Pattern String
step' ss steps = fastcat $ map f steps
    where f c | c == 'x' = pure $ head ss
              | isDigit c = pure $ ss!!(digitToInt c)
              | otherwise = silence

off :: Pattern Time -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
off tp f p = unwrap $ (\tv -> _off tv f p) <$> tp

_off :: Time -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
_off t f p = superimpose (f . (t `rotR`)) p

offadd :: Num a => Pattern Time -> Pattern a -> Pattern a -> Pattern a
offadd tp pn p = off tp (+pn) p

{- | `up` does a poor man's pitchshift by semitones via `speed`.

You can easily produce melodies from a single sample with up:

@
d1 # up "0 5 4 12" # sound "arpy"
@

This will play the _arpy_ sample four times a cycle in the original pitch, pitched by 5 semitones, by 4 and then by an octave.
-}
up :: Pattern Double -> ControlPattern
up = speed . ((1.059466**) <$>)

ghost'' a f p = superimpose (((a*2.5) `rotR`) . f) $ superimpose (((a*1.5) `rotR`) . f) $ p
ghost' a p = ghost'' 0.125 ((|*| gain (pure 0.7)) . (|> end (pure 0.2)) . (|*| speed (pure 1.25))) p
ghost p = ghost' 0.125 p


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


{- |
   tabby - A more literal weaving than the `weave` function, give number
   of 'threads' per cycle and two patterns, and this function will weave them
   together using a plain (aka 'tabby') weave, with a simple over/under structure
 -}
tabby n p p' = stack [maskedWarp n p,
                      maskedWeft n p'
                     ]
  where
    weft n = concatMap (\x -> [[0..n-1],(reverse [0..n-1])]) [0 .. (n `div` 2) - 1]
    warp = transpose . weft
    thread xs n p = _slow (n%1) $ fastcat $ map (\i -> zoom (i%n,(i+1)%n) p) (concat xs)
    weftP n p = thread (weft n) n p
    warpP n p = thread (warp n) n p
    maskedWeft n p = mask (every 2 rev $ _density ((n)%2) "~ 1" :: Pattern Int) $ weftP n p
    maskedWarp n p = mask (every 2 rev $ _density ((n)%2) "1 ~" :: Pattern Int) $ warpP n p

hurry :: Pattern Rational -> ControlPattern -> ControlPattern
hurry x = (|*| speed (fromRational <$> x)) . fast x
-}

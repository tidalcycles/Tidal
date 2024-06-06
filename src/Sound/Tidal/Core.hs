{-# LANGUAGE FlexibleInstances #-}

{-
    Core.hs - For functions judged to be 'core' to tidal functionality.
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

module Sound.Tidal.Core where

import           Prelude             hiding ((*>), (<*))

import           Data.Fixed          (mod')
import qualified Data.Map.Strict     as Map
import           Data.Maybe          (fromMaybe)
import           Sound.Tidal.Pattern

-- ** Elemental patterns

{-| Takes a function of time to values, and turns it into a 'Pattern'.
  Useful for creating continuous patterns such as 'sine' or 'perlin'.

  For example, 'saw' is defined as

  > saw = sig $ \t -> mod' (fromRational t) 1
-}
sig :: (Time -> a) -> Pattern a
sig f = pattern q
  where q (State (Arc s e) _)
          | s > e = []
          | otherwise = [Event (Context []) Nothing (Arc s e) (f (s+((e-s)/2)))]

-- | @sine@ - unipolar sinewave. A pattern of continuous values following a
-- sinewave with frequency of one cycle, and amplitude from 0 to 1.
sine :: Fractional a => Pattern a
sine = sig $ \t -> (sin_rat ((pi :: Double) * 2 * fromRational t) + 1) / 2
  where sin_rat = fromRational . toRational . sin

-- | @sine2@ - bipolar sinewave. A pattern of continuous values following a
-- sinewave with frequency of one cycle, and amplitude from -1 to 1.
sine2 :: Fractional a => Pattern a
sine2 = sig $ \t -> sin_rat ((pi :: Double) * 2 * fromRational t)
  where sin_rat = fromRational . toRational . sin

-- | @cosine@ - unipolar cosine wave. A pattern of continuous values
-- following a cosine with frequency of one cycle, and amplitude from
-- 0 to 1. Equivalent to @0.25 ~> sine@.
cosine :: Fractional a => Pattern a
cosine = 0.25 `rotR` sine

-- | @cosine2@ - bipolar cosine wave. A pattern of continuous values
-- following a cosine with frequency of one cycle, and amplitude from
-- -1 to 1. Equivalent to @0.25 ~> sine2@.
cosine2 :: Fractional a => Pattern a
cosine2 = 0.25 `rotR` sine2

-- | @saw@ - unipolar ascending sawtooth wave. A pattern of continuous values
-- following a sawtooth with frequency of one cycle, and amplitude from
-- 0 to 1.
saw :: (Fractional a, Real a) => Pattern a
saw = sig $ \t -> mod' (fromRational t) 1

-- | @saw2@ - bipolar ascending sawtooth wave. A pattern of continuous values
-- following a sawtooth with frequency of one cycle, and amplitude from
-- -1 to 1.
saw2 :: (Fractional a, Real a) => Pattern a
saw2 = sig $ \t -> mod' (fromRational t) 1 * 2 - 1

-- | @isaw@ like @saw@, but a descending (inverse) sawtooth.
isaw :: (Fractional a, Real a) => Pattern a
isaw = (1-) <$> saw

-- | @isaw2@ like @saw2@, but a descending (inverse) sawtooth.
isaw2 :: (Fractional a, Real a) => Pattern a
isaw2 = (*(-1)) <$> saw2

-- | @tri@ - unipolar triangle wave. A pattern of continuous values
-- following a triangle wave with frequency of one cycle, and amplitude from
-- 0 to 1.
tri :: (Fractional a, Real a) => Pattern a
tri = fastAppend saw isaw

-- | @tri2@ - bipolar triangle wave. A pattern of continuous values
-- following a triangle wave with frequency of one cycle, and amplitude from
-- -1 to 1.
tri2 :: (Fractional a, Real a) => Pattern a
tri2 = fastAppend saw2 isaw2

-- | @square@ - unipolar square wave. A pattern of continuous values
-- following a square wave with frequency of one cycle, and amplitude from
-- 0 to 1.
-- | @square@ is like 'sine', for square waves.
square :: (Fractional a) => Pattern a
square = sig $
       \t -> fromIntegral ((floor $ mod' (fromRational t :: Double) 1 * 2) :: Integer)

-- | @square2@ - bipolar square wave. A pattern of continuous values
-- following a square wave with frequency of one cycle, and amplitude from
-- -1 to 1.
square2 :: (Fractional a) => Pattern a
square2 = sig $
       \t -> fromIntegral (floor (mod' (fromRational t :: Double) 1 * 2) * 2 - 1 :: Integer)

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
envEq = sig $ \t -> sqrt (sin (pi/2 * max 0 (min (fromRational (1-t)) 1)))

-- | Equal power reversed
envEqR :: Pattern Double
envEqR = sig $ \t -> sqrt (cos (pi/2 * max 0 (min (fromRational (1-t)) 1)))

-- ** Pattern algebra

-- class for types that support a left-biased union
class Unionable a where
  union :: a -> a -> a

-- default union is just to take the left hand side..
instance Unionable a where
  union = const

instance {-# OVERLAPPING #-} Unionable ValueMap where
  union = Map.union

(|+|) :: (Applicative a, Num b) => a b -> a b -> a b
a |+| b = (+) <$> a <*> b
(|+ ) :: Num a => Pattern a -> Pattern a -> Pattern a
a |+  b = (+) <$> a <* b
( +|) :: Num a => Pattern a -> Pattern a -> Pattern a
a  +| b = (+) <$> a *> b
(||+) :: Num a => Pattern a -> Pattern a -> Pattern a
a ||+ b = (+) <$> a <<* b


(|++|) :: Applicative a => a String -> a String -> a String
a |++| b = (++) <$> a <*> b
(|++ ) :: Pattern String -> Pattern String -> Pattern String
a |++  b = (++) <$> a <* b
( ++|) :: Pattern String -> Pattern String -> Pattern String
a  ++| b = (++) <$> a *> b
(||++) :: Pattern String -> Pattern String -> Pattern String
a ||++ b = (++) <$> a <<* b

(|/|) :: (Applicative a, Fractional b) => a b -> a b -> a b
a |/| b = (/) <$> a <*> b
(|/ ) :: Fractional a => Pattern a -> Pattern a -> Pattern a
a |/  b = (/) <$> a <* b
( /|) :: Fractional a => Pattern a -> Pattern a -> Pattern a
a  /| b = (/) <$> a *> b
(||/) :: Fractional a => Pattern a -> Pattern a -> Pattern a
a ||/ b = (/) <$> a <<* b

(|*|) :: (Applicative a, Num b) => a b -> a b -> a b
a |*| b = (*) <$> a <*> b
(|* ) :: Num a => Pattern a -> Pattern a -> Pattern a
a |*  b = (*) <$> a <* b
( *|) :: Num a => Pattern a -> Pattern a -> Pattern a
a  *| b = (*) <$> a *> b
(||*) :: Num a => Pattern a -> Pattern a -> Pattern a
a ||* b = (*) <$> a <<* b

(|-|) :: (Applicative a, Num b) => a b -> a b -> a b
a |-| b = (-) <$> a <*> b
(|- ) :: Num a => Pattern a -> Pattern a -> Pattern a
a |-  b = (-) <$> a <* b
( -|) :: Num a => Pattern a -> Pattern a -> Pattern a
a  -| b = (-) <$> a *> b
(||-) :: Num a => Pattern a -> Pattern a -> Pattern a
a ||- b = (-) <$> a <<* b

(|%|) :: (Applicative a, Moddable b) => a b -> a b -> a b
a |%| b = gmod <$> a <*> b
(|% ) :: Moddable a => Pattern a -> Pattern a -> Pattern a
a |%  b = gmod <$> a <* b
( %|) :: Moddable a => Pattern a -> Pattern a -> Pattern a
a  %| b = gmod <$> a *> b
(||%) :: Moddable a => Pattern a -> Pattern a -> Pattern a
a ||% b = gmod <$> a <<* b

(|**|) :: (Applicative a, Floating b) => a b -> a b -> a b
a |**| b = (**) <$> a <*> b
(|** ) :: Floating a => Pattern a -> Pattern a -> Pattern a
a |**  b = (**) <$> a <* b
( **|) :: Floating a => Pattern a -> Pattern a -> Pattern a
a  **| b = (**) <$> a *> b
(||**) :: Floating a => Pattern a -> Pattern a -> Pattern a
a ||** b = (**) <$> a <<* b

(|>|) :: (Applicative a, Unionable b) => a b -> a b -> a b
a |>| b = flip union <$> a <*> b
(|> ) :: Unionable a => Pattern a -> Pattern a -> Pattern a
a |>  b = flip union <$> a <* b
( >|) :: Unionable a => Pattern a -> Pattern a -> Pattern a
a  >| b = flip union <$> a *> b
(||>) :: Unionable a => Pattern a -> Pattern a -> Pattern a
a ||> b = flip union <$> a <<* b

(|<|) :: (Applicative a, Unionable b) => a b -> a b -> a b
a |<| b = union <$> a <*> b
(|< ) :: Unionable a => Pattern a -> Pattern a -> Pattern a
a |<  b = union <$> a <* b
( <|) :: Unionable a => Pattern a -> Pattern a -> Pattern a
a  <| b = union <$> a *> b
(||<) :: Unionable a => Pattern a -> Pattern a -> Pattern a
a ||< b = union <$> a <<* b

-- Backward compatibility - structure from left, values from right.
(#) :: Unionable b => Pattern b -> Pattern b -> Pattern b
(#) = (|>)



-- ** Constructing patterns

{-| Turns a list of values into a pattern, playing one of them per cycle.
  The following are equivalent:

  > d1 $ n (fromList [0, 1, 2]) # s "superpiano"
  > d1 $ n "<0 1 2>" # s "superpiano"
-}
fromList :: [a] -> Pattern a
fromList = cat . map pure

{-| Turns a list of values into a pattern, playing /all/ of them per cycle.
  The following are equivalent:

  > d1 $ n (fastFromList [0, 1, 2]) # s "superpiano"
  > d1 $ n "[0 1 2]" # s "superpiano"
-}
fastFromList :: [a] -> Pattern a
fastFromList = fastcat . map pure

-- | A synonym for 'fastFromList'
listToPat :: [a] -> Pattern a
listToPat = fastFromList

-- | 'fromMaybes; is similar to 'fromList', but allows values to
-- be optional using the 'Maybe' type, so that 'Nothing' results in
-- gaps in the pattern.
-- The following are equivalent:
-- > d1 $ n (fromMaybes [Just 0, Nothing, Just 2]) # s "superpiano"
-- > d1 $ n "0 ~ 2" # s "superpiano"
fromMaybes :: [Maybe a] -> Pattern a
fromMaybes = fastcat . map f
  where f Nothing  = silence
        f (Just x) = pure x

{-| A pattern of whole numbers from 0 to the given number, in a single cycle.
  Can be used used to @run@ through a folder of samples in order:

  > d1 $ n (run 8) # sound "amencutup"

  The first parameter to run can be given as a pattern:

  > d1 $ n (run "<4 8 4 6>") # sound "amencutup"
-}
run :: (Enum a, Num a) => Pattern a -> Pattern a
run = (>>= _run)

_run :: (Enum a, Num a) => a -> Pattern a
_run n = fastFromList [0 .. n-1]

-- | Similar to 'run', but starts from @1@ for the first cycle, successively
-- adds a number until it gets up to @n@.
-- > d1 $ n (scan 8) # sound "amencutup"
scan :: (Enum a, Num a) => Pattern a -> Pattern a
scan = (>>= _scan)

_scan :: (Enum a, Num a) => a -> Pattern a
_scan n = slowcat $ map _run [1 .. n]

-- ** Combining patterns

-- | Alternate between cycles of the two given patterns
-- > d1 $ append (sound "bd*2 sn") (sound "arpy jvbass*2")
append :: Pattern a -> Pattern a -> Pattern a
append a b = cat [a,b]

{- |
  Like 'append', but for a list of patterns. Interlaces them, playing the
  first cycle from each in turn, then the second cycle from each, and so on. It
  concatenates a list of patterns into a new pattern; each pattern in the list
  will maintain its original duration. For example:

  > d1 $ cat [sound "bd*2 sn", sound "arpy jvbass*2"]
  > d1 $ cat [sound "bd*2 sn", sound "arpy jvbass*2", sound "drum*2"]
  > d1 $ cat [sound "bd*2 sn", sound "jvbass*3", sound "drum*2", sound "ht mt"]
-}
cat :: [Pattern a] -> Pattern a
cat [] = silence
cat (p:[]) = p
cat ps = pattern q
  where n = length ps
        q st = concatMap (f st) $ arcCyclesZW (arc st)
        f st a = query (withResultTime (+offset) p) $ st {arc = Arc (subtract offset (start a)) (subtract offset (stop a))}
          where p = ps !! i
                cyc = (floor $ start a) :: Int
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
slowappend :: Pattern a -> Pattern a -> Pattern a
slowappend = append

-- | Like 'append', but twice as fast
-- > d1 $ fastAppend (sound "bd*2 sn") (sound "arpy jvbass*2")
fastAppend :: Pattern a -> Pattern a -> Pattern a
fastAppend a b = _fast 2 $ append a b
fastappend :: Pattern a -> Pattern a -> Pattern a
fastappend = fastAppend

{-| The same as 'cat', but speeds up the result by the number of
  patterns there are, so the cycles from each are squashed to fit a
  single cycle.

  > d1 $ fastcat [sound "bd*2 sn", sound "arpy jvbass*2"]
  > d1 $ fastcat [sound "bd*2 sn", sound "arpy jvbass*2", sound "drum*2"]
  > d1 $ fastcat [sound "bd*2 sn", sound "jvbass*3", sound "drum*2", sound "ht mt"]
-}
fastCat :: [Pattern a] -> Pattern a
fastCat (p:[]) = p
fastCat ps     = setTactus t $ _fast (toTime $ length ps) $ cat ps
  where t = fromMaybe (toRational $ length ps) $ ((* (toRational $ length ps)) . foldl1 lcmr) <$> (sequence $ map tactus ps)

-- | Alias for @fastCat@
fastcat :: [Pattern a] -> Pattern a
fastcat = fastCat

{- | Similar to @fastCat@, but each pattern is given a relative duration.
  You provide proportionate sizes of the patterns to each other for when they’re
  concatenated into one cycle. The larger the value in the list, the larger
  relative size the pattern takes in the final loop. If all values are equal
  then this is equivalent to fastcat (e.g. the following two code fragments are
  equivalent).

  > d1 $ fastcat [s "bd*4", s "hh27*8", s "superpiano" # n 0]

  > d1 $ timeCat [ (1, s "bd*4")
  >              , (1, s "hh27*8")
  >              , (1, s "superpiano" # n 0)
  >              ]

-}
timeCat :: [(Time, Pattern a)] -> Pattern a
timeCat ((_,p):[]) = p
timeCat tps = setTactus total $ stack $ map (\(s,e,p) -> compressArc (Arc (s/total) (e/total)) p) $ arrange 0 $ filter (\(t, _) -> t > 0) $ tps
    where total = sum $ map fst tps
          arrange :: Time -> [(Time, Pattern a)] -> [(Time, Time, Pattern a)]
          arrange _ []            = []
          arrange t ((t',p):tps') = (t,t+t',p) : arrange (t+t') tps'

-- | Alias for @timeCat@
timecat :: [(Time, Pattern a)] -> Pattern a
timecat = timeCat

{- | @overlay@ combines two 'Pattern's into a new pattern, so that their events
are combined over time. For example, the following two lines are equivalent:

> d1 $ sound (overlay "bd sn:2" "cp*3")
> d1 $ sound "[bd sn:2, cp*3]"

@overlay@ is equal to '<>',

> (<>) :: Semigroup a => a -> a -> a

which can thus be used as an infix operator equivalent of 'overlay':

> d1 $ sound ("bd sn:2" <> "cp*3")
-}
overlay :: Pattern a -> Pattern a -> Pattern a
overlay = (<>)

{- | 'stack' combines a list of 'Pattern's into a new pattern, so that their
events are combined over time, i.e., all of the patterns in the list are played
simultaneously.

> d1 $ stack [
>  sound "bd bd*2",
>  sound "hh*2 [sn cp] cp future*4",
>  sound "arpy" +| n "0 .. 15"
> ]

This is particularly useful if you want to apply a function or synth control
pattern to multiple patterns at once:

> d1 $ whenmod 5 3 (striate 3) $ stack [
>  sound "bd bd*2",
>  sound "hh*2 [sn cp] cp future*4",
>  sound "arpy" +| n "0 .. 15"
> ] # speed "[[1 0.8], [1.5 2]*2]/3"
-}
stack :: [Pattern a] -> Pattern a
stack pats = (foldr overlay silence pats) {tactus = t}
  where t | length pats == 0 = Nothing
          | otherwise = foldl1 lcmr <$> (sequence $ map tactus pats)

-- ** Manipulating time

-- | Shifts a pattern back in time by the given amount, expressed in cycles
(<~) :: Pattern Time -> Pattern a -> Pattern a
(<~) = patternify' rotL

-- | Shifts a pattern forward in time by the given amount, expressed in cycles
(~>) :: Pattern Time -> Pattern a -> Pattern a
(~>) = patternify' rotR

{-| Slow down a pattern by the factors in the given time pattern, "squeezing"
  the pattern to fit the slot given in the time pattern. It is the slow analogue
  to 'fastSqueeze'.

  If the time pattern only has a single value in a cycle, @slowSqueeze@ becomes equivalent to slow. These are equivalent:

  > d1 $ slow "<2 4>" $ s "bd*8"
  > d1 $ slowSqueeze "<2 4>" $ s "bd*8"

  When the time pattern has multiple values, however, the behavior is a little
  different. Instead, a slowed version of the pattern will be made for each value
  in the time pattern, and they’re all combined together in a cycle according to
  the structure of the time pattern. For example, these are equivalent:

  > d1 $ slowSqueeze "2 4 8 16" $ s "bd*8"
  > d1 $ s "bd*4 bd*2 bd bd/2"

  as are these:

  > d1 $ slowSqueeze "2 4 [8 16]" $ s "bd*8"
  > d1 $ s "bd*4 bd*2 [bd bd/2]"
-}
slowSqueeze :: Pattern Time -> Pattern a -> Pattern a
slowSqueeze = patternifySqueeze _slow

-- | An alias for @slow@
sparsity :: Pattern Time -> Pattern a -> Pattern a
sparsity = slow

{- | Plays a portion of a pattern, specified by a time arc (start and end time).
  The new resulting pattern is played over the time period of the original pattern.

  > d1 $ zoom (0.25, 0.75) $ sound "bd*2 hh*3 [sn bd]*2 drum"

  In the pattern above, @zoom@ is used with an arc from 25% to 75%. It is
  equivalent to:

  > d1 $ sound "hh*3 [sn bd]*2"

  Here’s an example of it being used with a conditional:

  > d1 $ every 4 (zoom (0.25, 0.75)) $ sound "bd*2 hh*3 [sn bd]*2 drum"
-}
zoom :: (Time, Time) -> Pattern a -> Pattern a
zoom (s,e) = zoomArc (Arc s e)

zoomArc :: Arc -> Pattern a -> Pattern a
zoomArc (Arc s e) p | s >= e = nothing
                    | otherwise = withTactus (*d) $ splitQueries $
  withResultArc (mapCycle ((/d) . subtract s)) $ withQueryArc (mapCycle ((+s) . (*d))) p
     where d = e-s

{-| @fastGap@ is similar to 'fast' but maintains its cyclic alignment, i.e.,
  rather than playing the pattern multiple times, it instead leaves a gap in
  the remaining space of the cycle. For example, @fastGap 2 p@ would squash the
  events in pattern @p@ into the first half of each cycle (and the second halves
  would be empty). The factor should be at least 1.
-}
fastGap :: Pattern Time -> Pattern a -> Pattern a
fastGap = patternify _fastGap

-- | An alias for @fastGap@
densityGap :: Pattern Time -> Pattern a -> Pattern a
densityGap = fastGap

{-|
  @compress@ takes a pattern and squeezes it within the specified time span (i.e.
  the ‘arc’). The new resulting pattern is a sped up version of the original.

  > d1 $ compress (1/4, 3/4) $ s "[bd sn]!"

  In the above example, the pattern will play in an arc spanning from 25% to 75%
  of the duration of a cycle. It is equivalent to:

  > d1 $ s "~ [bd sn]! ~"

  Another example, where all events are different:

  > d1 $ compress (1/4, 3/4) $ n (run 4) # s "arpy"

  It differs from 'zoom' in that it preserves the original pattern but it speeds
  up its events so to match with the new time period.
-}
compress :: (Time,Time) -> Pattern a -> Pattern a
compress (s,e) = compressArc (Arc s e)

compressTo :: (Time,Time) -> Pattern a -> Pattern a
compressTo (s,e) = compressArcTo (Arc s e)

repeatCycles :: Pattern Int -> Pattern a -> Pattern a
repeatCycles = patternify _repeatCycles

_repeatCycles :: Int -> Pattern a -> Pattern a
_repeatCycles n p = cat (replicate n p)

fastRepeatCycles :: Int -> Pattern a -> Pattern a
fastRepeatCycles n p = cat (replicate n p)

-- | * Higher order functions

-- | Functions which work on other functions (higher order functions)

{- | @every n f p@ applies the function @f@ to @p@, but only affects
  every @n@ cycles.

  It takes three inputs: how often the function should be applied (e.g. 3 to
  apply it every 3 cycles), the function to be applied, and the pattern you are
  applying it to. For example: to reverse a pattern every three cycles (and for
  the other two play it normally)

  > d1 $ every 3 rev $ n "0 1 [~ 2] 3" # sound "arpy"

  Note that if the function you’re applying requires additional parameters
  itself (such as fast 2 to make a pattern twice as fast), then you’ll need to
  wrap it in parenthesis, like so:

  > d1 $ every 3 (fast 2) $ n "0 1 [~ 2] 3" # sound "arpy"

  Otherwise, the every function will think it is being passed too many parameters.
-}
every :: Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
every tp f p = innerJoin $ (\t -> _every t f p) <$> tp

_every :: Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
_every 0 _ p = p
_every n f p = when ((== 0) . (`mod` n)) f p

{-| @every' n o f p@ is like @every n f p@ but with an offset of @o@ cycles.

  For example, @every' 3 0 (fast 2)@ will speed up the cycle on cycles 0,3,6,…
  whereas @every' 3 1 (fast 2)@ will transform the pattern on cycles 1,4,7,….

  With this in mind, setting the second argument of @every'@ to 0 gives the
  equivalent every function. For example, every 3 is equivalent to every' 3 0.

  The @every@ functions can be used to silence a full cycle or part of a cycle
  by using silent or mask "~". Mask provides additional flexibility to turn on/off
  individual steps.

  > d1 $ every 3 silent $ n "2 9 11 2" # s "hh27"
  > d1 $ every 3 (mask "~") $ n "2 9 10 2" # s "hh27"
  > d1 $ every 3 (mask "0 0 0 0") $ n "2 9 11 2" # s "hh27"
-}
every' :: Pattern Int -> Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
every' np op f p = do { n <- np; o <- op; _every' n o f p }

_every' :: Int -> Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
_every' n o = when ((== o) . (`mod` n))

{- | @foldEvery ns f p@ applies the function @f@ to @p@, and is applied for
  each cycle in @ns@.

  It is similar to chaining multiple @every@ functions together. It transforms
  a pattern with a function, once per any of the given number of cycles. If a
  particular cycle is the start of more than one of the given cycle periods, then
  it it applied more than once.

  > d1 $ foldEvery [5,3] (|+ n 1) $ s "moog" # legato 1

  The first moog samples are tuned to C2, C3 and C4. Note how on cycles that are
  multiples of 3 or 5 the pitch is an octave higher, and on multiples of 15 the
  pitch is two octaves higher, as the transformation is applied twice.
-}
foldEvery :: [Int] -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
foldEvery ns f p = foldr (`_every` f) p ns

{-|
The given pattern transformation is applied only @when@ the given test function
returns @True@. The test function will be called with the current cycle as
a number.

> d1 $ when (elem '4' . show)
>           (striate 4)
>    $ sound "hh hc"

The above will only apply @striate 4@ to the pattern if the current
cycle number contains the number 4. So the fourth cycle will be
striated and the fourteenth and so on. Expect lots of striates after
cycle number 399.
-}
when :: (Int -> Bool) -> (Pattern a -> Pattern a) ->  Pattern a -> Pattern a
when test f p = splitQueries $ p {query = apply}
  where apply st | test (floor $ start $ arc st) = query (f p) st
                 | otherwise = query p st

{- | Like 'when', but works on continuous time values rather than cycle numbers.
  The following will apply @# speed 2@ only when the remainder of the current
  @Time@ divided by 2 is less than 0.5:

  > d1 $ whenT ((< 0.5) . (flip Data.Fixed.mod' 2))
  >            (# speed 2)
  >    $ sound "hh(4,8) hc(3,8)"
-}
whenT :: (Time -> Bool) -> (Pattern a -> Pattern a) ->  Pattern a -> Pattern a
whenT test f p = splitQueries $ p {query = apply}
  where apply st | test (start $ arc st) = query (f p) st
                 | otherwise = query p st

_getP_ :: (Value -> Maybe a) -> Pattern Value -> Pattern a
_getP_ f pat = filterJust $ f <$> pat

_getP :: a -> (Value -> Maybe a) -> Pattern Value -> Pattern a
_getP d f pat = fromMaybe d . f <$> pat

_cX :: a -> (Value -> Maybe a) -> String -> Pattern a
_cX d f s = pattern $ \(State a m) -> queryArc (maybe (pure d) (_getP d f . valueToPattern) $ Map.lookup s m) a

_cX_ :: (Value -> Maybe a) -> String -> Pattern a
_cX_ f s = pattern $ \(State a m) -> queryArc (maybe silence (_getP_ f . valueToPattern) $ Map.lookup s m) a

cF :: Double -> String -> Pattern Double
cF d = _cX d getF
cF_ :: String -> Pattern Double
cF_ = _cX_ getF
cF0 :: String -> Pattern Double
cF0 = _cX 0 getF

cN :: Note -> String -> Pattern Note
cN d = _cX d getN
cN_ :: String -> Pattern Note
cN_ = _cX_ getN
cN0 :: String -> Pattern Note
cN0 = _cX (Note 0) getN

cI :: Int -> String -> Pattern Int
cI d = _cX d getI
cI_ :: String -> Pattern Int
cI_ = _cX_ getI
cI0 :: String -> Pattern Int
cI0 = _cX 0 getI

cB :: Bool -> String -> Pattern Bool
cB d = _cX d getB
cB_ :: String -> Pattern Bool
cB_ = _cX_ getB
cB0 :: String -> Pattern Bool
cB0 = _cX False getB

cR :: Rational -> String -> Pattern Rational
cR d = _cX d getR
cR_ :: String -> Pattern Rational
cR_ = _cX_ getR
cR0 :: String -> Pattern Rational
cR0 = _cX 0 getR

cT :: Time -> String -> Pattern Time
cT = cR
cT0 :: String -> Pattern Time
cT0 = cR0
cT_ :: String -> Pattern Time
cT_ = cR_

cS :: String -> String -> Pattern String
cS d = _cX d getS
cS_ :: String -> Pattern String
cS_ = _cX_ getS
cS0 :: String -> Pattern String
cS0 = _cX "" getS

-- Default controller inputs (for MIDI)
in0 :: Pattern Double
in0 = cF 0 "0"
in1 :: Pattern Double
in1 = cF 0 "1"
in2 :: Pattern Double
in2 = cF 0 "2"
in3 :: Pattern Double
in3 = cF 0 "3"
in4 :: Pattern Double
in4 = cF 0 "4"
in5 :: Pattern Double
in5 = cF 0 "5"
in6 :: Pattern Double
in6 = cF 0 "6"
in7 :: Pattern Double
in7 = cF 0 "7"
in8 :: Pattern Double
in8 = cF 0 "8"
in9 :: Pattern Double
in9 = cF 0 "9"
in10 :: Pattern Double
in10 = cF 0 "10"
in11 :: Pattern Double
in11 = cF 0 "11"
in12 :: Pattern Double
in12 = cF 0 "12"
in13 :: Pattern Double
in13 = cF 0 "13"
in14 :: Pattern Double
in14 = cF 0 "14"
in15 :: Pattern Double
in15 = cF 0 "15"
in16 :: Pattern Double
in16 = cF 0 "16"
in17 :: Pattern Double
in17 = cF 0 "17"
in18 :: Pattern Double
in18 = cF 0 "18"
in19 :: Pattern Double
in19 = cF 0 "19"
in20 :: Pattern Double
in20 = cF 0 "20"
in21 :: Pattern Double
in21 = cF 0 "21"
in22 :: Pattern Double
in22 = cF 0 "22"
in23 :: Pattern Double
in23 = cF 0 "23"
in24 :: Pattern Double
in24 = cF 0 "24"
in25 :: Pattern Double
in25 = cF 0 "25"
in26 :: Pattern Double
in26 = cF 0 "26"
in27 :: Pattern Double
in27 = cF 0 "27"
in28 :: Pattern Double
in28 = cF 0 "28"
in29 :: Pattern Double
in29 = cF 0 "29"
in30 :: Pattern Double
in30 = cF 0 "30"
in31 :: Pattern Double
in31 = cF 0 "31"
in32 :: Pattern Double
in32 = cF 0 "32"
in33 :: Pattern Double
in33 = cF 0 "33"
in34 :: Pattern Double
in34 = cF 0 "34"
in35 :: Pattern Double
in35 = cF 0 "35"
in36 :: Pattern Double
in36 = cF 0 "36"
in37 :: Pattern Double
in37 = cF 0 "37"
in38 :: Pattern Double
in38 = cF 0 "38"
in39 :: Pattern Double
in39 = cF 0 "39"
in40 :: Pattern Double
in40 = cF 0 "40"
in41 :: Pattern Double
in41 = cF 0 "41"
in42 :: Pattern Double
in42 = cF 0 "42"
in43 :: Pattern Double
in43 = cF 0 "43"
in44 :: Pattern Double
in44 = cF 0 "44"
in45 :: Pattern Double
in45 = cF 0 "45"
in46 :: Pattern Double
in46 = cF 0 "46"
in47 :: Pattern Double
in47 = cF 0 "47"
in48 :: Pattern Double
in48 = cF 0 "48"
in49 :: Pattern Double
in49 = cF 0 "49"
in50 :: Pattern Double
in50 = cF 0 "50"
in51 :: Pattern Double
in51 = cF 0 "51"
in52 :: Pattern Double
in52 = cF 0 "52"
in53 :: Pattern Double
in53 = cF 0 "53"
in54 :: Pattern Double
in54 = cF 0 "54"
in55 :: Pattern Double
in55 = cF 0 "55"
in56 :: Pattern Double
in56 = cF 0 "56"
in57 :: Pattern Double
in57 = cF 0 "57"
in58 :: Pattern Double
in58 = cF 0 "58"
in59 :: Pattern Double
in59 = cF 0 "59"
in60 :: Pattern Double
in60 = cF 0 "60"
in61 :: Pattern Double
in61 = cF 0 "61"
in62 :: Pattern Double
in62 = cF 0 "62"
in63 :: Pattern Double
in63 = cF 0 "63"
in64 :: Pattern Double
in64 = cF 0 "64"
in65 :: Pattern Double
in65 = cF 0 "65"
in66 :: Pattern Double
in66 = cF 0 "66"
in67 :: Pattern Double
in67 = cF 0 "67"
in68 :: Pattern Double
in68 = cF 0 "68"
in69 :: Pattern Double
in69 = cF 0 "69"
in70 :: Pattern Double
in70 = cF 0 "70"
in71 :: Pattern Double
in71 = cF 0 "71"
in72 :: Pattern Double
in72 = cF 0 "72"
in73 :: Pattern Double
in73 = cF 0 "73"
in74 :: Pattern Double
in74 = cF 0 "74"
in75 :: Pattern Double
in75 = cF 0 "75"
in76 :: Pattern Double
in76 = cF 0 "76"
in77 :: Pattern Double
in77 = cF 0 "77"
in78 :: Pattern Double
in78 = cF 0 "78"
in79 :: Pattern Double
in79 = cF 0 "79"
in80 :: Pattern Double
in80 = cF 0 "80"
in81 :: Pattern Double
in81 = cF 0 "81"
in82 :: Pattern Double
in82 = cF 0 "82"
in83 :: Pattern Double
in83 = cF 0 "83"
in84 :: Pattern Double
in84 = cF 0 "84"
in85 :: Pattern Double
in85 = cF 0 "85"
in86 :: Pattern Double
in86 = cF 0 "86"
in87 :: Pattern Double
in87 = cF 0 "87"
in88 :: Pattern Double
in88 = cF 0 "88"
in89 :: Pattern Double
in89 = cF 0 "89"
in90 :: Pattern Double
in90 = cF 0 "90"
in91 :: Pattern Double
in91 = cF 0 "91"
in92 :: Pattern Double
in92 = cF 0 "92"
in93 :: Pattern Double
in93 = cF 0 "93"
in94 :: Pattern Double
in94 = cF 0 "94"
in95 :: Pattern Double
in95 = cF 0 "95"
in96 :: Pattern Double
in96 = cF 0 "96"
in97 :: Pattern Double
in97 = cF 0 "97"
in98 :: Pattern Double
in98 = cF 0 "98"
in99 :: Pattern Double
in99 = cF 0 "99"
in100 :: Pattern Double
in100 = cF 0 "100"
in101 :: Pattern Double
in101 = cF 0 "101"
in102 :: Pattern Double
in102 = cF 0 "102"
in103 :: Pattern Double
in103 = cF 0 "103"
in104 :: Pattern Double
in104 = cF 0 "104"
in105 :: Pattern Double
in105 = cF 0 "105"
in106 :: Pattern Double
in106 = cF 0 "106"
in107 :: Pattern Double
in107 = cF 0 "107"
in108 :: Pattern Double
in108 = cF 0 "108"
in109 :: Pattern Double
in109 = cF 0 "109"
in110 :: Pattern Double
in110 = cF 0 "110"
in111 :: Pattern Double
in111 = cF 0 "111"
in112 :: Pattern Double
in112 = cF 0 "112"
in113 :: Pattern Double
in113 = cF 0 "113"
in114 :: Pattern Double
in114 = cF 0 "114"
in115 :: Pattern Double
in115 = cF 0 "115"
in116 :: Pattern Double
in116 = cF 0 "116"
in117 :: Pattern Double
in117 = cF 0 "117"
in118 :: Pattern Double
in118 = cF 0 "118"
in119 :: Pattern Double
in119 = cF 0 "119"
in120 :: Pattern Double
in120 = cF 0 "120"
in121 :: Pattern Double
in121 = cF 0 "121"
in122 :: Pattern Double
in122 = cF 0 "122"
in123 :: Pattern Double
in123 = cF 0 "123"
in124 :: Pattern Double
in124 = cF 0 "124"
in125 :: Pattern Double
in125 = cF 0 "125"
in126 :: Pattern Double
in126 = cF 0 "126"
in127 :: Pattern Double
in127 = cF 0 "127"

{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-
    Pattern.hs - core representation of Tidal patterns
    Copyright (C) 2020 Alex McLean and contributors

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

module Sound.Tidal.Pattern (module Sound.Tidal.Pattern,
                            module Sound.Tidal.Time
                           )
where

import           Prelude             hiding ((*>), (<*))

import           Control.Applicative (liftA2)
import           Control.DeepSeq     (NFData)
import           Control.Monad       ((>=>))
import           Data.Data           (Data)
import           Data.Fixed          (mod')
import           Data.List           (delete, findIndex, (\\))
import qualified Data.Map.Strict     as Map
import           Data.Maybe          (catMaybes, fromJust, isJust, mapMaybe)
import           Data.Typeable       (Typeable)
import           Data.Word           (Word8)
import           GHC.Generics        (Generic)

import           Sound.Tidal.Time

------------------------------------------------------------------------
-- * Types

-- | an Arc and some named control values
data State = State {arc      :: Arc,
                    controls :: ValueMap
                   }

-- | A datatype representing events taking place over time
data Pattern a = Pattern {query :: State -> [Event a], tactus :: Maybe Rational, pureValue :: Maybe a}
  deriving (Generic, Functor)

instance NFData a => NFData (Pattern a)

pattern :: (State -> [Event a]) -> Pattern a
pattern f = Pattern f Nothing Nothing

setTactus :: Rational -> Pattern a -> Pattern a
setTactus r p = p {tactus = Just r}

setTactusFrom :: Pattern b -> Pattern a -> Pattern a
setTactusFrom a b = b {tactus = tactus a}

withTactus :: (Rational -> Rational) -> Pattern a -> Pattern a
withTactus f p = p {tactus = f <$> tactus p}

_steps :: Rational -> Pattern a -> Pattern a
_steps target p@(Pattern _ (Just t) _) = setTactus target $ _fast (target / t) p
-- raise error?
_steps  _ p                            =  p

steps :: Pattern Rational -> Pattern a -> Pattern a
steps = patternify _steps

keepMeta :: Pattern a -> Pattern a -> Pattern a
keepMeta from to = to {tactus = tactus from, pureValue = pureValue from}

keepTactus :: Pattern a -> Pattern b -> Pattern b
keepTactus from to = to {tactus = tactus from}

-- type StateMap = Map.Map String (Pattern Value)
type ControlPattern = Pattern ValueMap

-- * Applicative and friends

instance Applicative Pattern where
  -- | Repeat the given value once per cycle, forever
  pure v = Pattern q (Just 1) (Just v)
    where q (State a _) =
            map (\a' -> Event
                        (Context [])
                        (Just a')
                        (sect a a')
                        v)
            $ cycleArcsInArc a

  -- | In each of @a <*> b@, @a <* b@ and @a *> b@
  -- (using the definitions from this module, not the Prelude),
  -- the time structure of the result
  -- depends on the structures of both @a@ and @b@.
  -- They all result in @Event@s with identical @part@s and @value@s.
  -- However, their @whole@s are different.
  --
  -- For instance, @listToPat [(+1), (+2)] <*> "0 10 100"@
  -- gives the following 4-@Event@ cycle:
  -- > (0>⅓)|1
  -- > (⅓>½)|11
  -- > (½>⅔)|12
  -- > (⅔>1)|102
  -- If we use @<*@ instead, we get this:
  -- > (0>⅓)-½|1
  -- > 0-(⅓>½)|11
  -- > (½>⅔)-1|12
  -- > ½-(⅔>1)|102
  -- And if we use @*>@, we get this:
  -- >   (0>⅓)|1
  -- > (⅓>½)-⅔|11
  -- > ⅓-(½>⅔)|12
  -- >   (⅔>1)|102
  (<*>) a b = (applyPatToPatBoth a b) {tactus = lcmr <$> tactus a <*> tactus b }

-- | Like @<*>@, but the "wholes" come from the left
(<*) :: Pattern (a -> b) -> Pattern a -> Pattern b
(<*) a b = keepTactus a $ applyPatToPatLeft a b

-- | Like @<*>@, but the "wholes" come from the right
(*>) :: Pattern (a -> b) -> Pattern a -> Pattern b
(*>) a b = keepTactus b $ applyPatToPatRight a b

-- | Like @<*>@, but the "wholes" come from the left
(<<*) :: Pattern (a -> b) -> Pattern a -> Pattern b
(<<*) a b = (applyPatToPatSqueeze a b) {tactus = (*) <$> tactus a <*> tactus b }

infixl 4 <*, *>, <<*
applyPatToPat :: (Maybe Arc -> Maybe Arc -> Maybe (Maybe Arc)) -> Pattern (a -> b) -> Pattern a -> Pattern b
applyPatToPat combineWholes pf px = pattern q
    where q st = catMaybes $ concatMap match $ query pf st
            where
              match ef@(Event (Context c) _ fPart f) =
                map
                (\ex@(Event (Context c') _ xPart x) ->
                  do whole' <- combineWholes (whole ef) (whole ex)
                     part' <- subArc fPart xPart
                     return (Event (Context $ c ++ c') whole' part' (f x))
                )
                (query px $ st {arc = wholeOrPart ef})

applyPatToPatBoth :: Pattern (a -> b) -> Pattern a -> Pattern b
applyPatToPatBoth pf px = pattern q
    where q st = catMaybes $ (concatMap match $ query pf st) ++ (concatMap matchX $ query (filterAnalog px) st)
            where
              -- match analog events from pf with all events from px
              match ef@(Event _ Nothing fPart _)   = map (withFX ef) (query px $ st {arc = fPart}) -- analog
              -- match digital events from pf with digital events from px
              match ef@(Event _ (Just fWhole) _ _) = map (withFX ef) (query (filterDigital px) $ st {arc = fWhole}) -- digital
              -- match analog events from px (constrained above) with digital events from px
              matchX ex@(Event _ Nothing fPart _)  = map (`withFX` ex) (query (filterDigital pf) $ st {arc = fPart}) -- digital
              matchX _ = error "can't happen"
              withFX ef ex = do whole' <- subMaybeArc (whole ef) (whole ex)
                                part' <- subArc (part ef) (part ex)
                                return (Event (combineContexts [context ef, context ex]) whole' part' (value ef $ value ex))

applyPatToPatLeft :: Pattern (a -> b) -> Pattern a -> Pattern b
applyPatToPatLeft pf px = pattern q
    where q st = catMaybes $ concatMap match $ query pf st
            where
              match ef = map (withFX ef) (query px $ st {arc = wholeOrPart ef})
              withFX ef ex = do let whole' = whole ef
                                part' <- subArc (part ef) (part ex)
                                return (Event (combineContexts [context ef, context ex]) whole' part' (value ef $ value ex))

applyPatToPatRight :: Pattern (a -> b) -> Pattern a -> Pattern b
applyPatToPatRight pf px = pattern q
    where q st = catMaybes $ concatMap match $ query px st
            where
              match ex = map (`withFX` ex) (query pf $ st {arc = wholeOrPart ex})
              withFX ef ex = do let whole' = whole ex
                                part' <- subArc (part ef) (part ex)
                                return (Event (combineContexts [context ef, context ex]) whole' part' (value ef $ value ex))

applyPatToPatSqueeze :: Pattern (a -> b) -> Pattern a -> Pattern b
applyPatToPatSqueeze pf px = squeezeJoin $ (\f -> f <$> px) <$> pf

-- * Monad and friends
--
-- $monadAndFriends
--
-- Note there are four ways of joining - the default 'unwrap' used by @>>=@, as well
-- as @innerJoin@, @innerJoin@ and @squeezeJoin@.

instance Monad Pattern where
  return = pure
  p >>= f = unwrap (f <$> p)

-- | Turns a pattern of patterns into a single pattern.
-- (this is actually 'join')
--
-- 1/ For query 'arc', get the events from the outer pattern @pp@
-- 2/ Query the inner pattern using the 'part' of the outer
-- 3/ For each inner event, set the whole and part to be the intersection
--    of the outer whole and part, respectively
-- 4/ Concatenate all the events together (discarding wholes/parts that didn't intersect)
--
-- TODO - what if a continuous pattern contains a discrete one, or vice-versa?
unwrap :: Pattern (Pattern a) -> Pattern a
unwrap pp = pp {query = q, pureValue = Nothing}
  where q st = concatMap
          (\(Event c w p v) ->
             mapMaybe (munge c w p) $ query v st {arc = p})
          (query pp st)
        munge oc ow op (Event ic iw ip v') =
          do
            w' <- subMaybeArc ow iw
            p' <- subArc op ip
            return (Event (combineContexts [ic, oc]) w' p' v')

-- | Turns a pattern of patterns into a single pattern. Like @unwrap@,
-- but structure only comes from the inner pattern.
innerJoin :: Pattern (Pattern a) -> Pattern a
innerJoin pp = pp {query = q, pureValue = Nothing}
  where q st = concatMap
               (\(Event oc _ op v) -> mapMaybe (munge oc) $ query v st {arc = op}
          )
          (query pp st)
          where munge oc (Event ic iw ip v) =
                  do
                    p <- subArc (arc st) ip
                    p' <- subArc p (arc st)
                    return (Event (combineContexts [ic, oc]) iw p' v)

-- | Turns a pattern of patterns into a single pattern. Like @unwrap@,
-- but structure only comes from the outer pattern.
outerJoin :: Pattern (Pattern a) -> Pattern a
outerJoin pp = pp {query = q, pureValue = Nothing}
  where q st = concatMap
          (\e ->
             mapMaybe (munge (context e) (whole e) (part e)) $ query (value e) st {arc = pure (start $ wholeOrPart e)}
          )
          (query pp st)
          where munge oc ow op (Event ic _ _ v') =
                  do
                    p' <- subArc (arc st) op
                    return (Event (combineContexts [oc, ic]) ow p' v')

-- | Like @unwrap@, but cycles of the inner patterns are compressed to fit the
-- timespan of the outer whole (or the original query if it's a continuous pattern?)
-- TODO - what if a continuous pattern contains a discrete one, or vice-versa?
squeezeJoin :: Pattern (Pattern a) -> Pattern a
squeezeJoin pp = pp {query = q, pureValue = Nothing}
  where q st = concatMap
          (\e@(Event c w p v) ->
             mapMaybe (munge c w p) $ query (focusArc (wholeOrPart e) v) st {arc = p}
          )
          (query pp st)
        munge oContext oWhole oPart (Event iContext iWhole iPart v) =
          do w' <- subMaybeArc oWhole iWhole
             p' <- subArc oPart iPart
             return (Event (combineContexts [iContext, oContext]) w' p' v)


_trigJoin :: Bool -> Pattern (Pattern a) -> Pattern a
_trigJoin cycleZero pat_of_pats = pattern q
  where q st =
          catMaybes $
          concatMap
          (\(Event oc jow op ov) ->
             map (\(Event ic (iw) ip iv) ->
                    do w <- subMaybeArc jow iw
                       p <- subArc op ip
                       return $ Event (combineContexts [ic, oc]) w p iv
                 )
               $ query (((if cycleZero then id else cyclePos) $ start (fromJust jow)) `rotR` ov) st
          )
          (query (filterDigital pat_of_pats) st)

trigJoin :: Pattern (Pattern a) -> Pattern a
trigJoin = _trigJoin False

trigZeroJoin :: Pattern (Pattern a) -> Pattern a
trigZeroJoin = _trigJoin True

reset :: Pattern Bool -> Pattern a -> Pattern a
reset bp pat = trigJoin $ (\v -> if v then pat else silence) <$> bp

resetTo :: Pattern Rational -> Pattern a -> Pattern a
resetTo bp pat = trigJoin $ (\v -> rotL v pat) <$> bp

restart :: Pattern Bool -> Pattern a -> Pattern a
restart bp pat = trigZeroJoin $ (\v -> if v then pat else silence) <$> bp

restartTo :: Pattern Rational -> Pattern a -> Pattern a
restartTo bp pat = trigZeroJoin $ (\v -> rotL v pat) <$> bp

-- | * Patterns as numbers

noOv :: String -> a
noOv meth = error $ meth ++ ": not supported for patterns"

instance Eq (Pattern a) where
  (==) = noOv "(==)"

instance Ord a => Ord (Pattern a) where
  min = liftA2 min
  max = liftA2 max
  compare = noOv "compare"
  (<=) = noOv "(<=)"

instance Num a => Num (Pattern a) where
  negate      = fmap negate
  (+)         = liftA2 (+)
  (*)         = liftA2 (*)
  fromInteger = pure . fromInteger
  abs         = fmap abs
  signum      = fmap signum

instance Enum a => Enum (Pattern a) where
  succ           = fmap succ
  pred           = fmap pred
  toEnum         = pure . toEnum
  fromEnum       = noOv "fromEnum"
  enumFrom       = noOv "enumFrom"
  enumFromThen   = noOv "enumFromThen"
  enumFromTo     = noOv "enumFromTo"
  enumFromThenTo = noOv "enumFromThenTo"

instance Monoid (Pattern a) where
  mempty = empty

instance Semigroup (Pattern a) where
  (<>) !p !p' = pattern $ \st -> query p st ++ query p' st

instance (Num a, Ord a) => Real (Pattern a) where
  toRational = noOv "toRational"

instance (Integral a) => Integral (Pattern a) where
  quot          = liftA2 quot
  rem           = liftA2 rem
  div           = liftA2 div
  mod           = liftA2 mod
  toInteger     = noOv "toInteger"
  x `quotRem` y = (x `quot` y, x `rem` y)
  x `divMod`  y = (x `div`  y, x `mod` y)

instance (Fractional a) => Fractional (Pattern a) where
  recip        = fmap recip
  fromRational = pure . fromRational

instance (Floating a) => Floating (Pattern a) where
  pi    = pure pi
  sqrt  = fmap sqrt
  exp   = fmap exp
  log   = fmap log
  sin   = fmap sin
  cos   = fmap cos
  asin  = fmap asin
  atan  = fmap atan
  acos  = fmap acos
  sinh  = fmap sinh
  cosh  = fmap cosh
  asinh = fmap asinh
  atanh = fmap atanh
  acosh = fmap acosh

instance (RealFrac a) => RealFrac (Pattern a) where
  properFraction = noOv "properFraction"
  truncate       = noOv "truncate"
  round          = noOv "round"
  ceiling        = noOv "ceiling"
  floor          = noOv "floor"

instance (RealFloat a) => RealFloat (Pattern a) where
  floatRadix     = noOv "floatRadix"
  floatDigits    = noOv "floatDigits"
  floatRange     = noOv "floatRange"
  decodeFloat    = noOv "decodeFloat"
  encodeFloat    = ((.).(.)) pure encodeFloat
  exponent       = noOv "exponent"
  significand    = noOv "significand"
  scaleFloat n   = fmap (scaleFloat n)
  isNaN          = noOv "isNaN"
  isInfinite     = noOv "isInfinite"
  isDenormalized = noOv "isDenormalized"
  isNegativeZero = noOv "isNegativeZero"
  isIEEE         = noOv "isIEEE"
  atan2          = liftA2 atan2

instance Num ValueMap where
  negate      = (applyFIS negate negate id <$>)
  (+)         = Map.unionWith (fNum2 (+) (+))
  (*)         = Map.unionWith (fNum2 (*) (*))
  fromInteger i = Map.singleton "n" $ VI (fromInteger i)
  signum      = (applyFIS signum signum id <$>)
  abs         = (applyFIS abs abs id <$>)

instance Fractional ValueMap where
  recip        = fmap (applyFIS recip id id)
  fromRational r = Map.singleton "speed" $ VF (fromRational r)

class Moddable a where
  gmod :: a -> a -> a

instance Moddable Double where
  gmod = mod'
instance Moddable Rational where
  gmod = mod'
instance Moddable Note where
  gmod (Note a) (Note b) = Note (mod' a b)
instance Moddable Int where
  gmod = mod
instance Moddable ValueMap where
  gmod = Map.unionWith (fNum2 mod mod')

instance Floating ValueMap
  where pi = noOv "pi"
        exp _ = noOv "exp"
        log _ = noOv "log"
        sin _ = noOv "sin"
        cos _ = noOv "cos"
        asin _ = noOv "asin"
        acos _ = noOv "acos"
        atan _ = noOv "atan"
        sinh _ = noOv "sinh"
        cosh _ = noOv "cosh"
        asinh _ = noOv "asinh"
        acosh _ = noOv "acosh"
        atanh _ = noOv "atanh"

------------------------------------------------------------------------
-- * Internal/fundamental functions

empty :: Pattern a
empty = Pattern {query = const [], tactus = Just 1, pureValue = Nothing}

silence :: Pattern a
silence = empty

nothing :: Pattern a
nothing = empty {tactus = Just 0}

queryArc :: Pattern a -> Arc -> [Event a]
queryArc p a = query p $ State a Map.empty

-- | Splits queries that span cycles. For example `query p (0.5, 1.5)` would be
-- turned into two queries, `(0.5,1)` and `(1,1.5)`, and the results
-- combined. Being able to assume queries don't span cycles often
-- makes transformations easier to specify.
splitQueries :: Pattern a -> Pattern a
splitQueries p = p {query = \st -> concatMap (\a -> query p st {arc = a}) $ arcCyclesZW (arc st)}

-- | Apply a function to the arcs/timespans (both whole and parts) of the result
withResultArc :: (Arc -> Arc) -> Pattern a -> Pattern a
withResultArc f pat = pat
  { query = map (\(Event c w p e) -> Event c (f <$> w) (f p) e) . query pat}

-- | Apply a function to the time (both start and end of the timespans
-- of both whole and parts) of the result
withResultTime :: (Time -> Time) -> Pattern a -> Pattern a
withResultTime f = withResultArc (\(Arc s e) -> Arc (f s) (f e))

withResultStart :: (Time -> Time) -> Pattern a -> Pattern a
withResultStart f pat = withResultArc (\(Arc s e) -> Arc (f s) (f s + (e-s))) pat

-- | Apply a function to the timespan of the query
withQueryArc :: (Arc -> Arc) -> Pattern a -> Pattern a
withQueryArc f pat = pat {query = query pat . (\(State a m) -> State (f a) m)}

-- | Apply a function to the time (both start and end) of the query
withQueryTime :: (Time -> Time) -> Pattern a -> Pattern a
withQueryTime f pat = withQueryArc (\(Arc s e) -> Arc (f s) (f e)) pat

withQueryStart :: (Time -> Time) -> Pattern a -> Pattern a
withQueryStart f pat = withQueryArc (\(Arc s e) -> Arc (f s) (f s + (e-s))) pat

-- | Apply a function to the control values of the query
withQueryControls :: (ValueMap -> ValueMap) -> Pattern a -> Pattern a
withQueryControls f pat = pat { query = query pat . (\(State a m) -> State a (f m))}

-- | @withEvent f p@ returns a new @Pattern@ with each event mapped over
-- function @f@.
withEvent :: (Event a -> Event b) -> Pattern a -> Pattern b
withEvent f p = p {query = map f . query p, pureValue = Nothing}

-- | @withEvent f p@ returns a new @Pattern@ with each value mapped over
-- function @f@.
withValue :: (a -> b) -> Pattern a -> Pattern b
withValue f pat = withEvent (fmap f) pat

-- | @withEvent f p@ returns a new @Pattern@ with f applied to the resulting list of events for each query
-- function @f@.
withEvents :: ([Event a] -> [Event b]) -> Pattern a -> Pattern b
withEvents f p = p {query = f . query p, pureValue = Nothing}

-- | @withPart f p@ returns a new @Pattern@ with function @f@ applied
-- to the part.
withPart :: (Arc -> Arc) -> Pattern a -> Pattern a
withPart f = withEvent (\(Event c w p v) -> Event c w (f p) v)

_extract :: (Value -> Maybe a) -> String -> ControlPattern -> Pattern a
_extract f name pat = filterJust $ withValue (Map.lookup name >=> f) pat

-- | Extract a pattern of integer values by from a control pattern, given the name of the control
extractI :: String -> ControlPattern -> Pattern Int
extractI = _extract getI

-- | Extract a pattern of floating point values by from a control pattern, given the name of the control
extractF :: String -> ControlPattern -> Pattern Double
extractF = _extract getF

-- | Extract a pattern of string values by from a control pattern, given the name of the control
extractS :: String -> ControlPattern -> Pattern String
extractS = _extract getS

-- | Extract a pattern of boolean values by from a control pattern, given the name of the control
extractB :: String -> ControlPattern -> Pattern Bool
extractB = _extract getB

-- | Extract a pattern of rational values by from a control pattern, given the name of the control
extractR :: String -> ControlPattern -> Pattern Rational
extractR = _extract getR

-- | Extract a pattern of note values by from a control pattern, given the name of the control
extractN :: String -> ControlPattern -> Pattern Note
extractN = _extract getN

compressArc :: Arc -> Pattern a -> Pattern a
compressArc (Arc s e) p | s > e = empty
                        | s > 1 || e > 1 = empty
                        | s < 0 || e < 0 = empty
                        | otherwise = s `rotR` _fastGap (1/(e-s)) p

compressArcTo :: Arc -> Pattern a -> Pattern a
compressArcTo (Arc s e) = compressArc (Arc (cyclePos s) (e - sam s))

focusArc :: Arc -> Pattern a -> Pattern a
focusArc (Arc s e) p = (cyclePos s) `rotR` (_fast (1/(e-s)) p)


{-| Speed up a pattern by the given time pattern.

For example, the following will play the sound pattern @"bd sn kurt"@ twice as
fast (i.e., so it repeats twice per cycle), and the vowel pattern three times
as fast:

> d1 $ sound (fast 2 "bd sn kurt")
>    # fast 3 (vowel "a e o")

The first parameter can be patterned to, for example, play the pattern at twice
the speed for the first half of each cycle and then four times the speed for the
second half:

> d1 $ fast "2 4" $ sound "bd sn kurt cp"
-}
fast :: Pattern Time -> Pattern a -> Pattern a
fast t pat = patternify' _fast t pat

{-| @fastSqueeze@ speeds up a pattern by a time pattern given as input,
  squeezing the resulting pattern inside one cycle and playing the original
  pattern at every repetition.

  To better understand how it works, compare it with 'fast':

  >>> print $ fast "1 2" $ s "bd sn"
  (0>½)|s: "bd"
  (½>¾)|s: "bd"
  (¾>1)|s: "sn"

  This will give @bd@ played in the first half cycle, and @bd sn@ in the second
  half. On the other hand, using fastSqueeze;

  >>> print $ fastSqueeze "1 2" $ s "bd sn"
  (0>¼)|s: "bd"
  (¼>½)|s: "sn"
  (½>⅝)|s: "bd"
  (⅝>¾)|s: "sn"
  (¾>⅞)|s: "bd"
  (⅞>1)|s: "sn"

  The original pattern will play in the first half, and two repetitions of the
  original pattern will play in the second half. That is, every repetition
  contains the whole pattern.

  If the time pattern has a single value, it becomes equivalent to 'fast':

  > d1 $ fastSqueeze 2 $ s "bd sn"
  > d1 $ fast 2 $ s "bd sn"
  > d1 $ s "[bd sn]*2"
-}
fastSqueeze :: Pattern Time -> Pattern a -> Pattern a
fastSqueeze = patternifySqueeze _fast

-- | An alias for @fast@
density :: Pattern Time -> Pattern a -> Pattern a
density = fast

_fast :: Time -> Pattern a -> Pattern a
_fast rate pat | rate == 0 = silence
               | rate < 0 = rev $ _fast (negate rate) pat
               | otherwise = keepTactus pat $ withResultTime (/ rate) $ withQueryTime (* rate) pat

{-| Slow down a pattern by the given time pattern.

  For example, the following will play the sound pattern @"bd sn kurt"@ twice as
  slow (i.e., so it repeats once every two cycles), and the vowel pattern three
  times as slow:

  > d1 $ sound (slow 2 "bd sn kurt")
  >    # slow 3 (vowel "a e o")
-}
slow :: Pattern Time -> Pattern a -> Pattern a
slow = patternify _slow
_slow :: Time -> Pattern a -> Pattern a
_slow 0 _ = silence
_slow r p = _fast (1/r) p

_fastGap :: Time -> Pattern a -> Pattern a
_fastGap 0 _ = empty
_fastGap r p = splitQueries $
  withResultArc (\(Arc s e) -> Arc (sam s + ((s - sam s)/r'))
                             (sam s + ((e - sam s)/r'))
                 ) $ p {query = f}
  where r' = max r 1
        -- zero width queries of the next sam should return zero in this case..
        f st@(State a _) | start a' == nextSam (start a) = []
                         | otherwise = query p st {arc = a'}
          where mungeQuery t = sam t + min 1 (r' * cyclePos t)
                a' = (\(Arc s e) -> Arc (mungeQuery s) (mungeQuery e)) a

{-| Shifts a pattern back in time by the given amount, expressed in cycles.

  This will skip to the fourth cycle:

  > do
  >   resetCycles
  >   d1 $ rotL 4 $ seqP
  >     [ (0, 12, sound "bd bd*2")
  >     , (4, 12, sound "hh*2 [sn cp] cp future*4")
  >     , (8, 12, sound (samples "arpy*8" (run 16)))
  >     ]

  Useful when building and testing out longer sequences.
-}
rotL :: Time -> Pattern a -> Pattern a
rotL t p = withResultTime (subtract t) $ withQueryTime (+ t) p

{-| Shifts a pattern forward in time by the given amount, expressed in cycles.
  Opposite of 'rotL'.
-}
rotR :: Time -> Pattern a -> Pattern a
rotR t = rotL (negate t)

{- | @rev p@ returns @p@ with the event positions in each cycle reversed (or
  mirrored).

  For example rev @"1 [~ 2] ~ 3"@ is equivalent to rev @"3 ~ [2 ~] 1"@.

  Note that @rev@ reverses on a cycle-by-cycle basis. This means that @rev (slow
  2 "1 2 3 4")@ would actually result in @(slow 2 "2 1 4 3")@. This is because the
  @slow 2@ makes the repeating pattern last two cycles, each of which is reversed
  independently.

  In practice rev is generally used with conditionals, for example with every:

  > d1 $ every 3 rev $ n "0 1 [~ 2] 3" # sound "arpy"

  or 'jux':

  > d1 $ jux rev $ n (iter 4 "0 1 [~ 2] 3") # sound "arpy"
-}
rev :: Pattern a -> Pattern a
rev p =
  keepMeta p $ splitQueries $ p {
    query = \st -> map makeWholeAbsolute $
      mapParts (mirrorArc (midCycle $ arc st)) $
      map makeWholeRelative
      (query p st
        {arc = mirrorArc (midCycle $ arc st) (arc st)
        })
    }
  where makeWholeRelative :: Event a -> Event a
        makeWholeRelative e@Event {whole = Nothing} = e
        makeWholeRelative (Event c (Just (Arc s e)) p'@(Arc s' e') v) =
          Event c (Just $ Arc (s'-s) (e-e')) p' v
        makeWholeAbsolute :: Event a -> Event a
        makeWholeAbsolute e@Event {whole = Nothing} = e
        makeWholeAbsolute (Event c (Just (Arc s e)) p'@(Arc s' e') v) =
          Event c (Just $ Arc (s'-e) (e'+s)) p' v
        midCycle :: Arc -> Time
        midCycle (Arc s _) = sam s + 0.5
        mapParts :: (Arc -> Arc) -> [Event a] -> [Event a]
        mapParts f es = (\(Event c w p' v) -> Event c w (f p') v) <$> es
        -- | Returns the `mirror image' of a 'Arc' around the given point in time
        mirrorArc :: Time -> Arc -> Arc
        mirrorArc mid' (Arc s e) = Arc (mid' - (e-mid')) (mid'+(mid'-s))

-- | Mark values in the first pattern which match with at least one
-- value in the second pattern.
matchManyToOne :: (b -> a -> Bool) -> Pattern a -> Pattern b -> Pattern (Bool, b)
matchManyToOne f pa pb = pa {query = q, pureValue = Nothing}
  where q st = map match $ query pb st
          where
            match ex@(Event xContext xWhole xPart x) =
              Event (combineContexts $ xContext:map context as') xWhole xPart (any (f x . value) as', x)
                where as' = as $ start $ wholeOrPart ex
            as s = query pa $ fQuery s
            fQuery s = st {arc = Arc s s}

-- ** Event filters

-- | Remove events from patterns that to not meet the given test
filterValues :: (a -> Bool) -> Pattern a -> Pattern a
filterValues f p = p {query = filter (f . value) . query p}

-- | Turns a pattern of 'Maybe' values into a pattern of values,
-- dropping the events of 'Nothing'.
filterJust :: Pattern (Maybe a) -> Pattern a
filterJust p = fromJust <$> filterValues isJust p

filterWhen :: (Time -> Bool) -> Pattern a -> Pattern a
filterWhen test p = p {query = filter (test . wholeStart) . query p}

filterOnsets :: Pattern a -> Pattern a
filterOnsets p = p {query = filter (\e -> eventPartStart e == wholeStart e) . query (filterDigital p)}

filterEvents :: (Event a -> Bool) -> Pattern a -> Pattern a
filterEvents f p = p {query = filter f . query p}

filterDigital :: Pattern a -> Pattern a
filterDigital = filterEvents isDigital

filterAnalog :: Pattern a -> Pattern a
filterAnalog = filterEvents isAnalog

playFor :: Time -> Time -> Pattern a -> Pattern a
playFor s e pat = pattern $ \st -> maybe [] (\a -> query pat (st {arc = a})) $ subArc (Arc s e) (arc st)

-- | Splits a pattern into a list containing the given 'n' number of
-- patterns. Each one plays every 'n'th cycle, successfully offset by
-- a cycle.
separateCycles :: Int -> Pattern a -> [Pattern a]
separateCycles n pat = map (\i -> skip $ rotL (toRational i) pat) [0 .. n-1]
  where n' = toRational n
        skip pat' = splitQueries $ withResultStart (\t -> ((sam t) / n') + cyclePos t) $ withQueryStart (\t -> (sam t * n') + cyclePos t) $ pat'

-- ** Temporal parameter helpers

patternify :: (t1 -> t2 -> Pattern a) -> Pattern t1 -> t2 -> Pattern a
patternify f (Pattern _ _ (Just a)) b = f a b
patternify f pa p                     = innerJoin $ (`f` p) <$> pa

-- versions that preserve the tactus
patternify' ::(b -> Pattern c -> Pattern a) -> Pattern b -> Pattern c -> Pattern a
patternify' f pa p = (patternify f pa p) {tactus = tactus p}

patternify2 :: (a -> b -> c -> Pattern d) -> Pattern a -> Pattern b -> c -> Pattern d
patternify2 f (Pattern _ _ (Just a)) (Pattern _ _ (Just b)) c = f a b c
patternify2 f a b p = innerJoin $ (\x y -> f x y p) <$> a <*> b

patternify2' :: (a -> b -> Pattern c -> Pattern d) -> Pattern a -> Pattern b -> Pattern c -> Pattern d
patternify2' f a b p = patternify2 f a b p

patternify3 :: (a -> b -> c -> Pattern d -> Pattern e) -> (Pattern a -> Pattern b -> Pattern c -> Pattern d -> Pattern e)
patternify3 f (Pattern _ _ (Just a)) (Pattern _ _ (Just b)) (Pattern _ _ (Just c)) d = f a b c d
patternify3 f a b c p = innerJoin $ (\x y z -> f x y z p) <$> a <*> b <*> c

patternify3' :: (a -> b -> c -> Pattern d -> Pattern e) -> (Pattern a -> Pattern b -> Pattern c -> Pattern d -> Pattern e)
patternify3' f a b c p = keepTactus p $ patternify3 f a b c p

patternifySqueeze :: (a -> Pattern b -> Pattern c) -> (Pattern a -> Pattern b -> Pattern c)
patternifySqueeze f tv p = squeezeJoin $ (`f` p) <$> tv

-- ** Context

combineContexts :: [Context] -> Context
combineContexts = Context . concatMap contextPosition

setContext :: Context -> Pattern a -> Pattern a
setContext c pat = keepMeta pat $ withEvents (map (\e -> e {context = c})) pat

withContext :: (Context -> Context) -> Pattern a -> Pattern a
withContext f pat = keepMeta pat $ withEvents (map (\e -> e {context = f $ context e})) pat

-- A hack to add to manipulate source code to add calls to
-- 'deltaContext' around strings, so events from mininotation know
-- where they are within a whole tidal pattern
deltaMini :: String -> String
deltaMini = outside 0 0
  where outside :: Int -> Int -> String -> String
        outside _ _ [] = []
        outside column line ('"':xs) = "(deltaContext "
                                         ++ show column
                                         ++ " "
                                         ++ show line
                                         ++ " \""
                                         ++ inside (column+1) line xs
        outside _ line ('\n':xs) = '\n':outside 0 (line+1) xs
        outside column line (x:xs) = x:outside (column+1) line xs
        inside :: Int -> Int -> String -> String
        inside _ _ []               = []
        inside column line ('"':xs) = '"':')':outside (column+1) line xs
        inside _ line ('\n':xs)     = '\n':inside 0 (line+1) xs
        inside column line (x:xs)   = x:inside (column+1) line xs

class Stringy a where
  deltaContext :: Int -> Int -> a -> a

instance Stringy (Pattern a) where
  deltaContext column line pat = withEvents (map (\e -> e {context = f $ context e})) pat
    where f :: Context -> Context
          f (Context xs) = Context $ map (\((bx,by), (ex,ey)) -> ((bx+column,by+line), (ex+column,ey+line))) xs

-- deltaContext on an actual (non overloaded) string is a no-op
instance Stringy String where
  deltaContext _ _ = id

-- ** Events

-- | Some context for an event, currently just position within sourcecode
data Context = Context {contextPosition :: [((Int, Int), (Int, Int))]}
  deriving (Eq, Ord, Generic)
instance NFData Context

-- | An event is a value that's active during a timespan. If a whole
-- is present, the part should be equal to or fit inside it.
data EventF a b = Event
  { context :: Context
  , whole   :: Maybe a
  , part    :: a
  , value   :: b
  } deriving (Eq, Ord, Functor, Generic)
instance (NFData a, NFData b) => NFData (EventF a b)

type Event a = EventF (ArcF Time) a

-- * Event utilities

isAnalog :: Event a -> Bool
isAnalog (Event {whole = Nothing}) = True
isAnalog _                         = False

isDigital :: Event a -> Bool
isDigital = not . isAnalog

-- | `True` if an `Event`'s starts is within given `Arc`
onsetIn :: Arc -> Event a -> Bool
onsetIn a e = isIn a (wholeStart e)

-- | Returns a list of events, with any adjacent parts of the same whole combined
defragParts :: Eq a => [Event a] -> [Event a]
defragParts [] = []
defragParts [e] = [e]
defragParts (e:es) | isJust i = defraged : defragParts (delete e' es)
                   | otherwise = e : defragParts es
  where i = findIndex (isAdjacent e) es
        e' = es !! fromJust i
        defraged = Event (context e) (whole e) u (value e)
        u = hull (part e) (part e')

-- | Returns 'True' if the two given events are adjacent parts of the same whole
isAdjacent :: Eq a => Event a -> Event a -> Bool
isAdjacent e e' = (whole e == whole e')
                  && (value e == value e')
                  && ((stop (part e) == start (part e'))
                      ||
                      (stop (part e') == start (part e))
                     )

wholeOrPart :: Event a -> Arc
wholeOrPart (Event {whole = Just a}) = a
wholeOrPart e                        = part e

-- | Get the onset of an event's 'whole'
wholeStart :: Event a -> Time
wholeStart = start . wholeOrPart

-- | Get the offset of an event's 'whole'
wholeStop :: Event a -> Time
wholeStop = stop . wholeOrPart

-- | Get the onset of an event's 'whole'
eventPartStart :: Event a -> Time
eventPartStart = start . part

-- | Get the offset of an event's 'part'
eventPartStop :: Event a -> Time
eventPartStop = stop . part

-- | Get the timespan of an event's 'part'
eventPart :: Event a -> Arc
eventPart = part

eventValue :: Event a -> a
eventValue = value

eventHasOnset :: Event a -> Bool
eventHasOnset e | isAnalog e = False
                | otherwise = start (fromJust $ whole e) == start (part e)

-- TODO - Is this used anywhere? Just tests, it seems
-- TODO - support 'context' field
toEvent :: (((Time, Time), (Time, Time)), a) -> Event a
toEvent (((ws, we), (ps, pe)), v) = Event (Context []) (Just $ Arc ws we) (Arc ps pe) v

 -- Resolves higher order VState values to plain values, by passing through (and changing) state
resolveState :: ValueMap -> [Event ValueMap] -> (ValueMap, [Event ValueMap])
resolveState sMap [] = (sMap, [])
resolveState sMap (e:es) = (sMap'', (e {value = v'}):es')
  where f sm (VState v) = v sm
        f sm v          = (sm, v)
        (sMap', v') | eventHasOnset e = Map.mapAccum f sMap (value e)    -- pass state through VState functions
                    | otherwise = (sMap, Map.filter notVState $ value e) -- filter out VState values without onsets
        (sMap'', es') = resolveState sMap' es
        notVState (VState _) = False
        notVState _          = True

-- ** Values

-- | Polymorphic values

data Value = VS { svalue :: String   }
           | VF { fvalue :: Double   }
           | VN { nvalue :: Note     }
           | VR { rvalue :: Rational }
           | VI { ivalue :: Int      }
           | VB { bvalue :: Bool     }
           | VX { xvalue :: [Word8]  } -- Used for OSC 'blobs'
           | VPattern {pvalue :: Pattern Value}
           | VList {lvalue :: [Value]}
           | VState {statevalue :: ValueMap -> (ValueMap, Value)}
           deriving (Typeable, Generic)

class Valuable a where
  toValue :: a -> Value
instance NFData Value

type ValueMap = Map.Map String Value

-- | Note is Double, but with a different parser
newtype Note = Note { unNote :: Double }
  deriving (Typeable, Data, Generic, Eq, Ord, Enum, Num, Fractional, Floating, Real, RealFrac)

instance NFData Note

instance Show Note where
  show n = (show . unNote $ n) ++ "n (" ++ pitchClass ++ octave ++ ")"
    where
      pitchClass = pcs !! mod noteInt 12
      octave = show $ div noteInt 12 + 5
      noteInt = round . unNote $ n
      pcs = ["c", "cs", "d", "ds", "e", "f", "fs", "g", "gs", "a", "as", "b"]

instance Valuable String where
  toValue a = VS a
instance Valuable Double where
  toValue a = VF a
instance Valuable Rational where
  toValue a = VR a
instance Valuable Int where
  toValue a = VI a
instance Valuable Bool where
  toValue a = VB a
instance Valuable Note where
  toValue a = VN a
instance Valuable [Word8] where
  toValue a = VX a
instance Valuable [Value] where
  toValue a = VList a

instance Eq Value where
  (VS x) == (VS y) = x == y
  (VB x) == (VB y) = x == y
  (VF x) == (VF y) = x == y
  (VI x) == (VI y) = x == y
  (VN x) == (VN y) = x == y
  (VR x) == (VR y) = x == y
  (VX x) == (VX y) = x == y

  (VF x) == (VI y) = x == fromIntegral y
  (VI y) == (VF x) = x == fromIntegral y

  (VF x) == (VR y) = toRational x == y
  (VR y) == (VF x) = toRational x == y
  (VI x) == (VR y) = toRational x == y
  (VR y) == (VI x) = toRational x == y

  _ == _           = False

instance Ord Value where
  compare (VS x) (VS y)             = compare x y
  compare (VB x) (VB y)             = compare x y
  compare (VF x) (VF y)             = compare x y
  compare (VN x) (VN y)             = compare (unNote x) (unNote y)
  compare (VI x) (VI y)             = compare x y
  compare (VR x) (VR y)             = compare x y
  compare (VX x) (VX y)             = compare x y

  compare (VS _) _                  = LT
  compare _ (VS _)                  = GT
  compare (VB _) _                  = LT
  compare _ (VB _)                  = GT
  compare (VX _) _                  = LT
  compare _ (VX _)                  = GT

  compare (VF x) (VI y)             = compare x (fromIntegral y)
  compare (VI x) (VF y)             = compare (fromIntegral x) y

  compare (VR x) (VI y)             = compare x (fromIntegral y)
  compare (VI x) (VR y)             = compare (fromIntegral x) y

  compare (VF x) (VR y)             = compare x (fromRational y)
  compare (VR x) (VF y)             = compare (fromRational x) y

  compare (VN x) (VI y)             = compare x (fromIntegral y)
  compare (VI x) (VN y)             = compare (fromIntegral x) y

  compare (VN x) (VR y)             = compare (unNote x) (fromRational y)
  compare (VR x) (VN y)             = compare (fromRational x) (unNote y)

  compare (VF x) (VN y)             = compare x (unNote y)
  compare (VN x) (VF y)             = compare (unNote x) y

  -- you can't really compare patterns, state or lists..
  compare (VPattern _) (VPattern _) = EQ
  compare (VPattern _) _            = GT
  compare _ (VPattern _)            = LT

  compare (VState _) (VState _)     = EQ
  compare (VState _) _              = GT
  compare _ (VState _)              = LT

  compare (VList _) (VList _)       = EQ
  compare (VList _) _               = GT
  compare _ (VList _)               = LT

-- | General utilities..

-- | Apply one of three functions to a Value, depending on its type
applyFIS :: (Double -> Double) -> (Int -> Int) -> (String -> String) -> Value -> Value
applyFIS f _ _ (VF f') = VF (f f')
applyFIS f _ _ (VN (Note f')) = VN (Note $ f f')
applyFIS _ f _ (VI i) = VI (f i)
applyFIS _ _ f (VS s) = VS (f s)
applyFIS f f' f'' (VState x) = VState $ \cmap -> (applyFIS f f' f'') <$> (x cmap)
applyFIS _ _ _ v = v

-- | Apply one of two functions to a pair of Values, depending on their types (int
-- or float; strings and rationals are ignored)
fNum2 :: (Int -> Int -> Int) -> (Double -> Double -> Double) -> Value -> Value -> Value
fNum2 fInt _      (VI a) (VI b) = VI (fInt a b)
fNum2 _    fFloat (VF a) (VF b) = VF (fFloat a b)
fNum2 _    fFloat (VN (Note a)) (VN (Note b)) = VN (Note $ fFloat a b)
fNum2 _    fFloat (VF a) (VN (Note b)) = VN (Note $ fFloat a b)
fNum2 _    fFloat (VN (Note a)) (VF b) = VN (Note $ fFloat a b)
fNum2 _    fFloat (VI a) (VF b) = VF (fFloat (fromIntegral a) b)
fNum2 _    fFloat (VF a) (VI b) = VF (fFloat a (fromIntegral b))
fNum2 fInt fFloat (VState a) b = VState $ \cmap -> ((\a' -> fNum2 fInt fFloat a' b) <$> (a cmap))
fNum2 fInt fFloat a (VState b) = VState $ \cmap -> ((\b' -> fNum2 fInt fFloat a b') <$> (b cmap))
fNum2 _    _      x      _      = x

getI :: Value -> Maybe Int
getI (VI i) = Just i
getI (VR x) = Just $ floor x
getI (VF x) = Just $ floor x
getI _      = Nothing

getF :: Value -> Maybe Double
getF (VF f) = Just f
getF (VR x) = Just $ fromRational x
getF (VI x) = Just $ fromIntegral x
getF _      = Nothing

getN :: Value -> Maybe Note
getN (VN n) = Just n
getN (VF f) = Just $ Note f
getN (VR x) = Just $ Note $ fromRational x
getN (VI x) = Just $ Note $ fromIntegral x
getN _      = Nothing

getS :: Value -> Maybe String
getS (VS s) = Just s
getS _      = Nothing

getB :: Value -> Maybe Bool
getB (VB b) = Just b
getB _      = Nothing

getR :: Value -> Maybe Rational
getR (VR r) = Just r
getR (VF x) = Just $ toRational x
getR (VI x) = Just $ toRational x
getR _      = Nothing

getBlob :: Value -> Maybe [Word8]
getBlob (VX xs) = Just xs
getBlob _       = Nothing

getList :: Value -> Maybe [Value]
getList (VList vs) = Just vs
getList _          = Nothing

valueToPattern :: Value -> Pattern Value
valueToPattern (VPattern pat) = pat
valueToPattern v              = pure v

--- functions relating to chords/patterns of lists


sameDur :: Event a -> Event a -> Bool
sameDur e1 e2 = (whole e1 == whole e2) && (part e1 == part e2)

groupEventsBy :: Eq a => (Event a -> Event a -> Bool) -> [Event a] -> [[Event a]]
groupEventsBy _ [] = []
groupEventsBy f (e:es) = eqs:(groupEventsBy f (es \\ eqs))
                   where eqs = e:[x | x <- es, f e x]

-- assumes that all events in the list have same whole/part
collectEvent :: [Event a] -> Maybe (Event [a])
collectEvent [] = Nothing
collectEvent l@(e:_) = Just $ e {context = con, value = vs}
                      where con = unionC $ map context l
                            vs = map value l
                            unionC [] = Context []
                            unionC ((Context is):cs) = Context (is ++ iss)
                                                 where Context iss = unionC cs

collectEventsBy :: Eq a => (Event a -> Event a -> Bool) -> [Event a] -> [Event [a]]
collectEventsBy f es = remNo $ map collectEvent (groupEventsBy f es)
                     where
                     remNo []            = []
                     remNo (Nothing:cs)  = remNo cs
                     remNo ((Just c):cs) = c : (remNo cs)

-- | collects all events satisfying the same constraint into a list
collectBy :: Eq a => (Event a -> Event a -> Bool) -> Pattern a -> Pattern [a]
collectBy f = withEvents (collectEventsBy f)

-- | collects all events occuring at the exact same time into a list
collect :: Eq a => Pattern a -> Pattern [a]
collect = collectBy sameDur

uncollectEvent :: Event [a] -> [Event a]
uncollectEvent e = [e {value = (value e)!!i, context = resolveContext i (context e)} | i <-[0..length (value e) - 1]]
               where resolveContext i (Context xs) = case length xs <= i of
                                                                  True -> Context []
                                                                  False -> Context [xs!!i]

uncollectEvents :: [Event [a]] -> [Event a]
uncollectEvents = concatMap uncollectEvent

-- | merges all values in a list into one pattern by stacking the values
uncollect :: Pattern [a] -> Pattern a
uncollect = withEvents uncollectEvents

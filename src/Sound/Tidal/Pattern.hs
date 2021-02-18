{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
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
                            module Sound.Tidal.Time,
                            module Sound.Tidal.Value
                           )
where

import           Prelude hiding ((<*), (*>))

import           Control.Applicative (liftA2)
import           GHC.Generics
import           Control.DeepSeq (NFData)
import           Control.Monad ((>=>))
import qualified Data.Map.Strict as Map
import           Data.Maybe (isJust, fromJust, catMaybes, mapMaybe)

import           Sound.Tidal.Time
import           Sound.Tidal.Value

------------------------------------------------------------------------
-- * Types

-- | an Arc and some named control values
data State = State {arc :: Arc,
                    controls :: StateMap
                   }

-- | A datatype representing events taking place over time
data Pattern a = Pattern {query :: State -> [Event a]}
  deriving Generic
instance NFData a => NFData (Pattern a)

type StateMap = Map.Map String (Pattern Value)
type ControlMap = Map.Map String Value
type ControlPattern = Pattern ControlMap

instance TolerantEq ControlMap where
  a ~== b = Map.differenceWith (\a' b' -> if a' ~== b' then Nothing else Just a') a b == Map.empty

instance TolerantEq (Event ControlMap) where
  (Event _ w p x) ~== (Event _ w' p' x') = w == w' && p == p' && x ~== x'

-- * Functor

instance Functor Pattern where
  -- | apply a function to all the values in a pattern
  fmap f p = p {query = fmap (fmap f) . query p}

-- * Applicative and friends

instance Applicative Pattern where
  -- | Repeat the given value once per cycle, forever
  pure v = Pattern $ \(State a _) ->
    map (\a' -> Event (Context []) (Just a') (sect a a') v) $ cycleArcsInArc a

  (<*>) = applyPatToPatBoth

-- | Like <*>, but the 'wholes' come from the left
(<*) :: Pattern (a -> b) -> Pattern a -> Pattern b
(<*) = applyPatToPatLeft

-- | Like <*>, but the 'wholes' come from the right
(*>) :: Pattern (a -> b) -> Pattern a -> Pattern b
(*>) = applyPatToPatRight

infixl 4 <*, *>
applyPatToPat :: (Maybe Arc -> Maybe Arc -> Maybe (Maybe Arc)) -> Pattern (a -> b) -> Pattern a -> Pattern b
applyPatToPat combineWholes pf px = Pattern q
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
applyPatToPatBoth pf px = Pattern q
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
applyPatToPatLeft pf px = Pattern q
    where q st = catMaybes $ concatMap match $ query pf st
            where
              match ef = map (withFX ef) (query px $ st {arc = wholeOrPart ef})
              withFX ef ex = do let whole' = whole ef
                                part' <- subArc (part ef) (part ex)
                                return (Event (combineContexts [context ef, context ex]) whole' part' (value ef $ value ex))

applyPatToPatRight :: Pattern (a -> b) -> Pattern a -> Pattern b
applyPatToPatRight pf px = Pattern q
    where q st = catMaybes $ concatMap match $ query px st
            where
              match ex = map (`withFX` ex) (query pf $ st {arc = wholeOrPart ex})
              withFX ef ex = do let whole' = whole ex
                                part' <- subArc (part ef) (part ex)
                                return (Event (combineContexts [context ef, context ex]) whole' part' (value ef $ value ex))

-- * Monad and friends

-- Note there are four ways of joining - the default 'unwrap' used by @>>=@, as well
-- as innerJoin, outerJoin and squeezeJoin.

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
unwrap pp = pp {query = q}
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
innerJoin pp = pp {query = q}
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
outerJoin pp = pp {query = q}
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
squeezeJoin pp = pp {query = q}
  where q st = concatMap
          (\e@(Event c w p v) ->
             mapMaybe (munge c w p) $ query (compressArc (cycleArc $ wholeOrPart e) v) st {arc = p}
          )
          (query pp st)
        munge oContext oWhole oPart (Event iContext iWhole iPart v) =
          do w' <- subMaybeArc oWhole iWhole
             p' <- subArc oPart iPart
             return (Event (combineContexts [iContext, oContext]) w' p' v)

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

instance Num ControlMap where
  negate      = (applyFIS negate negate id <$>)
  (+)         = Map.unionWith (fNum2 (+) (+))
  (*)         = Map.unionWith (fNum2 (*) (*))
  fromInteger i = Map.singleton "n" $ VI (fromInteger i)
  signum      = (applyFIS signum signum id <$>)
  abs         = (applyFIS abs abs id <$>)

instance Fractional ControlMap where
  recip        = fmap (applyFIS recip id id)
  fromRational r = Map.singleton "speed" $ VF (fromRational r)

------------------------------------------------------------------------
-- * Internal functions

empty :: Pattern a
empty = Pattern {query = const []}

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

-- | Apply a function to the timespan of the query
withQueryArc :: (Arc -> Arc) -> Pattern a -> Pattern a
withQueryArc f p = p {query = query p . (\(State a m) -> State (f a) m)}

-- | Apply a function to the time (both start and end) of the query
withQueryTime :: (Time -> Time) -> Pattern a -> Pattern a
withQueryTime f = withQueryArc (\(Arc s e) -> Arc (f s) (f e))

-- | @withEvent f p@ returns a new @Pattern@ with each event mapped over
-- function @f@.
withEvent :: (Event a -> Event b) -> Pattern a -> Pattern b
withEvent f p = p {query = map f . query p}

-- | @withEvent f p@ returns a new @Pattern@ with each value mapped over
-- function @f@.
withValue :: (a -> b) -> Pattern a -> Pattern b
withValue f pat = withEvent (fmap f) pat

-- | @withEvent f p@ returns a new @Pattern@ with f applied to the resulting list of events for each query
-- function @f@.
withEvents :: ([Event a] -> [Event b]) -> Pattern a -> Pattern b
withEvents f p = p {query = f . query p}

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

compressArc :: Arc -> Pattern a -> Pattern a
compressArc (Arc s e) p | s > e = empty
                        | s > 1 || e > 1 = empty
                        | s < 0 || e < 0 = empty
                        | otherwise = s `rotR` _fastGap (1/(e-s)) p

compressArcTo :: Arc -> Pattern a -> Pattern a
compressArcTo (Arc s e) = compressArc (Arc (cyclePos s) (e - sam s))

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

-- | Shifts a pattern back in time by the given amount, expressed in cycles
rotL :: Time -> Pattern a -> Pattern a
rotL t p = withResultTime (subtract t) $ withQueryTime (+ t) p

-- | Shifts a pattern forward in time by the given amount, expressed in cycles
rotR :: Time -> Pattern a -> Pattern a
rotR t = rotL (negate t)


-- | Mark values in the first pattern which match with at least one
-- value in the second pattern.
matchManyToOne :: (b -> a -> Bool) -> Pattern a -> Pattern b -> Pattern (Bool, b)
matchManyToOne f pa pb = pa {query = q}
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
playFor s e = filterWhen (\t -> (t >= s) && (t < e))

-- ** Temporal parameter helpers

tParam :: (t1 -> t2 -> Pattern a) -> Pattern t1 -> t2 -> Pattern a
tParam f tv p = innerJoin $ (`f` p) <$> tv

tParam2 :: (a -> b -> c -> Pattern d) -> Pattern a -> Pattern b -> c -> Pattern d
tParam2 f a b p = innerJoin $ (\x y -> f x y p) <$> a <*> b

tParam3 :: (a -> b -> c -> Pattern d -> Pattern e) -> (Pattern a -> Pattern b -> Pattern c -> Pattern d -> Pattern e)
tParam3 f a b c p = innerJoin $ (\x y z -> f x y z p) <$> a <*> b <*> c

tParamSqueeze :: (a -> Pattern b -> Pattern c) -> (Pattern a -> Pattern b -> Pattern c)
tParamSqueeze f tv p = squeezeJoin $ (`f` p) <$> tv

-- ** Context

combineContexts :: [Context] -> Context
combineContexts = Context . concatMap contextPosition

setContext :: Context -> Pattern a -> Pattern a
setContext c pat = withEvents (map (\e -> e {context = c})) pat

withContext :: (Context -> Context) -> Pattern a -> Pattern a
withContext f pat = withEvents (map (\e -> e {context = f $ context e})) pat

deltaContext :: Int -> Int -> Pattern a -> Pattern a
deltaContext column line pat = withEvents (map (\e -> e {context = f $ context e})) pat
  where f :: Context -> Context
        f (Context xs) = Context $ map (\((bx,by), (ex,ey)) -> ((bx+column,by+line), (ex+column,ey+line))) xs

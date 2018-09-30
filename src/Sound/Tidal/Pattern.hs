{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sound.Tidal.Pattern where

import Prelude hiding ((<*), (*>))
import qualified Data.Map.Strict as Map
import Data.Fixed (mod')
import Data.Maybe (isJust, fromJust, catMaybes, fromMaybe)
import Data.Ratio (numerator, denominator, (%))
import Control.Applicative (liftA2)
import Data.List (delete, findIndex, sort, intercalate, findIndices)
import Data.Typeable (Typeable)
import Data.Data (Data) -- toConstr
import System.Random.Mersenne.Pure64 (randomDouble, pureMT)

import Sound.Tidal.Utils

------------------------------------------------------------------------
-- * Types

-- | Time is rational
type Time = Rational

-- | A time arc (start and end)
type Arc = (Time, Time)

-- | The second arc (the part) should be equal to or fit inside the
-- first one (the whole that it's a part of).
type Part = (Arc, Arc)

-- | An event is a value that's active during a timespan
type Event a = (Part, a)

data State = State {arc :: Arc,
                    controls :: ControlMap
                   }

-- | A function that represents events taking place over time
type Query a = (State -> [Event a])

-- | Also known as Continuous vs Discrete/Amorphous vs Pulsating etc.
data Nature = Analog | Digital
            deriving Eq

-- | A datatype that's basically a query, plus a hint about whether its events
-- are Analogue or Digital by nature
data Pattern a = Pattern {nature :: Nature, query :: Query a}

data Value = VS { svalue :: String }
           | VF { fvalue :: Float }
           | VI { ivalue :: Int }
           deriving (Eq,Ord,Typeable,Data)

type ControlMap = Map.Map String Value
type ControlPattern = Pattern ControlMap

------------------------------------------------------------------------
-- * Instances

instance Functor Pattern where
  -- | apply a function to all the values in a pattern
  fmap f p = p {query = (fmap (fmap f)) . query p}

instance Applicative Pattern where
  -- | Repeat the given value once per cycle, forever
  pure v = Pattern Digital $ \(State (s,e) _) -> map (\(s',e') -> (constrain (s,e) (s',e'),v)) $ cycleArcsInArc (s,e)
    where constrain (s,e) (s',e') = ((s',e'), (max s s', min e e'))

  (<*>) pf@(Pattern Digital _) px@(Pattern Digital _) = Pattern Digital q
    where q st = catMaybes $ concat $ map match $ query pf st
            where
              match ((fWhole, fPart), f) =
                map
                (\((xWhole, xPart),x) ->
                  do whole' <- subArc xWhole fWhole
                     part' <- subArc fPart xPart
                     return ((whole', part'), f x)
                )
                (query px $ st {arc = fPart})
  (<*>) pf@(Pattern Digital _) px@(Pattern Analog _) = Pattern Digital q
    where q st = concatMap match $ query pf st
            where
              match ((fWhole, fPart), f) =
                map
                (\(_ ,x) -> ((fWhole, fPart), f x))
                (query px $ st {arc = (fst fPart, fst fPart)})

  (<*>) pf@(Pattern Analog _) px@(Pattern Digital _) = Pattern Digital q
    where q st = concatMap match $ query px st
            where
              match ((xWhole, xPart), x) =
                map
                (\(_ ,f) -> ((xWhole, xPart), f x))
                (query pf st {arc = (fst xPart, fst xPart)})
                
  (<*>) pf px = Pattern Analog q
    where q st = concatMap match $ query pf st
            where
              match (_, f) =
                map
                (\(_ ,x) -> ((arc st, arc st), f x))
                (query px st)

-- | Like <*>, but the structure only comes from the left
(<*) :: Pattern (a -> b) -> Pattern a -> Pattern b
(<*) pf@(Pattern Digital _) px = Pattern Digital q
  where q st = concatMap match $ query pf st
         where
            match ((fWhole, fPart), f) =
              map
              (\(_, x) -> ((fWhole, fPart), f x)) $
              query px $ st {arc = xQuery fWhole}
            xQuery ((s,_)) = (s,s) -- for discrete events, match with the onset
            

pf <* px = Pattern Analog q
  where q st = concatMap match $ query pf st
          where
            match ((fWhole, fPart), f) =
              map
              (\(_, x) -> ((fWhole, fPart), f x)) $
              query px st -- for continuous events, use the original query

-- | Like <*>, but the structure only comes from the right
(*>) :: Pattern (a -> b) -> Pattern a -> Pattern b
(*>) pf px@(Pattern Digital _) = Pattern Digital q
  where q st = concatMap match $ query px st
         where
            match ((xWhole, xPart), x) =
              map
              (\(_, f) -> ((xWhole, xPart), f x)) $
              query pf $ fQuery xWhole
            fQuery ((s,_)) = st {arc = (s,s)} -- for discrete events, match with the onset
            

pf *> px = Pattern Analog q
  where q st = concatMap match $ query px st
          where
            match ((xWhole, xPart), x) =
              map
              (\(_, f) -> ((xWhole, xPart), f x)) $
              query pf st -- for continuous events, use the original query

infixl 4 <*, *>

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
  where q st = concatMap (\((whole, part), p) -> catMaybes $ map (munge whole part) $ query p st {arc = part}) (query pp st)
        munge oWhole oPart ((iWhole, iPart),v) = do w <- subArc oWhole iWhole
                                                    p <- subArc oPart iPart
                                                    return ((w,p),v)

-- | Turns a pattern of patterns into a single pattern. Like @unwrap@,
-- but structure only comes from the inner pattern.
innerJoin :: Pattern (Pattern a) -> Pattern a
innerJoin pp = pp {query = q}
  where q st = concatMap (\((_, part), p) -> catMaybes $ map munge $ query p st {arc = part}) (query pp st)
          where munge ((iWhole, iPart),v) = do let w = iWhole
                                               p <- subArc (arc st) iPart
                                               p' <- subArc p (arc st)
                                               return ((w,p'),v)

-- | Turns a pattern of patterns into a single pattern. Like @unwrap@,
-- but structure only comes from the outer pattern.
outerJoin :: Pattern (Pattern a) -> Pattern a
outerJoin pp = pp {query = q}
  where q st = concatMap (\((whole, part), p) -> catMaybes $ map (munge whole part) $ query p st {arc = (fst whole, fst whole)}) (query pp st)
          where munge oWhole oPart (_,v) = do let w = oWhole
                                              p <- subArc (arc st) oPart
                                              return ((w,p),v)


-- | Like @unwrap@, but cycles of the inner patterns are compressed to fit the
-- timespan of the outer whole (or the original query if it's a continuous pattern?)
-- TODO - what if a continuous pattern contains a discrete one, or vice-versa?
unwrapSqueeze :: Pattern (Pattern a) -> Pattern a
unwrapSqueeze pp = pp {query = q}
  where q st = concatMap (\((whole, part), p) -> catMaybes $ map (munge whole part) $ query (compress whole p) st {arc = part}) (query pp st)
        munge oWhole oPart ((iWhole, iPart),v) = do whole' <- subArc oWhole iWhole
                                                    part' <- subArc oPart iPart
                                                    return ((whole',part'),v)

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

instance Num (ControlMap) where
  negate      = ((applyFIS negate negate id) <$>)
  (+)         = Map.unionWith (fNum2 (+) (+))
  (*)         = Map.unionWith (fNum2 (*) (*))
  fromInteger i = Map.singleton "n" $ VI $ fromInteger i
  signum      = ((applyFIS signum signum id) <$>)
  abs         = ((applyFIS abs abs id) <$>)

instance Fractional ControlMap where
  recip        = fmap (applyFIS recip id id)
  fromRational = Map.singleton "speed" . VF . fromRational


instance {-# OVERLAPPING #-} Show Arc where
  show (s,e) = prettyRat s ++ ">" ++ prettyRat e

instance {-# OVERLAPPING #-} Show Part where
  show ((s,e),(s',e')) = h ++ "(" ++ show (s',e') ++ ")" ++ t
    where h | s == s' = ""
            | otherwise = prettyRat s ++ "-"
          t | e == e' = ""
            | otherwise = "-" ++ prettyRat e

instance {-# OVERLAPPING #-} Show a => Show (Event a) where
  show (p,v) = show p ++ "|" ++ show v

showPattern :: Show a => Arc -> Pattern a -> String
showPattern a p = intercalate "\n" $ map show $ queryArc p a

instance (Show a) => Show (Pattern a) where
  show p = showPattern (0,1) p

instance Show Value where
  show (VS s) = ('"':s) ++ "\""
  show (VI i) = show i
  show (VF f) = show f ++ "f"

instance {-# OVERLAPPING #-} Show (ControlMap) where
  show m = intercalate ", " $ map (\(name, value) -> name ++ ": " ++ show value) $ Map.toList m

prettyRat :: Rational -> String
prettyRat r | unit == 0 && frac > 0 = showFrac (numerator frac) (denominator frac)
            | otherwise =  show unit ++ showFrac (numerator frac) (denominator frac)
  where unit = floor r :: Int
        frac = (r - (toRational unit))

showFrac :: Integer -> Integer -> String
showFrac 0 _ = ""
showFrac 1 2 = "½"
showFrac 1 3 = "⅓"
showFrac 2 3 = "⅔"
showFrac 1 4 = "¼"
showFrac 3 4 = "¾"
showFrac 1 5 = "⅕"
showFrac 2 5 = "⅖"
showFrac 3 5 = "⅗"
showFrac 4 5 = "⅘"
showFrac 1 6 = "⅙"
showFrac 5 6 = "⅚"
showFrac 1 7 = "⅐"
showFrac 1 8 = "⅛"
showFrac 3 8 = "⅜"
showFrac 5 8 = "⅝"
showFrac 7 8 = "⅞"
showFrac 1 9 = "⅑"
showFrac 1 10 = "⅒"

showFrac n d = fromMaybe plain $ do n' <- up n
                                    d' <- down d
                                    return $ n' ++ d'
  where plain = " " ++ show n ++ "/" ++ show d
        up 1 = Just "¹"
        up 2 = Just "²"
        up 3 = Just "³"
        up 4 = Just "⁴"
        up 5 = Just "⁵"
        up 6 = Just "⁶"
        up 7 = Just "⁷"
        up 8 = Just "⁸"
        up 9 = Just "⁹"
        up 0 = Just "⁰"
        up _ = Nothing
        down 1 = Just "₁"
        down 2 = Just "₂"
        down 3 = Just "₃"
        down 4 = Just "₄"
        down 5 = Just "₅"
        down 6 = Just "₆"
        down 7 = Just "₇"
        down 8 = Just "₈"
        down 9 = Just "₉"
        down 0 = Just "₀"
        down _ = Nothing

------------------------------------------------------------------------
-- * Internal functions

queryArc :: Pattern a -> Arc -> [Event a]
queryArc p a = query p $ State a Map.empty 

-- | Get the timespan of an event's 'whole'
eventWhole :: Event a -> Arc
eventWhole = fst . fst

-- | Get the onset of an event's 'whole'
eventWholeOnset :: Event a -> Time
eventWholeOnset = fst . fst . fst

-- | Get the timespan of an event's 'part'
eventPart :: Event a -> Arc
eventPart = snd . fst

eventValue :: Event a -> a
eventValue = snd

eventHasOnset :: Event a -> Bool
eventHasOnset e = (fst $ eventWhole e) == (fst $ eventPart e)

isDigital :: Pattern a -> Bool
isDigital = (== Digital) . nature

isAnalog :: Pattern a -> Bool
isAnalog = not . isDigital

-- | Splits the given 'Arc' into a list of 'Arc's, at cycle boundaries.
arcCycles :: Arc -> [Arc]
arcCycles (s,e) | s >= e = []
                | sam s == sam e = [(s,e)]
                | otherwise = (s, nextSam s) : (arcCycles (nextSam s, e))

-- | Like arcCycles, but returns zero-width arcs
arcCyclesZW :: Arc -> [Arc]
arcCyclesZW (s,e) | s == e = [(s,e)]
                  | otherwise = arcCycles (s,e)

-- | Similar to 'mapArc' but time is relative to the cycle (i.e. the
-- sam of the start of the arc)
mapCycle :: (Time -> Time) -> Arc -> Arc
mapCycle f (s,e) = (sam' + (f $ s - sam'), sam' + (f $ e - sam'))
         where sam' = sam s

-- | Splits queries that span cycles. For example `query p (0.5, 1.5)` would be
-- turned into two queries, `(0.5,1)` and `(1,1.5)`, and the results
-- combined. Being able to assume queries don't span cycles often
-- makes transformations easier to specify.
splitQueries :: Pattern a -> Pattern a
splitQueries p = p {query = \st -> concatMap (\a -> query p st {arc = a}) $ arcCyclesZW (arc st)}

-- | The 'sam' (start of cycle) for the given time value
sam :: Time -> Time
sam = fromIntegral . (floor :: Time -> Int)

-- | Turns a number into a (rational) time value. An alias for 'toRational'.
toTime :: Real a => a -> Rational
toTime = toRational

-- | The end point of the current cycle (and starting point of the next cycle)
nextSam :: Time -> Time
nextSam = (1+) . sam

-- | The position of a time value relative to the start of its cycle.
cyclePos :: Time -> Time
cyclePos t = t - sam t

-- | @subArc i j@ is the timespan that is the intersection of @i@ and @j@.
subArc :: Arc -> Arc -> Maybe Arc
subArc (s, e) (s',e') | s'' <= e'' = Just (s'', e'')
                      | otherwise = Nothing
  where s'' = max s s'
        e'' = min e e'

{-
maybeSubArc :: Maybe Arc -> Maybe Arc -> Maybe (Maybe Arc)
maybeSubArc (Just a) (Just b) = Just $ subArc a b
maybeSubArc _ _ = Nothing
-}

-- | The arc of the whole cycle that the given time value falls within
timeToCycleArc :: Time -> Arc
timeToCycleArc t = (sam t, (sam t) + 1)

-- | A list of cycle numbers which are included in the given arc
cyclesInArc :: Integral a => Arc -> [a]
cyclesInArc (s,e) | s > e = []
                  | s == e = [floor s]
                  | otherwise = [floor s .. (ceiling e)-1]

-- | A list of arcs of the whole cycles which are included in the given arc
cycleArcsInArc :: Arc -> [Arc]
cycleArcsInArc = map (timeToCycleArc . (toTime :: Int -> Time)) . cyclesInArc

-- | Apply a function to the arcs/timespans (both whole and parts) of the result
withResultArc :: (Arc -> Arc) -> Pattern a -> Pattern a
withResultArc f p = p {query = map (mapFst (mapBoth f)) . query p}

-- | Apply a function to the time (both start and end of the timespans
-- of both whole and parts) of the result
withResultTime :: (Time -> Time) -> Pattern a -> Pattern a
withResultTime = withResultArc . mapBoth

-- | Apply a function to the timespan of the query
withQueryArc :: (Arc -> Arc) -> Pattern a -> Pattern a
withQueryArc f p = p {query = query p . (\(State a m) -> State (f a) m)}

-- | Apply a function to the time (both start and end) of the query
withQueryTime :: (Time -> Time) -> Pattern a -> Pattern a
withQueryTime = withQueryArc . mapBoth

-- | Compares two lists of events, attempting to combine fragmented events in the process
-- for a 'truer' compare
compareDefrag :: (Eq a, Ord a) => [Event a] -> [Event a] -> Bool
compareDefrag as bs = (sort $ defragParts as) == (sort $ defragParts bs)

-- | Returns a list of events, with any adjacent parts of the same whole combined
defragParts :: Eq a => [Event a] -> [Event a]
defragParts [] = []
defragParts (e:[]) = (e:[])
defragParts (e:es) | isJust i = defraged:(defragParts (delete e' es))
                   | otherwise = e:(defragParts es)
  where i = findIndex (isAdjacent e) es
        e' = es !! (fromJust i)
        defraged = ((eventWhole e, part),eventValue e)
        part = (start,end)
        start = min (fst $ eventPart e) (fst $ eventPart e')
        end = max (snd $ eventPart e) (snd $ eventPart e')

-- | Returns 'True' if the two given events are adjacent parts of the same whole
isAdjacent :: Eq a => Event a -> Event a -> Bool
isAdjacent e e' = (eventWhole e == eventWhole e')
                  && (eventValue e == eventValue e')
                  && (((snd $ eventPart e) == (fst $ eventPart e'))
                      ||
                      ((snd $ eventPart e') == (fst $ eventPart e))
                     )

-- | Compare the events of two patterns using the given arc
compareP :: (Ord a, Show a) => Arc -> Pattern a -> Pattern a -> Bool
compareP a p p' = (sort $ query p $ State a Map.empty) == (sort $ query p' $ State a Map.empty)

-- | Like @compareP@, but tries to 'defragment' the events
comparePD :: (Ord a, Show a) => Arc -> Pattern a -> Pattern a -> Bool
comparePD a p p' = compareDefrag es es'
  where es = query p (State a Map.empty)
        es' = query p' (State a Map.empty)

-- | Apply one of three functions to a Value, depending on its type
applyFIS :: (Float -> Float) -> (Int -> Int) -> (String -> String) -> Value -> Value
applyFIS f _ _ (VF f') = VF $ f f'
applyFIS _ f _ (VI i ) = VI $ f i
applyFIS _ _ f (VS s ) = VS $ f s

-- | Apply one of two functions to a Value, depending on its type (int
-- or float; strings are ignored)
fNum2 :: (Int -> Int -> Int) -> (Float -> Float -> Float) -> Value -> Value -> Value
fNum2 fInt _      (VI a) (VI b) = VI $ fInt a b
fNum2 _    fFloat (VF a) (VF b) = VF $ fFloat a b
fNum2 _    fFloat (VI a) (VF b) = VF $ fFloat (fromIntegral a) b
fNum2 _    fFloat (VF a) (VI b) = VF $ fFloat a (fromIntegral b)
fNum2 _    _      x      _      = x

-- ** Event filters

-- | Remove events from patterns that to not meet the given test
filterValues :: (a -> Bool) -> Pattern a -> Pattern a
filterValues f p = p {query = (filter (f . snd)) . query p}

-- | Turns a pattern of 'Maybe' values in to a pattern of values,
-- dropping the events of 'Nothing'.
filterJust :: Pattern (Maybe a) -> Pattern a
filterJust p = fromJust <$> (filterValues (isJust) p)

filterWhen :: (Time -> Bool) -> Pattern a -> Pattern a
filterWhen test p = p {query = filter (test . eventWholeOnset) . query p}

playFor :: Time -> Time -> Pattern a -> Pattern a
playFor s e = filterWhen (\t -> and [t >= s, t < e])

-- ** Temporal parameter helpers

tParam :: (a -> Pattern b -> Pattern c) -> (Pattern a -> Pattern b -> Pattern c)
tParam f tv p = innerJoin $ (`f` p) <$> tv

tParam2 :: (a -> b -> Pattern c -> Pattern d) -> (Pattern a -> Pattern b -> Pattern c -> Pattern d)
tParam2 f a b p = unwrap $ (\x y -> f x y p) <$> a <*> b

tParam3 :: (a -> b -> c -> Pattern d -> Pattern e) -> (Pattern a -> Pattern b -> Pattern c -> Pattern d -> Pattern e)
tParam3 f a b c p = unwrap $ (\x y z -> f x y z p) <$> a <*> b <*> c

tParamSqueeze :: (a -> Pattern b -> Pattern c) -> (Pattern a -> Pattern b -> Pattern c)
tParamSqueeze f tv p = unwrapSqueeze $ (`f` p) <$> tv

------------------------------------------------------------------------
-- * UI

-- ** Pattern algebra

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

(|>|) :: (Applicative a) => a b -> a b -> a b
a |>| b = (flip const) <$> a <*> b
(|> ) :: Pattern a -> Pattern a -> Pattern a
a |>  b = (flip const) <$> a <* b
( >|) :: Pattern a -> Pattern a -> Pattern a
a  >| b = (flip const) <$> a *> b

(|<|) :: (Applicative a) => a b -> a b -> a b
a |<| b = const <$> a <*> b
(|< ) :: Pattern a -> Pattern a -> Pattern a
a |<  b = const <$> a <* b
( <|) :: Pattern a -> Pattern a -> Pattern a
a  <| b = const <$> a *> b

(</) :: ControlPattern -> ControlPattern -> ControlPattern
a </ b = (Map.union) <$> a *> b

(/<) :: ControlPattern -> ControlPattern -> ControlPattern
a /< b = (Map.union) <$> a <* b

(>/) :: ControlPattern -> ControlPattern -> ControlPattern
a >/ b = (flip Map.union) <$> a <* b

(/>) :: ControlPattern -> ControlPattern -> ControlPattern
a /> b = (flip Map.union) <$> a <* b

-- ** Elemental patterns

-- | An empty pattern
silence :: Pattern a
silence = Pattern {nature = Digital, query = const []}

-- | Takes a function from time to values, and turns it into a 'Pattern'.
sig :: (Time -> a) -> Pattern a
sig f = Pattern Analog q
  where q (State (s,e) _) | s > e = []
                          | otherwise = [(((s,e), (s,e)), f (s+((e-s)/2)))]

-- | @sine@ returns a 'Pattern' of continuous 'Fractional' values following a
-- sinewave with frequency of one cycle, and amplitude from 0 to 1.
sine :: Fractional a => Pattern a
sine = sig $ \t -> ((sin_rat $ (pi :: Float) * 2 * (fromRational t)) + 1) / 2
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
rotL :: Time -> Pattern a -> Pattern a
rotL t p = withResultTime (subtract t) $ withQueryTime (+ t) p

-- | Infix alias for 'rotL'
(<~) :: Pattern Time -> Pattern a -> Pattern a
(<~) = tParam rotL

-- | Shifts a pattern forward in time by the given amount, expressed in cycles
rotR :: Time -> Pattern a -> Pattern a
rotR t = rotL (0-t)

-- | Infix alias for 'rotR'
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
_fastGap :: Time -> Pattern a -> Pattern a
_fastGap 0 _ = silence
_fastGap r p = splitQueries $ 
  withResultArc (\(s,e) -> (sam s + ((s - sam s)/r'),
                             sam s + ((e - sam s)/r')
                            )
                 ) $ p {query = f}
  where r' = max r 1
        -- zero width queries of the next sam should return zero in this case..
        f st@(State a _) | fst a' == nextSam (fst a) = []
                         | otherwise = query p st {arc = a'}
          where mungeQuery t = sam t + (min 1 $ r' * cyclePos t)
                a' = mapBoth mungeQuery a

compress :: Arc -> Pattern a -> Pattern a
compress (s,e) p | s > e = silence
                 | s > 1 || e > 1 = silence
                 | s < 0 || e < 0 = silence
                 | otherwise = s `rotR` _fastGap (1/(e-s)) p

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

controlF :: String -> Pattern Float
controlF s = Pattern Analog $ \(State a m) -> maybe [] (f a) $ Map.lookup s m
  where f a (VI v) = [((a,a),fromIntegral v)]
        f a (VF v) = [((a,a),v)]
        f a (VS v) = maybe [] (\v' -> [((a,a),v')]) (readMaybe v)

controlS :: String -> Pattern String
controlS s = Pattern Analog $ \(State a m) -> maybe [] (f a) $ Map.lookup s m
  where f a (VI v) = [((a,a),show v)]
        f a (VF v) = [((a,a),show v)]
        f a (VS v) = [((a,a),v)]

scale :: (Functor f, Num b) => b -> b -> f b -> f b
scale from to p = ((+ from) . (* (to-from))) <$> p

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
someCyclesBy x = when (test x)
  where test x c = (timeToRand (fromIntegral c :: Double)) < x

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


{-

{- | (The above is difficult to describe, if you don't understand Haskell,
just ignore it and read the below..)

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
        mrg _ = error "groupByTime"


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

timeToRand :: RealFrac r => r -> Double
timeToRand t = fst $ randomDouble $ pureMT $ floor $ (*1000000) t

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
d1 $ sound (samples "xx(3,8)" (tom $ choose ["a", "e", "g", "c"]))
@

plays a melody randomly choosing one of the four notes \"a\", \"e\", \"g\", \"c\".
-}
choose :: [a] -> Pattern a
choose [] =  E.throw (E.ErrorCall "Empty list. Nothing to choose from.")
choose xs = (xs !!) <$> (irand $ length xs)



-- | @wedge t p p'@ combines patterns @p@ and @p'@ by squashing the
-- @p@ into the portion of each cycle given by @t@, and @p'@ into the
-- remainer of each cycle.
wedge :: Time -> Pattern a -> Pattern a -> Pattern a
wedge t p p' = overlay (densityGap (1/t) p) (t `rotR` densityGap (1/(1-t)) p')

timeCat :: [(Time, Pattern a)] -> Pattern a
timeCat tps = stack $ map (\(s,e,p) -> compress (s/total, e/total) p) $ arrange 0 tps
    where total = sum $ map fst tps
          arrange :: Time -> [(Time, Pattern a)] -> [(Time, Time, Pattern a)]
          arrange _ [] = []
          arrange t ((t',p):tps) = (t,t+t',p):(arrange (t+t') tps)

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
superimpose :: (Pattern a -> Pattern a) -> Pattern a -> Pattern a
superimpose f p = stack [p, f p]

-- | @splitQueries p@ wraps `p` to ensure that it does not get
-- queries that span arcs. For example `arc p (0.5, 1.5)` would be
-- turned into two queries, `(0.5,1)` and `(1,1.5)`, and the results
-- combined. Being able to assume queries don't span cycles often
-- makes transformations easier to specify.
splitQueries :: Pattern a -> Pattern a
splitQueries p = Pattern $ \a -> concatMap (arc p) $ arcCycles a

{- | @trunc@ truncates a pattern so that only a fraction of the pattern is played.
The following example plays only the first quarter of the pattern:

@
d1 $ trunc 0.25 $ sound "bd sn*2 cp hh*4 arpy bd*2 cp bd*2"
@
-}
trunc :: Pattern Time -> Pattern a -> Pattern a
trunc = temporalParam _trunc

_trunc :: Time -> Pattern a -> Pattern a
_trunc t = compress (0,t) . zoom (0,t)

{- | @linger@ is similar to `trunc` but the truncated part of the pattern loops until the end of the cycle

@
d1 $ linger 0.25 $ sound "bd sn*2 cp hh*4 arpy bd*2 cp bd*2"
@
-}
linger :: Pattern Time -> Pattern a -> Pattern a
linger = temporalParam _linger

_linger :: Time -> Pattern a -> Pattern a
_linger n p = _density (1/n) $ zoom (0,n) p

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
zoom (s,e) p = splitQueries $ withResultArc (mapCycle ((/d) . (subtract s))) $ withQueryArc (mapCycle ((+s) . (*d))) p
     where d = e-s

compress :: Arc -> Pattern a -> Pattern a
compress (s,e) p | s >= e = silence
                 | otherwise = s `rotR` densityGap (1/(e-s)) p

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
within (s,e) f p = stack [playWhen (\t -> cyclePos t >= s && cyclePos t < e) $ f p,
                          playWhen (\t -> not $ cyclePos t >= s && cyclePos t < e) $ p
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
within' (s,e) f p = stack [playWhen (\t -> cyclePos t >= s && cyclePos t < e) $ compress (s,e) $ f $ zoom (s,e) $ p, 
                           playWhen (\t -> not $ cyclePos t >= s && cyclePos t < e) $ p 
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
e :: Pattern Int -> Pattern Int -> Pattern a -> Pattern a
e = temporalParam2 _e

_e :: Int -> Int -> Pattern a -> Pattern a
_e n k p = (flip const) <$> (filterValues (== True) $ listToPat $ bjorklund (n,k)) <*> p

e' :: Pattern Int -> Pattern Int -> Pattern a -> Pattern a
e' = temporalParam2 _e'

_e' :: Int -> Int -> Pattern a -> Pattern a
_e' n k p = fastcat $ map (\x -> if x then p else silence) (bjorklund (n,k))


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
einv :: Pattern Int -> Pattern Int -> Pattern a -> Pattern a
einv = temporalParam2 _einv

_einv :: Int -> Int -> Pattern a -> Pattern a
_einv n k p = (flip const) <$> (filterValues (== False) $ listToPat $ bjorklund (n,k)) <*> p

{- | `efull n k pa pb` stacks @e n k pa@ with @einv n k pb@ -}
efull :: Pattern Int -> Pattern Int -> Pattern a -> Pattern a -> Pattern a
efull n k pa pb = stack [ e n k pa, einv n k pb ]

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

-- | @discretise n p@: 'samples' the pattern @p@ at a rate of @n@
-- events per cycle. Useful for turning a continuous pattern into a
-- discrete one.
discretise :: Time -> Pattern a -> Pattern a
discretise = _discretise

discretise' :: Pattern Time -> Pattern a -> Pattern a
discretise' n p = (density n $ atom (id)) <*> p

_discretise :: Time -> Pattern a -> Pattern a
_discretise n p = (_density n $ atom (id)) <*> p

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
fit perCycle xs p = (xs !!!) <$> (Pattern $ \a -> map ((\e -> (mapThd' (+ (cyclePos perCycle e)) e))) (arc p a))
  where cyclePos perCycle e = perCycle * (floor $ eventStart e)

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
substruct s p = Pattern $ f
  where f a = concatMap (\a' -> arc (compressTo a' p) a') $ (map fst' $ arc s a)

compressTo :: Arc -> Pattern a -> Pattern a
compressTo (s,e) p = compress (cyclePos s, e-(sam s)) p

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

randStruct :: Int -> Pattern Int
randStruct n = splitQueries $ Pattern f
  where f (s,e) = mapSnds' fromJust $ filter (\(_,x,_) -> isJust x) $ as
          where as = map (\(n, (s',e')) -> ((s' + sam s, e' + sam s),
                                           subArc (s,e) (s' + sam s, e' + sam s),
                                           n
                                          )
                         ) $ enumerate $ thd' $ head $ arc (randArcs n) (sam s, nextSam s)

substruct' :: Pattern Int -> Pattern a -> Pattern a
substruct' s p = Pattern $ \a -> concatMap (\(a', _, i) -> arc (compressTo a' (inside (pure $ 1/toRational(length (arc s (sam (fst a), nextSam (fst a))))) (rotR (toRational i)) p)) a') (arc s a)

-- | @stripe n p@: repeats pattern @p@, @n@ times per cycle. So
-- similar to @fast@, but with random durations. The repetitions will
-- be continguous (touching, but not overlapping) and the durations
-- will add up to a single cycle. @n@ can be supplied as a pattern of
-- integers.
stripe :: Pattern Int -> Pattern a -> Pattern a
stripe = temporalParam _stripe

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
fit' :: Pattern Time -> Int -> Pattern Int -> Pattern Int -> Pattern a -> Pattern a
fit' cyc n from to p = unwrap' $ fit n (mapMasks n from' p') to
  where mapMasks n from p = [stretch $ mask (filterValues (== i) from) p
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
loopFirst p = splitQueries $ Pattern f
  where f a@(s,_) = mapSnds' plus $ mapFsts' plus $ arc p (minus a)
          where minus = mapArc (subtract (sam s))
                plus = mapArc (+ (sam s))

timeLoop :: Pattern Time -> Pattern a -> Pattern a
timeLoop n = outside n loopFirst

seqPLoop :: [(Time, Time, Pattern a)] -> Pattern a
seqPLoop ps = timeLoop (pure $ maxT - minT) $ minT `rotL` seqP ps
  where minT = minimum $ map fst' ps
        maxT = maximum $ map snd' ps

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
cycleChoose xs = Pattern $ \(s,e) -> [((s,e),(s,e), xs!!(floor $ (dlen xs)*(ctrand s) ))]
  where dlen xs = fromIntegral $ length xs
        ctrand s = (timeToRand :: Time -> Double) $ fromIntegral $ (floor :: Time -> Int) $ sam s

{- | `shuffle n p` evenly divides one cycle of the pattern `p` into `n` parts,
and returns a random permutation of the parts each cycle.  For example,
`shuffle 3 "a b c"` could return `"a b c"`, `"a c b"`, `"b a c"`, `"b c a"`,
`"c a b"`, or `"c b a"`.  But it will **never** return `"a a a"`, because that
is not a permutation of the parts.
-}
shuffle::Int -> Pattern a -> Pattern a
shuffle n = fit' 1 n (_run n) (randpat n)
  where randpat n = Pattern $ \(s,e) -> arc (p n $ sam s) (s,e)
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
scramble n = fit' 1 n (_run n) (_density (fromIntegral n) $
  liftA2 (+) (pure 0) $ irand n)

ur :: Time -> Pattern String -> [(String, Pattern a)] -> [(String, Pattern a -> Pattern a)] -> Pattern a
ur t outer_p ps fs = _slow t $ unwrap $ adjust <$> (timedValues $ (getPat . split) <$> outer_p)
  where split s = wordsBy (==':') s
        getPat (s:xs) = (match s, transform xs)
        match s = fromMaybe silence $ lookup s ps'
        ps' = map (fmap (_density t)) ps
        adjust (a, (p, f)) = f a p
        transform (x:_) a = transform' x a
        transform _ _ = id
        transform' str (s,e) p = s `rotR` (inside (pure $ 1/(e-s)) (matchF str) p)
        matchF str = fromMaybe id $ lookup str fs

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
flatpat p = Pattern $ \a -> (concatMap (\(b,b',xs) -> map (\x -> (b,b',x)) xs) $ arc p a)

-- | @layer@ takes a Pattern of lists and pulls the list elements as
-- separate Events
layer :: [a -> Pattern b] -> a -> Pattern b
layer fs p = stack $ map ($ p) fs

-- | @breakUp@ finds events that share the same timespan, and spreads them out during that timespan, so for example @breakUp "[bd,sn]"@ gets turned into @"bd sn"@
breakUp :: Pattern a -> Pattern a
breakUp p = Pattern $ \a -> munge $ arc p a
  where munge es = concatMap spreadOut (groupBy (\a b -> fst' a == fst' b) es)
        spreadOut xs = mapMaybe (\(n, x) -> shiftIt n (length xs) x) $ enumerate xs
        shiftIt n d ((s,e), a', v) = do a'' <- subArc (newS, newE) a'
                                        return ((newS, newE), a'', v)
          where newS = s + (dur*(fromIntegral n))
                newE = newS + dur
                dur = (e - s) / (fromIntegral d)

-- | @fill@ 'fills in' gaps in one pattern with events from another. For example @fill "bd" "cp ~ cp"@ would result in the equivalent of `"~ bd ~"`. This only finds gaps in a resulting pattern, in other words @"[bd ~, sn]"@ doesn't contain any gaps (because @sn@ covers it all), and @"bd ~ ~ sn"@ only contains a single gap that bridges two steps.
fill :: Pattern a -> Pattern a -> Pattern a
fill p' p = struct (splitQueries $ Pattern (f p)) p'
  where
    f p (s,e) = removeTolerance (s,e) $ invert (s-tolerance, e+tolerance) $ arc p (s-tolerance, e+tolerance)
    invert (s,e) es = map arcToEvent $ foldr remove [(s,e)] (map snd' es)
    remove (s,e) xs = concatMap (remove' (s, e)) xs
    remove' (s,e) (s',e') | s > s' && e < e' = [(s',s),(e,e')] -- inside
                          | s > s' && s < e' = [(s',s)] -- cut off right
                          | e > s' && e < e' = [(e,e')] -- cut off left
                          | s <= s' && e >= e' = [] -- swallow
                          | otherwise = [(s',e')] -- miss
    arcToEvent a = (a,a,"x")
    removeTolerance (s,e) es = concatMap (expand) $ mapSnds' f es
      where f a = concatMap (remove' (e,e+tolerance)) $ remove' (s-tolerance,s) a
            expand (a,xs,c) = map (\x -> (a,x,c)) xs
    tolerance = 0.01

-- Repeats each event @n@ times within its arc
ply :: Pattern Int -> Pattern a -> Pattern a
ply = temporalParam _ply

_ply :: Int -> Pattern a -> Pattern a
_ply n p = breakUp $ stack (replicate n p)

-- Uses the first (binary) pattern to switch between the following two
-- patterns. 
sew :: Pattern Bool -> Pattern a -> Pattern a -> Pattern a
sew stitch p1 p2 = overlay (const <$> p1 <*> a) (const <$> p2 <*> b)
  where a = filterValues (id) stitch
        b = filterValues (not . id) stitch

-}

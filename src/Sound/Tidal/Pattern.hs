module Sound.Tidal.Pattern where

import Prelude hiding ((<*), (*>))
import qualified Data.Map.Strict as Map
import Data.Fixed (mod')
import Data.Maybe (isJust, isNothing, fromJust, catMaybes, fromMaybe)
-- import Data.Ratio
import Control.Applicative (liftA2)
-- import Data.Maybe (mapMaybe)

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

-- | A function that represents events taking place over time
type Query a = (Arc -> [Event a])

-- | Also known as Continuous vs Discrete/Amorphous vs Pulsating etc.
data Nature = Analog | Digital
            deriving Eq

-- | A datatype that's basically a query, plus a hint about whether its events
-- are Analogue or Digital by nature
data Pattern a = Pattern {nature :: Nature, query :: Query a}

type PatternMap a b = Pattern (Map.Map a b)

------------------------------------------------------------------------
-- * Instances

instance Functor Pattern where
  -- | apply a function to all the values in a pattern
  fmap f p = p {query = (fmap (fmap f)) . query p}

instance Applicative Pattern where
  -- | Repeat the given value once per cycle, forever
  pure v = Pattern Digital $ \(s,e) -> map (\(s',e') -> (constrain (s,e) (s',e'),v)) $ cycleArcsInArc (s,e)
    where constrain (s,e) (s',e') = ((s',e'), (max s s', min e e'))

  -- for the part of each event in pf
  -- - get matching events px matching the arc
  -- - for both whole and part, take the intersection of pf and px
  -- - (if the event is continuous, just take the part)
{-
pf <*> px = Pattern q
    where q arc = catMaybes $ concat $ map match $ query pf arc
            where
              match ((fWhole, fPart), f) =
                map
                (\((xWhole, xPart),x) ->
                      do part' <- subArc fPart xPart
                         let xWhole' = fromMaybe xPart xWhole
                         whole' <- maybe (Just part') (subArc xWhole') fWhole
                         return ((Just whole', part'), f x)
                )
                (query px fPart)
-}

  (<*>) pf@(Pattern Digital _) px@(Pattern Digital _) = Pattern Digital q
    where q a = catMaybes $ concat $ map match $ query pf a
            where
              match ((fWhole, fPart), f) =
                map
                (\((xWhole, xPart),x) ->
                  do whole' <- subArc xWhole fWhole
                     part' <- subArc fPart xPart
                     return ((whole', part'), f x)
                )
                (query px fPart)
  (<*>) pf@(Pattern Digital _) px@(Pattern Analog _) = Pattern Digital q
    where q a = concatMap match $ query pf a
            where
              match ((fWhole, fPart), f) =
                map
                (\(_ ,x) -> ((fWhole, fPart), f x))
                (query px (fst fPart, fst fPart))

  (<*>) pf@(Pattern Analog _) px@(Pattern Digital _) = Pattern Digital q
    where q a = concatMap match $ query px a
            where
              match ((xWhole, xPart), x) =
                map
                (\(_ ,f) -> ((xWhole, xPart), f x))
                (query pf (fst xPart, fst xPart))
                
  (<*>) pf px = Pattern Analog q
    where q a = concatMap match $ query pf a
            where
              match (_, f) =
                map
                (\(_ ,x) -> ((a,a), f x))
                (query px a)

-- | Like <*>, but the structure only comes from the left
(<*) :: Pattern (a -> b) -> Pattern a -> Pattern b
(<*) pf@(Pattern Digital _) px = Pattern Digital q
  where q a = concatMap match $ query pf a
         where
            match ((fWhole, fPart), f) =
              map
              (\(_, x) -> ((fWhole, fPart), f x)) $
              query px $ xQuery fWhole
            xQuery ((s,_)) = (s,s) -- for discrete events, match with the onset
            

pf <* px = Pattern Analog q
  where q a = concatMap match $ query pf a
          where
            match ((fWhole, fPart), f) =
              map
              (\(_, x) -> ((fWhole, fPart), f x)) $
              query px a -- for continuous events, use the original query

-- | Like <*>, but the structure only comes from the right
(*>) :: Pattern (a -> b) -> Pattern a -> Pattern b
(*>) pf px@(Pattern Digital _) = Pattern Digital q
  where q a = concatMap match $ query px a
         where
            match ((xWhole, xPart), x) =
              map
              (\(_, f) -> ((xWhole, xPart), f x)) $
              query pf $ fQuery xWhole
            fQuery ((s,_)) = (s,s) -- for discrete events, match with the onset
            

pf *> px = Pattern Analog q
  where q a = concatMap match $ query px a
          where
            match ((xWhole, xPart), x) =
              map
              (\(_, f) -> ((xWhole, xPart), f x)) $
              query pf a -- for continuous events, use the original query

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
-- TODO - what is a continuous pattern contains a discrete one, or vice-versa?

unwrap :: Pattern (Pattern a) -> Pattern a
unwrap pp = pp {query = q}
  where q arc = concatMap (\((whole, part), p) -> catMaybes $ map (munge whole part) $ query p part) (query pp arc)
        munge oWhole oPart ((iWhole, iPart),v) = do w <- subArc oWhole iWhole
                                                    p <- subArc oPart iPart
                                                    return ((w,p),v)

-- | Like @unwrap@, but cycles of the inner patterns are compressed to fit the
-- timespan of the outer whole (or the original query if it's a continuous pattern?)
-- TODO - what is a continuous pattern contains a discrete one, or vice-versa?
unwrap' :: Pattern (Pattern a) -> Pattern a
unwrap' pp = pp {query = q}
  where q arc = concatMap (\((whole, part), p) -> catMaybes $ map (munge whole part) $ query (compress whole p) part) (query pp arc)
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

------------------------------------------------------------------------
-- * Internal functions

-- | Get the timespan of an event's 'whole'
eventWhole :: Event a -> Arc
eventWhole = fst . fst

-- | Get the timespan of an event's 'part'
eventPart :: Event a -> Arc
eventPart = snd . fst

eventValue :: Event a -> a
eventValue = snd

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
splitQueries p = p {query = \a -> concatMap (query p) $ arcCyclesZW a}

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
subArc (s, e) (s',e') | s'' < e'' = Just (s'', e'')
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
withQueryArc f p = p {query = query p . f}

-- | Apply a function to the time (both start and end) of the query
withQueryTime :: (Time -> Time) -> Pattern a -> Pattern a
withQueryTime = withQueryArc . mapBoth

-- ** Event filters

-- | Remove events from patterns that to not meet the given test
filterValues :: (a -> Bool) -> Pattern a -> Pattern a
filterValues f p = p {query = (filter (f . snd)) . query p}

-- | Turns a pattern of 'Maybe' values in to a pattern of values,
-- dropping the events of 'Nothing'.
filterJust :: Pattern (Maybe a) -> Pattern a
filterJust p = fromJust <$> (filterValues (isJust) p)

-- ** Temporal parameter helpers

temporalParam :: (a -> Pattern b -> Pattern c) -> (Pattern a -> Pattern b -> Pattern c)
temporalParam f tv p = unwrap $ (`f` p) <$> tv

temporalParam2 :: (a -> b -> Pattern c -> Pattern d) -> (Pattern a -> Pattern b -> Pattern c -> Pattern d)
temporalParam2 f a b p = unwrap $ (\x y -> f x y p) <$> a <*> b

temporalParam3 :: (a -> b -> c -> Pattern d -> Pattern e) -> (Pattern a -> Pattern b -> Pattern c -> Pattern d -> Pattern e)
temporalParam3 f a b c p = unwrap $ (\x y z -> f x y z p) <$> a <*> b <*> c


------------------------------------------------------------------------
-- * UI

-- ** Pattern algebra
{-
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
-}

-- ** Elemental patterns

-- | An empty pattern
silence :: Pattern a
silence = Pattern {nature = Digital, query = const []}

-- | Takes a function from time to values, and turns it into a 'Pattern'.
sig :: (Time -> a) -> Pattern a
sig f = Pattern Analog q
  where q (s,e) | s > e = []
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

-- | @fromMaybes@ is similar to 'fromList', but allows values to
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
        q a = concatMap f $ arcCyclesZW a
        f a = query (withResultTime (+offset) p) $  mapBoth (subtract offset) a
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
overlay p@(Pattern Analog _) p'@(Pattern Analog _) = Pattern Analog $ \a -> (query p a) ++ (query p' a)
-- Otherwise digital. Won't really work to have a mixture.. Hmm
overlay p p' = Pattern Digital $ \a -> (query p a) ++ (query p' a)

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
(<~) = temporalParam rotL

-- | Shifts a pattern forward in time by the given amount, expressed in cycles
rotR :: Time -> Pattern a -> Pattern a
rotR t = rotL (0-t)

-- | Infix alias for 'rotR'
(~>) :: Pattern Time -> Pattern a -> Pattern a
(~>) = temporalParam rotR

-- | Speed up a pattern by the given factor
fast :: Pattern Time -> Pattern a -> Pattern a
fast = temporalParam _fast

-- | An alias for fast
density :: Pattern Time -> Pattern a -> Pattern a
density = fast

_fast :: Time -> Pattern a -> Pattern a
_fast r p | r == 0 = silence
          | r < 0 = rev $ _fast (0-r) p
          | otherwise = withResultTime (/ r) $ withQueryTime (* r) p

-- | Slow down a pattern by the given factor
slow :: Pattern Time -> Pattern a -> Pattern a
slow = temporalParam _slow
_slow :: Time -> Pattern a -> Pattern a
_slow r p = _fast (1/r) p

-- | An alias for slow
sparsity :: Pattern Time -> Pattern a -> Pattern a
sparsity = slow

-- | @rev p@ returns @p@ with the event positions in each cycle
-- reversed (or mirrored).
rev :: Pattern a -> Pattern a
rev p = splitQueries $ p {query = \a -> map makeWholeAbsolute $ mapParts (mirrorArc (mid a)) $ map makeWholeRelative (query p (mirrorArc (mid a) a))}
  where makeWholeRelative (((s,e), part@(s',e')), v) = (((s'-s, e-e'), part), v)
        makeWholeAbsolute (((s,e), part@(s',e')), v) = (((s'-e, e'+s), part), v)
        mid (s,_) = (sam s) + 0.5
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
fastGap = temporalParam _fastGap
_fastGap :: Time -> Pattern a -> Pattern a
_fastGap 0 _ = silence
_fastGap r p = splitQueries $ 
  withResultArc (\(s,e) -> (sam s + ((s - sam s)/r'),
                             sam s + ((e - sam s)/r')
                            )
                 ) $ p {query = f}
  where r' = max r 1
        -- zero width queries of the next sam should return zero in this case..
        f a | fst a' == nextSam (fst a) = []
            | otherwise = query p a'
              where mungeQuery t = sam t + (min 1 $ r' * cyclePos t)
                    a' = mapBoth mungeQuery a

compress :: Arc -> Pattern a -> Pattern a
compress (s,e) p | s > e = silence
                 | s > 1 || e > 1 = silence
                 | s < 0 || e < 0 = silence
                 | otherwise = s `rotR` _fastGap (1/(e-s)) p


-- | * Higher order functions

-- | Functions which work on other functions (higher order functions)

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
  where apply a | test (floor $ fst a) = query (f p) a
                | otherwise = query p a

-- | Like 'when', but works on continuous time values rather than cycle numbers.
whenT :: (Time -> Bool) -> (Pattern a -> Pattern a) ->  Pattern a -> Pattern a
whenT test f p = splitQueries $ p {query = apply}
  where apply a | test (fst a) = query (f p) a
                | otherwise = query p a



--eoff :: Int -> Int -> Integer -> Pattern a -> Pattern a
--eoff n k s p = ((s%(fromIntegral k)) `rotL`) (_e n k p)
   -- TPat_ShiftL (s%(fromIntegral k)) (TPat_E n k p)


-- TODO

-- spread and friends


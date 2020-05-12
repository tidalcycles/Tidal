{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sound.Tidal.Pattern where

import           Prelude hiding ((<*), (*>))

import           Control.Applicative (liftA2)
--import           Data.Bifunctor (Bifunctor(..))
import           Data.Data (Data) -- toConstr
import           Data.List (delete, findIndex, sort)
import qualified Data.Map.Strict as Map
import           Data.Maybe (isJust, fromJust, catMaybes, mapMaybe)
import           Data.Typeable (Typeable)
import           Control.DeepSeq (NFData(rnf))
import           Data.Word (Word8)

------------------------------------------------------------------------
-- * Types

-- | Time is rational
type Time = Rational 

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

-- | An arc of time, with a start time (or onset) and a stop time (or offset)
data ArcF a = Arc
  { start :: a
  , stop :: a
  } deriving (Eq, Ord, Functor, Show)

type Arc = ArcF Time

instance NFData a => 
  NFData (ArcF a) where 
    rnf (Arc s e) = rnf s `seq` rnf e

instance Num a => Num (ArcF a) where
  negate      = fmap negate
  (+)         = liftA2 (+)
  (*)         = liftA2 (*)
  fromInteger = pure . fromInteger
  abs         = fmap abs
  signum      = fmap signum

instance (Fractional a) => Fractional (ArcF a) where
  recip        = fmap recip
  fromRational = pure . fromRational

sect :: Arc -> Arc -> Arc
sect (Arc s e) (Arc s' e') = Arc (max s s') (min e e')

-- | convex hull union
hull :: Arc -> Arc -> Arc
hull (Arc s e) (Arc s' e') = Arc (min s s') (max e e')

-- | @subArc i j@ is the timespan that is the intersection of @i@ and @j@.
-- intersection
-- The definition is a bit fiddly as results might be zero-width, but
-- not at the end of an non-zero-width arc - e.g. (0,1) and (1,2) do
-- not intersect, but (1,1) (1,1) does.
subArc :: Arc -> Arc -> Maybe Arc
subArc a@(Arc s e) b@(Arc s' e')
  | and [s'' == e'', s'' == e, s < e] = Nothing
  | and [s'' == e'', s'' == e', s' < e'] = Nothing
  | s'' <= e'' = Just (Arc s'' e'')
  | otherwise = Nothing
  where (Arc s'' e'') = sect a b

subMaybeArc :: Maybe Arc -> Maybe Arc -> Maybe (Maybe Arc)
subMaybeArc (Just a) (Just b) = do sa <- subArc a b
                                   return $ Just sa
subMaybeArc _ _ = Just Nothing

instance Applicative ArcF where
  pure t = Arc t t
  (<*>) (Arc sf ef) (Arc sx ex) = Arc (sf sx) (ef ex)

-- | The arc of the whole cycle that the given time value falls within
timeToCycleArc :: Time -> Arc
timeToCycleArc t = Arc (sam t) (sam t + 1)

-- | Shifts an arc to the equivalent one that starts during cycle zero
cycleArc :: Arc -> Arc
cycleArc (Arc s e) = Arc (cyclePos s) (cyclePos s + (e-s))

-- | A list of cycle numbers which are included in the given arc
cyclesInArc :: Integral a => Arc -> [a]
cyclesInArc (Arc s e)
  | s > e = []
  | s == e = [floor s]
  | otherwise = [floor s .. ceiling e-1]

-- | A list of arcs of the whole cycles which are included in the given arc
cycleArcsInArc :: Arc -> [Arc]
cycleArcsInArc = map (timeToCycleArc . (toTime :: Int -> Time)) . cyclesInArc

-- | Splits the given 'Arc' into a list of 'Arc's, at cycle boundaries.
arcCycles :: Arc -> [Arc]
arcCycles (Arc s e) | s >= e = []
                | sam s == sam e = [Arc s e]
                | otherwise = Arc s (nextSam s) : arcCycles (Arc (nextSam s) e)

-- | Like arcCycles, but returns zero-width arcs
arcCyclesZW :: Arc -> [Arc]
arcCyclesZW (Arc s e) | s == e = [Arc s e]
                  | otherwise = arcCycles (Arc s e)

-- | Similar to 'fmap' but time is relative to the cycle (i.e. the
-- sam of the start of the arc)
mapCycle :: (Time -> Time) -> Arc -> Arc
mapCycle f (Arc s e) = Arc (sam' + f (s - sam')) (sam' + f (e - sam'))
         where sam' = sam s

-- | @isIn a t@ is @True@ if @t@ is inside
-- the arc represented by @a@.
isIn :: Arc -> Time -> Bool
isIn (Arc s e) t = t >= s && t < e

data Context = Context {contextPosition :: [((Int, Int), (Int, Int))]}
  deriving (Eq, Ord)

instance NFData Context where 
    rnf (Context c) = rnf c

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

-- | An event is a value that's active during a timespan. If a whole
-- is present, the part should be equal to or fit inside it.
data EventF a b = Event
  { context :: Context
  , whole :: Maybe a
  , part :: a
  , value :: b
  } deriving (Eq, Ord, Functor)

type Event a = EventF (ArcF Time) a

instance (NFData a, NFData b) => 
  NFData (EventF a b) where 
    rnf (Event c w p v) = rnf c `seq` rnf w `seq` rnf p `seq` rnf v

{-instance Bifunctor EventF where
  bimap f g (Event w p e) = Event (f w) (f p) (g e)
-}


isAnalog :: Event a -> Bool
isAnalog (Event {whole = Nothing}) = True
isAnalog _ = False

isDigital :: Event a -> Bool
isDigital = not . isAnalog

-- | `True` if an `Event`'s starts is within given `Arc`
onsetIn :: Arc -> Event a -> Bool
onsetIn a e = isIn a (wholeStart e)

-- | Compares two lists of events, attempting to combine fragmented events in the process
-- for a 'truer' compare
compareDefrag :: (Ord a) => [Event a] -> [Event a] -> Bool
compareDefrag as bs = sort (defragParts as) == sort (defragParts bs)

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
wholeOrPart e = part e

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

-- | an Arc and some named control values
data State = State {arc :: Arc,
                    controls :: StateMap
                   }

-- | A function that represents events taking place over time
type Query a = (State -> [Event a])

-- | A datatype that's basically a query
data Pattern a = Pattern {query :: Query a}

data Value = VS { svalue :: String }
           | VF { fvalue :: Double }
           | VR { rvalue :: Rational }
           | VI { ivalue :: Int }
           | VB { bvalue :: Bool }
           | VX { xvalue :: [Word8] } -- Used for OSC 'blobs'
           deriving (Typeable,Data)

class Valuable a where
  toValue :: a -> Value

instance NFData Value where 
  rnf (VS s) = rnf s 
  rnf (VF f) = rnf f 
  rnf (VR r) = rnf r 
  rnf (VI i) = rnf i 
  rnf (VB b) = rnf b
  rnf (VX xs) = rnf xs

instance Valuable String where
  toValue = VS
instance Valuable Double where
  toValue a = VF a
instance Valuable Rational where
  toValue a = VR a
instance Valuable Int where
  toValue a = VI a
instance Valuable Bool where
  toValue a = VB a
instance Valuable [Word8] where
  toValue a = VX a

instance Eq Value where
  (VS x) == (VS y) = x == y
  (VB x) == (VB y) = x == y
  (VF x) == (VF y) = x == y
  (VI x) == (VI y) = x == y
  (VR x) == (VR y) = x == y
  (VX x) == (VX y) = x == y
  
  (VF x) == (VI y) = x == (fromIntegral y)
  (VI y) == (VF x) = x == (fromIntegral y)

  (VF x) == (VR y) = (toRational x) == y
  (VR y) == (VF x) = (toRational x) == y
  (VI x) == (VR y) = (toRational x) == y
  (VR y) == (VI x) = (toRational x) == y

  _ == _ = False
  
instance Ord Value where
  compare (VS x) (VS y) = compare x y
  compare (VB x) (VB y) = compare x y
  compare (VF x) (VF y) = compare x y
  compare (VI x) (VI y) = compare x y
  compare (VR x) (VR y) = compare x y
  compare (VX x) (VX y) = compare x y
  compare (VS _) _ = LT
  compare _ (VS _) = GT
  compare (VB _) _ = LT
  compare _ (VB _) = GT
  compare (VX _) _ = LT
  compare _ (VX _) = GT
  compare (VF x) (VI y) = compare x (fromIntegral y)
  compare (VI x) (VF y) = compare (fromIntegral x) y

  compare (VR x) (VI y) = compare x (fromIntegral y)
  compare (VI x) (VR y) = compare (fromIntegral x) y

  compare (VF x) (VR y) = compare x (fromRational y)
  compare (VR x) (VF y) = compare (fromRational x) y

type StateMap = Map.Map String (Pattern Value)
type ControlMap = Map.Map String Value
type ControlPattern = Pattern ControlMap

------------------------------------------------------------------------
-- * Instances

instance NFData a => 
  NFData (Pattern a) where 
    rnf (Pattern q) = rnf $ \s -> q s

instance Functor Pattern where
  -- | apply a function to all the values in a pattern
  fmap f p = p {query = fmap (fmap f) . query p}

applyPatToPat :: (Maybe Arc -> Maybe Arc -> Maybe (Maybe Arc)) -> Pattern (a -> b) -> Pattern a -> Pattern b
applyPatToPat combineWholes pf px = Pattern q
    where q st = catMaybes $ concatMap match $ query pf st
            where
              match (ef@(Event (Context c) _ fPart f)) =
                map
                (\ex@(Event (Context c') _ xPart x) ->
                  do whole' <- combineWholes (whole ef) (whole ex)
                     part' <- subArc fPart xPart
                     return (Event (Context $ c ++ c') whole' part' (f x))
                )
                (query px $ st {arc = (wholeOrPart ef)})

instance Applicative Pattern where
  -- | Repeat the given value once per cycle, forever
  pure v = Pattern $ \(State a _) ->
    map (\a' -> Event (Context []) (Just a') (sect a a') v) $ cycleArcsInArc a

  (<*>) = applyPatToPatBoth

applyPatToPatBoth :: Pattern (a -> b) -> Pattern a -> Pattern b
applyPatToPatBoth pf px = Pattern q
    where q st = catMaybes $ (concatMap match $ query pf st) ++ (concatMap matchX $ query (filterAnalog px) st)
            where
              -- match analog events from pf with all events from px
              match ef@(Event _ Nothing fPart _)   = map (withFX ef) (query px $ st {arc = fPart}) -- analog
              -- match digital events from pf with digital events from px
              match ef@(Event _ (Just fWhole) _ _) = map (withFX ef) (query (filterDigital px) $ st {arc = fWhole}) -- digital
              -- match analog events from px (constrained above) with digital events from px
              matchX ex@(Event _ Nothing fPart _)  = map (\ef -> withFX ef ex) (query (filterDigital pf) $ st {arc = fPart}) -- digital
              matchX _ = error "can't happen"
              withFX ef ex = do whole' <- subMaybeArc (whole ef) (whole ex)
                                part' <- subArc (part ef) (part ex)
                                return (Event (combineContexts [context ef, context ex]) whole' part' (value ef $ value ex))

applyPatToPatLeft :: Pattern (a -> b) -> Pattern a -> Pattern b
applyPatToPatLeft pf px = Pattern q
    where q st = catMaybes $ (concatMap match $ query pf st)
            where
              match ef = map (withFX ef) (query px $ st {arc = wholeOrPart ef})
              withFX ef ex = do let whole' = whole ef
                                part' <- subArc (part ef) (part ex)
                                return (Event (combineContexts [context ef, context ex]) whole' part' (value ef $ value ex))

applyPatToPatRight :: Pattern (a -> b) -> Pattern a -> Pattern b
applyPatToPatRight pf px = Pattern q
    where q st = catMaybes $ (concatMap match $ query px st)
            where
              match ex = map (\ef -> withFX ef ex) (query pf $ st {arc = wholeOrPart ex})
              withFX ef ex = do let whole' = whole ex
                                part' <- subArc (part ef) (part ex)
                                return (Event (combineContexts [context ef, context ex]) whole' part' (value ef $ value ex))


-- | Like <*>, but the 'wholes' come from the left
(<*) :: Pattern (a -> b) -> Pattern a -> Pattern b
(<*) = applyPatToPatLeft

-- | Like <*>, but the 'wholes' come from the right
(*>) :: Pattern (a -> b) -> Pattern a -> Pattern b
(*>) = applyPatToPatRight

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

noOv :: String -> a
noOv meth = error $ meth ++ ": not supported for patterns"

class TolerantEq a where
   (~==) :: a -> a -> Bool

instance TolerantEq Value where
         (VS a) ~== (VS b) = a == b
         (VI a) ~== (VI b) = a == b
         (VR a) ~== (VR b) = a == b
         (VF a) ~== (VF b) = abs (a - b) < 0.000001
         _ ~== _ = False

instance TolerantEq ControlMap where
  a ~== b = Map.differenceWith (\a' b' -> if a' ~== b' then Nothing else Just a') a b == Map.empty

instance TolerantEq (Event ControlMap) where
  (Event _ w p x) ~== (Event _ w' p' x') = w == w' && p == p' && x ~== x'

instance TolerantEq a => TolerantEq [a] where
  as ~== bs = (length as == length bs) && all (uncurry (~==)) (zip as bs)


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
  fromInteger i = Map.singleton "n" $ VI $ fromInteger i
  signum      = (applyFIS signum signum id <$>)
  abs         = (applyFIS abs abs id <$>)

instance Fractional ControlMap where
  recip        = fmap (applyFIS recip id id)
  fromRational = Map.singleton "speed" . VF . fromRational

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

-- | @withEvent f p@ returns a new @Pattern@ with f applied to the resulting list of events for each query
-- function @f@.
withEvents :: ([Event a] -> [Event b]) -> Pattern a -> Pattern b
withEvents f p = p {query = f . query p}

-- | @withPart f p@ returns a new @Pattern@ with function @f@ applied
-- to the part.
withPart :: (Arc -> Arc) -> Pattern a -> Pattern a
withPart f = withEvent (\(Event c w p v) -> Event c w (f p) v)

-- | Apply one of three functions to a Value, depending on its type
applyFIS :: (Double -> Double) -> (Int -> Int) -> (String -> String) -> Value -> Value
applyFIS f _ _ (VF f') = VF $ f f'
applyFIS _ f _ (VI i ) = VI $ f i
applyFIS _ _ f (VS s ) = VS $ f s
applyFIS _ _ _ v = v

-- | Apply one of two functions to a Value, depending on its type (int
-- or float; strings and rationals are ignored)
fNum2 :: (Int -> Int -> Int) -> (Double -> Double -> Double) -> Value -> Value -> Value
fNum2 fInt _      (VI a) (VI b) = VI $ fInt a b
fNum2 _    fFloat (VF a) (VF b) = VF $ fFloat a b
fNum2 _    fFloat (VI a) (VF b) = VF $ fFloat (fromIntegral a) b
fNum2 _    fFloat (VF a) (VI b) = VF $ fFloat a (fromIntegral b)
fNum2 _    _      x      _      = x

getI :: Value -> Maybe Int
getI (VI i) = Just i
getI (VR x) = Just $ floor x
getI (VF x) = Just $ floor x
getI _  = Nothing

getF :: Value -> Maybe Double
getF (VF f) = Just f
getF (VR x) = Just $ fromRational x
getF (VI x) = Just $ fromIntegral x
getF _  = Nothing

getS :: Value -> Maybe String
getS (VS s) = Just s
getS _  = Nothing

getB :: Value -> Maybe Bool
getB (VB b) = Just b
getB _  = Nothing

getR :: Value -> Maybe Rational
getR (VR r) = Just r
getR (VF x) = Just $ toRational x
getR (VI x) = Just $ toRational x
getR _  = Nothing

getBlob :: Value -> Maybe [Word8]
getBlob (VX xs) = Just xs
getBlob _  = Nothing

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

-- ** Event filters

-- | Remove events from patterns that to not meet the given test
filterValues :: (a -> Bool) -> Pattern a -> Pattern a
filterValues f p = p {query = filter (f . value) . query p}

-- | Turns a pattern of 'Maybe' values into a pattern of values,
-- dropping the events of 'Nothing'.
filterJust :: Pattern (Maybe a) -> Pattern a
filterJust p = fromJust <$> filterValues isJust p

-- formerly known as playWhen
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

-- | Mark values in the first pattern which match with at least one
-- value in the second pattern.
matchManyToOne :: (b -> a -> Bool) -> Pattern a -> Pattern b -> Pattern (Bool, b)
matchManyToOne f pa pb = pa {query = q}
  where q st = map match $ query pb st
          where
            match (ex@(Event xContext xWhole xPart x)) =
              Event (combineContexts $ xContext:(map context as')) xWhole xPart (any (f x) (map value $ as'), x)
                where as' = as $ start $ wholeOrPart ex
            as s = query pa $ fQuery s
            fQuery s = st {arc = Arc s s}

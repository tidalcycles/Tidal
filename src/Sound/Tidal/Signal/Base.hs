{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- (c) Alex McLean 2023 and contributors
-- Shared under the terms of the GNU Public License v. 3.0

-- Base representation and instances for Signals, including
-- implementation of Pattern class, plus core definitions of waveforms
-- etc.

module Sound.Tidal.Signal.Base where

import           Control.Applicative      (liftA2)
import           Data.Bits                (Bits, testBit)
import           Data.Bool                (bool)
import           Data.Char                (ord)
import           Data.List                (groupBy, sort, (\\))
import qualified Data.Map.Strict          as Map
import           Data.Maybe               (fromMaybe, isJust, mapMaybe)
import           Data.Ratio

import           Sound.Tidal.Bjorklund    (bjorklund)
import           Sound.Tidal.Pattern
import           Sound.Tidal.Signal.Event
import           Sound.Tidal.Types
import           Sound.Tidal.Utils        (enumerate)
import           Sound.Tidal.Value

import           Prelude                  hiding ((*>), (<*))

-- ************************************************************ --
-- Signal

-- | A signal - a function from a timearc to a list of events active
-- during that timearc
-- This was known as a 'Pattern' in the previous version of Tidal. A
-- signal is a function from a timearc (possibly with some other
-- state) to events taking place in that timearc.

-- ************************************************************ --
-- Pattern instance

instance Pattern Signal where
  slowcat = sigSlowcat
  silence = sigSilence
  atom    = sigAtom
  stack   = sigStack
  _fast   = _sigFast
  _early  = _sigEarly
  rev     = sigRev
  timeCat = sigTimeCat
  when    = sigWhen
  _ply    = _sigPly
  _iter    = _sigIter
  _iterBack = _sigIterBack
  collect = sigCollect
  uncollect = sigUncollect
  _euclid = _sigEuclid
  innerJoin = sigInnerJoin
  (<*) = sigAppLeft
  (*>) = sigAppRight
  toSignal = id
  _pressBy = _sigPressBy
  _appAlign  = _sigAppAlign
  filterOnsets = filterEvents eventHasOnset
  filterValues f = filterEvents (f . value)
  withMetadata f pat = withEvents (map (\e -> e {metadata = f $ metadata e})) pat

_sigAppAlign :: (a -> Signal b -> Signal c) -> Align (Signal a) (Signal b) -> Signal c
_sigAppAlign f (Align SqueezeIn patt patv) = squeezeJoin $ (`f` patv) <$> patt
-- TODO - this one is wierd.. can it be simplified by defining squeezeOutJoin ?
_sigAppAlign f (Align SqueezeOut patt patv) =
  squeezeJoin $ _patternify f patt . pure <$> patv
_sigAppAlign f (Align CycleIn patt patv) = innerJoin $ (`f` patv) <$> patt
_sigAppAlign f (Align CycleOut patt patv) = outerJoin $ (`f` patv) <$> patt
_sigAppAlign f (Align CycleMix patt patv) = mixJoin $ (`f` patv) <$> patt
_sigAppAlign f (Align Trig patt patv) = trigJoin $ (`f` patv) <$> patt
_sigAppAlign f (Align TrigZero patt patv) = trigzeroJoin $ (`f` patv) <$> patt
_sigAppAlign _ (Align a _ _) = error $ "Alignment " ++ show a ++ " not implemented for signals"

-- ************************************************************ --

instance Applicative Signal where
  pure = atom -- TODO - would this be better as 'steady'?
  (<*>) = sigApp

-- | Apply a pattern of values to a pattern of functions, given a
-- function to merge the 'whole' timearcs
sigApp :: Signal (a -> b) -> Signal a -> Signal b
sigApp patf patv = Signal f
    where f s = concatMap (\ef -> mapMaybe (combine ef) $ query patv s) $ query patf s
          combine ef ev = do new_active <- maybeSect (active ef) (active ev)
                             return $ Event {metadata = metadata ef <> metadata ev,
                                             whole = liftA2 sect (whole ef) (whole ev),
                                             active = new_active,
                                             value = value ef $ value ev
                                            }

-- | Alternative definition of <*>, which takes the wholes from the
-- pattern of functions (unrelated to the <* in Prelude)
sigAppLeft :: Signal (a -> b) -> Signal a -> Signal b
sigAppLeft patf patv = Signal f
  where f s = concatMap (\ef -> mapMaybe (combine ef) $ query patv (s {sArc = wholeOrActive ef})
                        ) $ query patf s
        combine ef ev = do new_active <- maybeSect (active ef) (active ev)
                           return $ Event {metadata = metadata ef <> metadata ev,
                                           whole = whole ef,
                                           active = new_active,
                                           value = value ef $ value ev
                                          }

-- | Alternative definition of <*>, which takes the wholes from the
-- pattern of functions (unrelated to the <* in Prelude)
sigAppRight :: Signal (a -> b) -> Signal a -> Signal b
sigAppRight patf patv = Signal f
  where f s = concatMap (\ev -> mapMaybe (combine ev) $ query patf (s {sArc = wholeOrActive ev})
                        ) $ query patv s
        combine ev ef = do new_active <- maybeSect (active ef) (active ev)
                           return $ Event {metadata = metadata ef <> metadata ev,
                                           whole = whole ev,
                                           active = new_active,
                                           value = value ef $ value ev
                                          }

-- ************************************************************ --

instance Monoid (Signal a) where
  mempty = silence

instance Semigroup (Signal a) where
  (<>) !p !p' = Signal $ \st -> query p st ++ query p' st

-- ************************************************************ --

instance Monad Signal where
  (>>=) = bind

bind :: Signal a -> (a -> Signal b) -> Signal b
bind = bindWhole (liftA2 sect)

mixJoin :: Signal (Signal a) -> Signal a
mixJoin s = bind s id

innerBind :: Signal a -> (a -> Signal b) -> Signal b
innerBind = bindWhole (flip const)

sigInnerJoin :: Signal (Signal a) -> Signal a
sigInnerJoin s = innerBind s id

outerBind :: Signal a -> (a -> Signal b) -> Signal b
outerBind = bindWhole const

outerJoin :: Signal (Signal a) -> Signal a
outerJoin s = outerBind s id

bindWhole :: (Maybe Arc -> Maybe Arc -> Maybe Arc) -> Signal a -> (a -> Signal b) -> Signal b
bindWhole chooseWhole pv f = Signal $ \state -> concatMap (match state) $ query pv state
  where match state event = map (withWhole event) $ query (f $ value event) (state {sArc = active event})
        withWhole event event' = event' {whole = chooseWhole (whole event) (whole event'),
                                         metadata = metadata event <> metadata event'
                                        }

-- | Like @join@, but cycles of the inner patterns are compressed to fit the
-- timearc of the outer whole (or the original query if it's a continuous pattern?)
-- TODO - what if a continuous pattern contains a discrete one, or vice-versa?
squeezeJoin :: Signal (Signal a) -> Signal a
squeezeJoin pp = pp {query = q}
  where q st = concatMap
          (\e@(Event m w p v) ->
             mapMaybe (munge m w p) $ query (_focusArc (wholeOrActive e) v) st {sArc = p}
          )
          (query pp st)
        munge oMetadata oWhole oPart (Event iMetadata iWhole iPart v) =
          do w' <- maybeSect <$> oWhole <*> iWhole
             p' <- maybeSect oPart iPart
             return (Event (iMetadata <> oMetadata) w' p' v)

squeezeBind :: Signal a -> (a -> Signal b) -> Signal b
squeezeBind pat f = squeezeJoin $ fmap f pat

-- Flatterns patterns of patterns, by retriggering/resetting inner patterns at onsets of outer pattern haps

_trigTimeJoin :: (Time -> Time) -> Signal (Signal a) -> Signal a
_trigTimeJoin timeF patOfPats = Signal $ \state -> concatMap (queryInner state) $ query (discreteOnly patOfPats) state
  where queryInner state outerEvent
          = mapMaybe (\innerEvent -> do a <- maybeSect (active innerEvent) (active outerEvent)
                                        return $ Event {metadata = metadata innerEvent <> metadata outerEvent,
                                                        whole = sect <$> whole innerEvent <*> whole outerEvent,
                                                        active = a,
                                                        value = value innerEvent
                                                       }
                ) $ query (_late (timeF $ aBegin $ wholeOrActive outerEvent) (value outerEvent)) state

trigJoin :: Signal (Signal a) -> Signal a
trigJoin = _trigTimeJoin cyclePos

trigzeroJoin :: Signal (Signal a) -> Signal a
trigzeroJoin = _trigTimeJoin id

-- ************************************************************ --
-- Signals as numbers

noOv :: String -> a
noOv meth = error $ meth ++ ": not supported for signals"

instance Eq (Signal a) where
  (==) = noOv "(==)"

instance Ord a => Ord (Signal a) where
  min = liftA2 min
  max = liftA2 max
  compare = noOv "compare"
  (<=) = noOv "(<=)"

instance Num a => Num (Signal a) where
  negate      = fmap negate
  (+)         = liftA2 (+)
  (*)         = liftA2 (*)
  fromInteger = pure . fromInteger
  abs         = fmap abs
  signum      = fmap signum

instance Enum a => Enum (Signal a) where
  succ           = fmap succ
  pred           = fmap pred
  toEnum         = pure . toEnum
  fromEnum       = noOv "fromEnum"
  enumFrom       = noOv "enumFrom"
  enumFromThen   = noOv "enumFromThen"
  enumFromTo     = noOv "enumFromTo"
  enumFromThenTo = noOv "enumFromThenTo"

instance (Num a, Ord a) => Real (Signal a) where
  toRational = noOv "toRational"

instance (Integral a) => Integral (Signal a) where
  quot          = liftA2 quot
  rem           = liftA2 rem
  div           = liftA2 div
  mod           = liftA2 mod
  toInteger     = noOv "toInteger"
  x `quotRem` y = (x `quot` y, x `rem` y)
  x `divMod`  y = (x `div`  y, x `mod` y)

instance (Fractional a) => Fractional (Signal a) where
  recip        = fmap recip
  fromRational = pure . fromRational

instance (Floating a) => Floating (Signal a) where
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

instance (RealFrac a) => RealFrac (Signal a) where
  properFraction = noOv "properFraction"
  truncate       = noOv "truncate"
  round          = noOv "round"
  ceiling        = noOv "ceiling"
  floor          = noOv "floor"

instance (RealFloat a) => RealFloat (Signal a) where
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
  negate      = (applyFIRS negate negate negate id <$>)
  (+)         = Map.unionWith (fNum2 (+) (+))
  (*)         = Map.unionWith (fNum2 (*) (*))
  fromInteger i = Map.singleton "n" $ VI (fromInteger i)
  signum      = (applyFIRS signum signum signum id <$>)
  abs         = (applyFIRS abs abs abs id <$>)

instance Fractional ValueMap where
  recip        = fmap (applyFIRS recip id recip id)
  fromRational r = Map.singleton "speed" $ VF (fromRational r)

-- ************************************************************ --
-- General hacks and utilities

instance Show (a -> b) where
  show _ = "<function>"

filterEvents :: (Event a -> Bool) -> Signal a -> Signal a
filterEvents f pat = Signal $ \state -> filter f $ query pat state

filterTime :: (Time -> Bool) -> Signal a -> Signal a
filterTime test p = p {query = filter (test . aBegin . wholeOrActive) . query p}

discreteOnly :: Signal a -> Signal a
discreteOnly = filterEvents $ isJust . whole

playFor :: Time -> Time -> Signal a -> Signal a
playFor s e pat = Signal $ \st -> maybe [] (\a -> query pat (st {sArc = a})) $ maybeSect (Arc s e) (sArc st)

-- A hack to add to manipulate source code to add calls to
-- 'deltaContext' around strings, so events from mininotation know
-- where they are within a whole tidal pattern
deltaMini :: String -> String
deltaMini = outside 0 0
  where outside :: Int -> Int -> String -> String
        outside _ _ [] = []
        outside column line ('"':xs) = "(deltaMetadata "
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
  deltaMetadata :: Int -> Int -> a -> a

instance Stringy (Signal a) where
  deltaMetadata column line pat = withEvents (map (\e -> e {metadata = f $ metadata e})) pat
    where f :: Metadata -> Metadata
          f (Metadata xs) = Metadata $ map (\((bx,by), (ex,ey)) -> ((bx+column,by+line), (ex+column,ey+line))) xs

instance Stringy (Sequence a) where
  deltaMetadata column line pat = withAtom (\a -> a {atomMetadata = f $ atomMetadata a}) pat
    where f :: Metadata -> Metadata
          f (Metadata xs) = Metadata $ map (\((bx,by), (ex,ey)) -> ((bx+column,by+line), (ex+column,ey+line))) xs

-- deltaMetadata on an actual (non overloaded) string is a no-op
instance Stringy String where
  deltaMetadata _ _ = id

-- ************************************************************ --
-- Basic time/event manipulations

queryArc :: Signal a -> Arc -> [Event a]
queryArc sig arc = query sig (State arc Map.empty)

withEventArc :: (Arc -> Arc) -> Signal a -> Signal a
withEventArc arcf sig = Signal f
  where f s = map (\e -> e {active = arcf $ active e,
                            whole = arcf <$> whole e
                           }) $ query sig s

withEventTime :: (Time -> Time) -> Signal a -> Signal a
withEventTime timef sig = Signal f
  where f s = map (\e -> e {active = withArcTime timef $ active e,
                            whole = withArcTime timef <$> whole e
                           }) $ query sig s

withArcTime :: (Time -> Time) -> Arc -> Arc
withArcTime timef (Arc b e) = Arc (timef b) (timef e)

withQuery :: (State -> State) -> Signal a -> Signal a
withQuery statef sig = Signal $ \state -> query sig $ statef state

withQueryMaybe :: (State -> Maybe State) -> Signal a -> Signal a
withQueryMaybe statef sig = Signal $ \state -> fromMaybe [] $ do state' <- statef state
                                                                 return $ query sig state'

withQueryArc :: (Arc -> Arc) -> Signal a -> Signal a
withQueryArc arcf = withQuery (\state -> state {sArc = arcf $ sArc state})

withQueryArcMaybe :: (Arc -> Maybe Arc) -> Signal a -> Signal a
withQueryArcMaybe arcf = withQueryMaybe (\state -> do a <- arcf $ sArc state
                                                      return $ state {sArc = a}
                                        )

withQueryTime :: (Time -> Time) -> Signal a -> Signal a
withQueryTime timef = withQueryArc (withArcTime timef)

-- | Apply a function to the control values of the query
withQueryControls :: (ValueMap -> ValueMap) -> Signal a -> Signal a
withQueryControls f pat = pat { query = query pat . (\(State a m) -> State a (f m))}


-- | @withEvents f p@ returns a new @Signal@ with f applied to the resulting list of events for each query
-- function @f@.
withEvents :: ([Event a] -> [Event b]) -> Signal a -> Signal b
withEvents f p = p {query = f . query p}

-- | @withEvent f p@ returns a new @Signal@ with f applied to each event queried
-- function @f@.
withEvent :: (Event a -> Event b) -> Signal a -> Signal b
withEvent f = withEvents (map f)

-- ************************************************************ --
-- Fundamental signals

sigSilence :: Signal a
sigSilence = Signal $ const []

-- | Repeat discrete value once per cycle
sigAtom :: a -> Signal a
sigAtom v = Signal $ \state -> map
                               (\arc -> Event {metadata = mempty,
                                               whole = Just $ timeToCycleArc $ aBegin arc,
                                               active = arc,
                                               value = v
                                              }
                               )
                               (splitArcs $ sArc state)

-- See also - `steady` in Sound.Tidal.Signal.Waveform

-- ************************************************************ --
-- Signal manipulations

-- | Split queries at sample boundaries. An internal function that
-- makes other functions easier to define, as events that cross cycle
-- boundaries don't need to be considered then.
splitQueries :: Signal a -> Signal a
splitQueries pat = Signal $ \state -> concatMap (\arc -> query pat (state {sArc = arc}))
                                       $ splitArcs $ sArc state

-- | Concatenate a list of signals, interleaving cycles.
sigSlowcat :: [Signal a] -> Signal a
sigSlowcat pats = splitQueries $ Signal queryCycle
  where queryCycle state = query (_late (offset $ sArc state) (pat $ sArc state)) state
        pat arc = pats !! mod (floor $ aBegin arc) n
        offset arc = sam (aBegin arc) - sam (aBegin arc / toRational n)
        n = length pats

_sigFast :: Time -> Signal a -> Signal a
_sigFast 0 _   = silence
_sigFast t pat = withEventTime (/t) $ withQueryTime (*t) pat

_fastGap :: Time -> Signal a -> Signal a
_fastGap factor pat = splitQueries $ withEvent ef $ withQueryArcMaybe qf pat
  -- A bit fiddly, to drop zero-width queries at the start of the next cycle
  where qf (Arc b e) | bpos < 1 = Just $ Arc (cyc + bpos) (cyc + epos)
                     | otherwise = Nothing
          where cyc = sam b
                bpos = min 1 $ (b - cyc) * factor
                epos = min 1 $ (e - cyc) * factor
        -- Also fiddly, to maintain the right 'whole' relative to the part
        ef ev = ev {whole = w', active = a'}
          where a = active ev
                b = aBegin a
                e = aEnd a
                a' = Arc (cyc + bpos) (cyc + epos)
                  where cyc = sam b
                        bpos = min 1 $ (b - cyc) / factor
                        epos = min 1 $ (e - cyc) / factor
                w' = do w <- whole ev
                        let b' = aBegin a' - ((b - aBegin w) / factor)
                            e' = aEnd a' + ((aEnd w - e) / factor)
                        return $ Arc b' e'

-- | Like @fast@, but only plays one cycle of the original signal
-- once per cycle, leaving a gap at the end
fastGap :: Signal Time -> Signal a -> Signal a
fastGap = _patternify _fastGap

fastGapA :: Align (Signal Time) (Signal a) -> Signal a
fastGapA = _appAlign _fastGap

_compressArc :: Arc -> Signal a -> Signal a
_compressArc (Arc b e) pat | b > e || b > 1 || e > 1 || b < 0 || e < 0 = silence
                           | otherwise = _late b $ _fastGap (1/(e-b)) pat

_compressArcTo :: Arc -> Signal a -> Signal a
_compressArcTo (Arc b e) = _compressArc (Arc (cyclePos b) (e - sam b))

-- | Like @fastGap@, but takes the start and duration of the arc to compress the cycle into.
compress :: Signal Time -> Signal Time -> Signal a -> Signal a
compress patStart patDur pat = innerJoin $ (\s d -> _compressArc (Arc s (s+d)) pat) <$> patStart <*> patDur

_focusArc :: Arc -> Signal a -> Signal a
_focusArc (Arc b e) pat = _late (cyclePos b) $ _fast (1/(e-b)) pat

-- | Like @compress@, but doesn't leave a gap and can 'focus' on any arc (not just within a cycle)
focus :: Signal Time -> Signal Time -> Signal a -> Signal a
focus patStart patDur pat = innerJoin $ (\s d -> _focusArc (Arc s (s+d)) pat) <$> patStart <*> patDur

_sigEarly :: Time -> Signal a -> Signal a
_sigEarly t pat = withEventTime (subtract t) $ withQueryTime (+ t) pat

earlyA :: Align (Signal Time) (Signal a) -> Signal a
earlyA = _appAlign _early

lateA :: Align (Signal Time) (Signal a) -> Signal a
lateA = _appAlign _late

-- | Infix operator for @late@
(~>) :: Signal Time -> Signal x -> Signal x
(~>) = late

{- | Plays a portion of a signal, specified by start and duration
The new resulting signal is played over the time period of the original signal:

@
d1 $ zoom 0.25 0.75 $ sound "bd*2 hh*3 [sn bd]*2 drum"
@

In the signal above, `zoom` is used with an arc from 25% to 75%. It is equivalent to this signal:

@
d1 $ sound "hh*3 [sn bd]*2"
@
-}
zoom :: Signal Time -> Signal Time -> Signal a -> Signal a
zoom patStart patDur pat = innerJoin $ (\s d -> _zoomArc (Arc s (s+d)) pat) <$> patStart <*> patDur

_zoomArc :: Arc -> Signal a -> Signal a
_zoomArc (Arc s e) p = splitQueries $
  withEventArc (mapCycle ((/d) . subtract s)) $ withQueryArc (mapCycle ((+s) . (*d))) p
     where d = e-s

repeatCycles :: Signal Int -> Signal a -> Signal a
repeatCycles = _patternify _repeatCycles

repeatCyclesA :: Align (Signal Int) (Signal a) -> Signal a
repeatCyclesA = _appAlign _repeatCycles

_repeatCycles :: Int -> Signal a -> Signal a
_repeatCycles n p = slowcat $ replicate n p

-- TODO - no slowRepeatcycles?
fastRepeatCycles :: Signal Int -> Signal a -> Signal a
fastRepeatCycles = _patternify _repeatCycles

_fastRepeatCycles :: Int -> Signal a -> Signal a
_fastRepeatCycles n p = fastcat $ replicate n p

sigStack :: [Signal a] -> Signal a
sigStack pats = Signal $ \s -> concatMap (`query` s) pats

off :: Signal Time -> (Signal a -> Signal a) -> Signal a -> Signal a
off tp f p = innerJoin $ (\tv -> _off tv f p) <$> tp

_off :: Time -> (Signal a -> Signal a) -> Signal a -> Signal a
_off t f p = superimpose (f . (t `_late`)) p

squash :: Time -> Signal a -> Signal a
squash into pat = splitQueries $ withEventArc ef $ withQueryArc qf pat
  where qf (Arc s e) = Arc (sam s + min 1 ((s - sam s) / into)) (sam s + min 1 ((e - sam s) / into))
        ef (Arc s e) = Arc (sam s + (s - sam s) * into) (sam s + (e - sam s) * into)

squashTo :: Time -> Time -> Signal a -> Signal a
squashTo b e = _late b . squash (e-b)

{- @bite@ n ipat pat |
  slices a signal `pat` into `n` pieces, then uses the `ipat` signal of integers to index into those slices.
  So `bite 4 "0 2*2" (run 8)` is the same as `"[0 1] [4 5]*2"`.
-}
bite :: Signal Int -> Signal Int -> Signal a -> Signal a
bite npat ipat pat = innerJoin $ (\n -> _bite n ipat pat) <$> npat

_bite :: Int -> Signal Int -> Signal a -> Signal a
_bite n ipat pat = squeezeJoin $ zoompat <$> ipat
  where zoompat i = _zoomArc (Arc (i' / fromIntegral n) ((i'+1) / fromIntegral n)) pat
           where i' = fromIntegral $ i `mod` n

sigRev :: Signal a -> Signal a
sigRev pat = splitQueries $ Signal f
  where f state = withArc reflect <$> (query pat $ state {sArc = reflect $ sArc state})
          where cyc = sam $ aBegin $ sArc state
                next_cyc = nextSam cyc
                reflect (Arc b e) = Arc (cyc + (next_cyc - e)) (cyc + (next_cyc - b))

-- | Similar to @fastCat@, but each signal is given a relative duration
sigTimeCat :: [(Time, Signal a)] -> Signal a
sigTimeCat tps = stack $ map (\(s,e,p) -> _compressArc (Arc (s/total) (e/total)) p) $ arrange 0 tps
    where total = sum $ map fst tps
          arrange :: Time -> [(Time, Signal a)] -> [(Time, Time, Signal a)]
          arrange _ []            = []
          arrange t ((t',p):tps') = (t,t+t',p) : arrange (t+t') tps'

sigWhen :: Signal Bool -> (Signal b -> Signal b) -> Signal b -> Signal b
sigWhen boolpat f pat = innerJoin $ (\b -> if b then f pat else pat) <$> boolpat


{-|
Only `when` the given test function returns `True` the given signal
transformation is applied. The test function will be called with the
current cycle as a number.

@
d1 $ whenT ((elem '4').show)
  (striate 4)
  $ sound "hh hc"
@

The above will only apply `striate 4` to the signal if the current
cycle number contains the number 4. So the fourth cycle will be
striated and the fourteenth and so on. Expect lots of striates after
cycle number 399.
-}
whenT :: (Int -> Bool) -> (Signal a -> Signal a) ->  Signal a -> Signal a
whenT test f p = splitQueries $ p {query = apply}
  where apply st | test (floor $ aBegin $ sArc st) = query (f p) st
                 | otherwise = query p st


_sigPly :: Time -> Signal a -> Signal a
_sigPly t pat = squeezeJoin $ _fast t . atom <$> pat

-- | @segment n p@: 'samples' the signal @p@ at a rate of @n@
-- events per cycle. Useful for turning a continuous signal into a
-- discrete one.
segment :: Signal Time -> Signal a -> Signal a
segment = _patternify _segment

_segment :: Time -> Signal a -> Signal a
_segment n p = _fast n (atom id) <* p

segmentA :: Align (Signal Time) (Signal a) -> Signal a
segmentA = _appAlign _segment

--- functions relating to chords/patterns of lists

_sameDur :: Event a -> Event a -> Bool
_sameDur e1 e2 = (whole e1 == whole e2) && (active e1 == active e2)

_groupEventsBy :: Eq a => (Event a -> Event a -> Bool) -> [Event a] -> [[Event a]]
_groupEventsBy _ [] = []
_groupEventsBy f (e:es) = eqs : _groupEventsBy f (es \\ eqs)
  where eqs = e:[x | x <- es, f e x]

-- assumes that all events in the list have same whole/active
_collectEvent :: [Event a] -> Maybe (Event [a])
_collectEvent [] = Nothing
_collectEvent l@(e:_) = Just $ e {metadata = con, value = vs}
  where con = unionC $ map metadata l
        vs = map value l
        unionC [] = Metadata []
        unionC ((Metadata is):cs) = Metadata (is ++ iss)
          where Metadata iss = unionC cs

_collectEventsBy :: Eq a => (Event a -> Event a -> Bool) -> [Event a] -> [Event [a]]
_collectEventsBy f es = remNo $ map _collectEvent (_groupEventsBy f es)
  where
    remNo []            = []
    remNo (Nothing:cs)  = remNo cs
    remNo ((Just c):cs) = c : remNo cs

-- | collects all events satisfying the same constraint into a list
_collectBy :: Eq a => (Event a -> Event a -> Bool) -> Signal a -> Signal [a]
_collectBy f = withEvents (_collectEventsBy f)

-- | collects all events occuring at the exact same time into a list
sigCollect :: Eq a => Signal a -> Signal [a]
sigCollect = _collectBy _sameDur

_uncollectEvent :: Event [a] -> [Event a]
_uncollectEvent e = [e {value = value e !! i, metadata = resolveMetadata i (metadata e)} | i <-[0..length (value e) - 1]]
  where resolveMetadata i (Metadata xs) = if length xs <= i
                                            then Metadata []
                                            else Metadata [xs!!i]

_uncollectEvents :: [Event [a]] -> [Event a]
_uncollectEvents = concatMap _uncollectEvent

-- | merges all values in a list into one pattern by stacking the values
sigUncollect :: Signal [a] -> Signal a
sigUncollect = withEvents _uncollectEvents

_sigPressBy :: Time -> Signal a -> Signal a
_sigPressBy r pat = squeezeJoin $ _compressArcTo (Arc r 1) . atom <$> pat

{- | Make a pattern sound a bit like a breakbeat, by playing it twice as
   fast and shifted by a quarter of a cycle, every other cycle.

Example:

@
d1 $ sound (brak "bd sd")
@
-}
brak :: Signal a -> Signal a
brak = whenT ((== 1) . (`mod` 2)) (((1%4) `_late`) . (\x -> fastcat [x, silence]))


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

There is also `iterBack`, which shifts the pattern in the opposite direction.

-}
_sigIter :: Int -> Signal a -> Signal a
_sigIter n p = slowcat $ map (\i -> (fromIntegral i % fromIntegral n) `_early` p) [0 .. (n-1)]

-- | @iterBack@ is the same as @iter@, but decrements the starting
-- subdivision instead of incrementing it.
_sigIterBack :: Int -> Signal a -> Signal a
_sigIterBack n p = slowcat $ map (\i -> (fromIntegral i % fromIntegral n) `_late` p) [0 .. (n-1)]

{- | @trunc@ truncates a signal so that only a fraction of the signal is played.
The following example plays only the first quarter of the signal:

@
d1 $ trunc 0.25 $ sound "bd sn*2 cp hh*4 arpy bd*2 cp bd*2"
@
-}
trunc :: Signal Time -> Signal a -> Signal a
trunc = _patternify _trunc

truncA :: Align (Signal Time) (Signal a) -> Signal a
truncA = _appAlign _trunc

_trunc :: Time -> Signal a -> Signal a
_trunc t = _compressArc (Arc 0 t) . _zoomArc (Arc 0 t)

-- | @rot n p@ rotates the values in a signal @p@ by @n@ beats to the left.
-- Example: @d1 $ every 4 (rot 2) $ slow 2 $ sound "bd hh hh hh"@
rot :: Ord a => Signal Int -> Signal a -> Signal a
rot = _patternify _rot

rotA :: Ord a => Align (Signal Int) (Signal a) -> Signal a
rotA = _appAlign _rot

-- Calculates a whole cycle, rotates it, then constrains events to the original query arc
_rot :: Ord a => Int -> Signal a -> Signal a
_rot i pat = splitQueries $ pat {query = \st -> f st (query pat (st {sArc = wholeCycle (sArc st)}))}
  where -- TODO maybe events with the same arc (active+whole) should be
        -- grouped together in the rotation?
        f st es = constrainEvents (sArc st) $ shiftValues $ sort $ defragActives es
        shiftValues es | i >= 0 =
                         zipWith (\e s -> e {value = s}) es
                         (drop i $ cycle $ map value es)
                       | otherwise =
                         zipWith (\e s -> e{value = s}) es
                         (drop (length es - abs i) $ cycle $ map value es)
        wholeCycle (Arc s _) = Arc (sam s) (nextSam s)
        constrainEvents :: Arc -> [Event a] -> [Event a]
        constrainEvents a es = mapMaybe (constrainEvent a) es
        constrainEvent :: Arc -> Event a -> Maybe (Event a)
        constrainEvent a e =
          do
            p' <- maybeSect (active e) a
            return e {active = p'}


{- | @linger@ is similar to `trunc` but the truncated part of the signal loops until the end of the cycle.

@
d1 $ linger 0.25 $ sound "bd sn*2 cp hh*4 arpy bd*2 cp bd*2"
@

If you give it a negative number, it will linger on the last part of
the signal, instead of the start of it. E.g. to linger on the last
quarter:

@
d1 $ linger (-0.25) $ sound "bd sn*2 cp hh*4 arpy bd*2 cp bd*2"
@
-}
linger :: Signal Time -> Signal a -> Signal a
linger = _patternify _linger

lingerA :: Align (Signal Time) (Signal a) -> Signal a
lingerA = _appAlign _linger

_linger :: Time -> Signal a -> Signal a
_linger n p | n < 0 = _fast (1/n) $ _zoomArc (Arc (1 + n) 1) p
            | otherwise = _fast (1/n) $ _zoomArc (Arc 0 n) p


{-|
  Treats the given signal @p@ as having @n@ chunks, and applies the function @f@ to one of those sections per cycle.
  Running:
   - from left to right if chunk number is positive
   - from right to left if chunk number is negative

  @
  d1 $ chunk 4 (fast 4) $ sound "cp sn arpy [mt lt]"
  @
-}
chunk :: Signal Int -> (Signal b -> Signal b) -> Signal b -> Signal b
chunk npat f p = innerJoin $ (\n -> _chunk n f p) <$> npat

_chunk :: Int -> (Signal b -> Signal b) -> Signal b -> Signal b
_chunk n f p | n == 0 = p
             | n > 0 = when (_iterBack n $ fastcat (map pure $ True:replicate (n-1) False)) f p
             | otherwise = when (_iter (abs n) $ fastcat (map pure $ replicate (abs n-1) False ++ [True])) f p

-- | Repeats the first cycle forever
loopFirst :: Signal a -> Signal a
loopFirst pat = trigzeroJoin $ pure pat

-- | Repeats the first given number of cycles forever. Previously known as `timeLoop`.
loopCycles :: Signal Time -> Signal a -> Signal a
loopCycles n = outside n loopFirst

{- | `rolled` plays each note of a chord quickly in order, as opposed to simultaneously; to give a chord a harp-like effect.
This will played from the lowest note to the highest note of the chord
@
rolled $ n "c'maj'4" # s "superpiano"
@


And you can use `rolledBy` or `rolledBy'` to specify the length of the roll. The value in the passed pattern
is the divisor of the cycle length. A negative value will play the arpeggio in reverse order.

@
rolledBy "<1 -0.5 0.25 -0.125>" $ note "c'maj9" # s "superpiano"
@
-}

rolledWith :: Ratio Integer -> Signal a -> Signal a
rolledWith t = withEvents aux
         where aux es = concatMap steppityIn (groupBy (\a b -> whole a == whole b) $ isRev t es)
               isRev b = (\x -> if x > 0 then id else reverse ) b
               steppityIn xs = mapMaybe (\(n, ev) -> timeguard n xs ev t) $ enumerate xs
               timeguard _ _ ev 0  = return ev
               timeguard n xs ev _ = shiftIt n (length xs) ev
               shiftIt n d (Event c (Just (Arc s e)) a' v) = do
                         a'' <- maybeSect (Arc newS e) a'
                         return (Event c (Just $ Arc newS e) a'' v)
                      where newS = s + (dur * fromIntegral n)
                            dur = (e - s) / ((1 / abs t)*fromIntegral d)
               shiftIt _ _ ev =  return ev

rolledBy :: Signal (Ratio Integer) -> Signal a -> Signal a
rolledBy pt = _patternify rolledWith $ _segment 1 pt

rolled :: Signal a -> Signal a
rolled = rolledBy (1/4)

{-
  snowball |
  snowball takes a function that can combine patterns (like '+'),
  a function that transforms a pattern (like 'slow'),
  a depth, and a starting pattern,
  it will then transform the pattern and combine it with the last transformation until the depth is reached
  this is like putting an effect (like a filter) in the feedback of a delay line
  each echo is more effected
  d1 $ note (scale "hexDorian" $ snowball (+) (slow 2 . rev) 8 "0 ~ . -1 . 5 3 4 . ~ -2") # s "gtr"
-}
snowball :: Int -> (Signal a -> Signal a -> Signal a) -> (Signal a -> Signal a) -> Signal a -> Signal a
snowball depth combinationFunction f signal = cat $ take depth $ scanl combinationFunction signal $ drop 1 $ iterate f signal

{- @soak@ |
    applies a function to a signal and cats the resulting signal,
    then continues applying the function until the depth is reached
    this can be used to create a signal that wanders away from
    the original signal by continually adding random numbers
    d1 $ note (scale "hexDorian" mutateBy (+ (range -1 1 $ irand 2)) 8 $ "0 1 . 2 3 4") # s "gtr"
-}
soak ::  Int -> (Signal a -> Signal a) -> Signal a -> Signal a
soak depth f signal = cat $ take depth $ iterate f signal

-- | limit values in a Pattern (or other Functor) to n equally spaced
-- divisions of 1.
-- TODO move to Pattern ?
quantise :: (Functor f, RealFrac b) => b -> f b -> f b
quantise n = fmap ((/n) . (fromIntegral :: RealFrac b => Int -> b) . round . (*n))

-- quantise but with floor
qfloor :: (Functor f, RealFrac b) => b -> f b -> f b
qfloor n = fmap ((/n) . (fromIntegral :: RealFrac b => Int -> b) . floor . (*n))

qceiling :: (Functor f, RealFrac b) => b -> f b -> f b
qceiling n = fmap ((/n) . (fromIntegral :: RealFrac b => Int -> b) . ceiling . (*n))

qround :: (Functor f, RealFrac b) => b -> f b -> f b
qround = quantise

-- ************************************************************ --
-- Binary signals

__binary :: Data.Bits.Bits b => Int -> b -> [Bool]
__binary n num = map (testBit num) $ reverse [0 .. n-1]

_binary :: Data.Bits.Bits b => Int -> b -> Signal Bool
_binary n num = fastFromList $ __binary n num

_binaryN :: Int -> Signal Int -> Signal Bool
_binaryN n p = squeezeJoin $ _binary n <$> p

binaryN :: Signal Int -> Signal Int -> Signal Bool
binaryN n p = _patternify _binaryN n p

binary :: Signal Int -> Signal Bool
binary = binaryN 8

ascii :: Signal String -> Signal Bool
ascii p = squeezeJoin $ fastFromList . concatMap (__binary 8 . ord) <$> p

-- | For specifying a boolean pattern according to a list of offsets
-- (aka inter-onset intervals).  For example `necklace 12 [4,2]` is
-- the same as "t f f f t f t f f f t f". That is, 12 steps per cycle,
-- with true values alternating between every 4 and every 2 steps.
necklace :: Rational -> [Int] -> Signal Bool
necklace perCycle xs = _slow (toRational (sum xs) / perCycle) $ fastFromList $ list xs
  where list :: [Int] -> [Bool]
        list []      = []
        list (x:xs') = (True : replicate (x-1) False) ++ list xs'

-- | Inverts all the values in a boolean pattern (or other functor)
-- TODO move to Pattern ?
inv :: Functor f => f Bool -> f Bool
inv = (not <$>)

-- ************************************************************ --
-- Euclidean / diaspora algorithm

{- | You can use the @e@ function to apply a Euclidean algorithm over a
complex pattern, although the structure of that pattern will be lost:

@
d1 $ e 3 8 $ sound "bd*2 [sn cp]"
@

In the above, three sounds are picked from the pattern on the right according
to the structure given by the `e 3 8`. It ends up picking two `bd` sounds, a
`cp` and missing the `sn` entirely.

A negative first argument provides the inverse of the euclidean pattern.

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

_sigEuclid :: Int -> Int -> Signal a -> Signal a
_sigEuclid n k a | n >= 0 = fastcat $ fmap (bool silence a) $ bjorklund (n,k)
                 | otherwise = fastcat $ fmap (bool a silence) $ bjorklund (-n,k)

_euclid' :: Int -> Int -> Signal a -> Signal a
_euclid' n k p = fastcat $ map (\x -> if x then p else silence) (bjorklund (n,k))

distrib :: [Signal Int] -> Signal a -> Signal a
distrib ps p = do p' <- sequence ps
                  _distrib p' p

_distrib :: [Int] -> Signal a -> Signal a
_distrib xs p = boolsToPat (foldr distrib' (replicate (last xs) True) (reverse $ layers xs)) p
  where
    distrib' :: [Bool] -> [Bool] -> [Bool]
    distrib' [] _           = []
    distrib' (_:a) []       = False : distrib' a []
    distrib' (True:a) (x:b) = x : distrib' a b
    distrib' (False:a) b    = False : distrib' a b
    layers = map bjorklund . (zip<*>tail)
    boolsToPat a b' = flip const <$> filterValues (== True) (fastFromList a) <*> b'

-- ************************************************************ --
-- Functions for getting control input as signals

valueToSignal :: Value -> Signal Value
valueToSignal (VSignal pat) = pat
valueToSignal v             = pure v

_getP_ :: (Value -> Maybe a) -> Signal Value -> Signal a
_getP_ f pat = filterJusts $ f <$> pat

_getP :: a -> (Value -> Maybe a) -> Signal Value -> Signal a
_getP d f pat = fromMaybe d . f <$> pat

_cX :: a -> (Value -> Maybe a) -> String -> Signal a
_cX d f s = Signal $ \(State a m) -> queryArc (maybe (pure d) (_getP d f . valueToSignal) $ Map.lookup s m) a

_cX_ :: (Value -> Maybe a) -> String -> Signal a
_cX_ f s = Signal $ \(State a m) -> queryArc (maybe silence (_getP_ f . valueToSignal) $ Map.lookup s m) a

cF :: Double -> String -> Signal Double
cF d = _cX d getF
cF_ :: String -> Signal Double
cF_ = _cX_ getF
cF0 :: String -> Signal Double
cF0 = _cX 0 getF

cN :: Note -> String -> Signal Note
cN d = _cX d getN
cN_ :: String -> Signal Note
cN_ = _cX_ getN
cN0 :: String -> Signal Note
cN0 = _cX (Note 0) getN

cI :: Int -> String -> Signal Int
cI d = _cX d getI
cI_ :: String -> Signal Int
cI_ = _cX_ getI
cI0 :: String -> Signal Int
cI0 = _cX 0 getI

cB :: Bool -> String -> Signal Bool
cB d = _cX d getB
cB_ :: String -> Signal Bool
cB_ = _cX_ getB
cB0 :: String -> Signal Bool
cB0 = _cX False getB

cR :: Rational -> String -> Signal Rational
cR d = _cX d getR
cR_ :: String -> Signal Rational
cR_ = _cX_ getR
cR0 :: String -> Signal Rational
cR0 = _cX 0 getR

cT :: Time -> String -> Signal Time
cT = cR
cT0 :: String -> Signal Time
cT0 = cR0
cT_ :: String -> Signal Time
cT_ = cR_

cS :: String -> String -> Signal String
cS d = _cX d getS
cS_ :: String -> Signal String
cS_ = _cX_ getS
cS0 :: String -> Signal String
cS0 = _cX "" getS

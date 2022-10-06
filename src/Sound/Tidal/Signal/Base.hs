{-# LANGUAGE OverloadedStrings, FlexibleInstances, BangPatterns #-}

-- (c) Alex McLean 2022 and contributors
-- Shared under the terms of the GNU Public License v. 3.0

-- Base representation and instances for Signals, including
-- implementation of Pattern class, plus core definitions of waveforms
-- etc.

module Sound.Tidal.Signal.Base where

import Data.Ratio
import Data.Fixed (mod')
import Data.Maybe (catMaybes, isJust, mapMaybe, fromJust, fromMaybe)
import qualified Data.Map.Strict as Map
import Data.List (delete, findIndex, (\\))
import Control.Applicative (liftA2)
import Data.Bool (bool)

import Sound.Tidal.Value
import Sound.Tidal.Signal.Event
import Sound.Tidal.Types
import Sound.Tidal.Pattern
import Sound.Tidal.Bjorklund (bjorklund)

import Prelude hiding ((<*), (*>))

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
  rev     = sigRev
  _run    = _sigRun
  _scan   = _sigScan
  timeCat = sigTimeCat
  when    = sigWhen
  _ply    = _sigPly
  collect = sigCollect
  uncollect = sigUncollect
  euclid  = sigEuclid
  _euclid = _sigEuclid
  _patternify f a pat      = innerJoin $ (`f` pat) <$> a
  _patternify2 f a b pat   = innerJoin $ (\x y -> f x y pat) <$> a <* b
  _patternify3 f a b c pat = innerJoin $ (\x y z -> f x y z pat) <$> a <* b <* c
  toSignal = id

-- ************************************************************ --

instance Applicative Signal where
  pure = atom -- TODO - would this be better as 'steady'?
  (<*>) = app

-- | Apply a pattern of values to a pattern of functions, given a
-- function to merge the 'whole' timearcs
app :: Signal (a -> b) -> Signal a -> Signal b
app patf patv = Signal f
    where f s = concatMap (\ef -> mapMaybe (combine ef) $ query patv s) $ query patf s
          combine ef ev = do new_active <- maybeSect (active ef) (active ev)
                             return $ Event {metadata = metadata ef <> metadata ev,
                                             whole = liftA2 sect (whole ef) (whole ev),
                                             active = new_active,
                                             value = value ef $ value ev
                                            }

-- | Alternative definition of <*>, which takes the wholes from the
-- pattern of functions (unrelated to the <* in Prelude)
(<*), appLeft :: Signal (a -> b) -> Signal a -> Signal b
(<*) patf patv = Signal f
  where f s = concatMap (\ef -> mapMaybe (combine ef) $ query patv (s {sArc = wholeOrActive ef})
                        ) $ query patf s
        combine ef ev = do new_active <- maybeSect (active ef) (active ev)
                           return $ Event {metadata = metadata ef <> metadata ev,
                                           whole = whole ef,
                                           active = new_active,
                                           value = value ef $ value ev
                                          }
appLeft = (<*)

-- | Alternative definition of <*>, which takes the wholes from the
-- pattern of functions (unrelated to the <* in Prelude)
(*>) :: Signal (a -> b) -> Signal a -> Signal b
(*>) patf patv = Signal f
  where f s = concatMap (\ev -> mapMaybe (combine ev) $ query patf (s {sArc = wholeOrActive ev})
                        ) $ query patv s
        combine ev ef = do new_active <- maybeSect (active ef) (active ev)
                           return $ Event {metadata = metadata ef <> metadata ev,
                                           whole = whole ev,
                                           active = new_active,
                                           value = value ef $ value ev
                                          }
appRight = (*>)

infixl 4 <*, *>

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

innerJoin :: Signal (Signal a) -> Signal a
innerJoin s = innerBind s id

outerBind :: Signal a -> (a -> Signal b) -> Signal b
outerBind = bindWhole (const)

outerJoin :: Signal (Signal a) -> Signal a
outerJoin s = outerBind s id

bindWhole :: (Maybe Arc -> Maybe Arc -> Maybe Arc) -> Signal a -> (a -> Signal b) -> Signal b
bindWhole chooseWhole pv f = Signal $ \state -> concatMap (match state) $ query pv state
  where match state event = map (withWhole event) $ query (f $ value event) (state {sArc = active event})
        withWhole event event' = event' {whole = chooseWhole (whole event) (whole event')}

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
          do w' <- (maybeSect <$> oWhole <*> iWhole)
             p' <- maybeSect oPart iPart
             return (Event (iMetadata <> oMetadata) w' p' v)

squeezeBind :: Signal a -> (a -> Signal b) -> Signal b
squeezeBind pat f = squeezeJoin $ fmap f pat

-- Flatterns patterns of patterns, by retriggering/resetting inner patterns at onsets of outer pattern haps

_trigTimeJoin :: (Time -> Time) -> Signal (Signal a) -> Signal a
_trigTimeJoin timeF patOfPats = Signal $ \state -> concatMap (queryInner state) $ query (discreteOnly patOfPats) state
  where queryInner state outerEvent
          = map (\innerEvent ->
                   Event {metadata = metadata innerEvent <> metadata outerEvent,
                          whole = sect <$> whole innerEvent <*> whole outerEvent,
                          active = sect (active innerEvent) (active outerEvent),
                          value = value innerEvent
                         }
                ) $ query (_late (timeF $ (aBegin $ wholeOrActive outerEvent)) (value outerEvent)) state

trigJoin :: Signal (Signal a) -> Signal a
trigJoin = _trigTimeJoin id

trigzeroJoin :: Signal (Signal a) -> Signal a
trigzeroJoin = _trigTimeJoin cyclePos

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
-- Metadata utils

setMetadata :: Metadata -> Signal a -> Signal a
setMetadata c pat = withEvents (map (\e -> e {metadata = c})) pat

withMetadata :: (Metadata -> Metadata) -> Signal a -> Signal a
withMetadata f pat = withEvents (map (\e -> e {metadata = f $ metadata e})) pat


-- ************************************************************ --
-- General hacks and utilities

instance Show (a -> b) where
  show _ = "<function>"

filterEvents :: (Event a -> Bool) -> Signal a -> Signal a
filterEvents f pat = Signal $ \state -> filter f $ query pat state

filterValues :: (a -> Bool) -> Signal a -> Signal a
filterValues f = filterEvents (f . value)

filterJusts :: Signal (Maybe a) -> Signal a
filterJusts = fmap fromJust . filterValues isJust

filterTime :: (Time -> Bool) -> Signal a -> Signal a
filterTime test p = p {query = filter (test . aBegin . wholeOrActive) . query p}

discreteOnly :: Signal a -> Signal a
discreteOnly = filterEvents $ isJust . whole

playFor :: Time -> Time -> Signal a -> Signal a
playFor s e pat = Signal $ \st -> maybe [] (\a -> query pat (st {sArc = a})) $ maybeSect (Arc s e) (sArc st)

-- ************************************************************ --
-- Time/event manipulations

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
sigSilence = Signal (\_ -> [])

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

-- | Hold a continuous value
steady :: a -> Signal a
steady v = waveform (const v)

-- ************************************************************ --
-- Waveforms

-- | A continuous signal as a function from time to values. Takes the
-- midpoint of the given query as the time value.
waveform :: (Time -> a) -> Signal a
waveform timeF = Signal $ \(State (Arc b e) _) -> 
  [Event {metadata = mempty,
          whole = Nothing,
          active = (Arc b e),
          value = timeF $ b+((e - b)/2)
         }
  ]

-- | Sawtooth waveform
saw :: (Fractional a, Real a) => Signal a
saw = waveform $ \t -> mod' (fromRational t) 1

saw2 :: (Fractional a, Real a) => Signal a
saw2 = toBipolar saw

-- | Inverse (descending) sawtooth waveform
isaw :: (Fractional a, Real a) => Signal a
isaw = (1-) <$> saw

isaw2 :: (Fractional a, Real a) => Signal a
isaw2 = toBipolar isaw

-- | Triangular wave
tri :: (Fractional a, Real a) => Signal a
tri = fastAppend saw isaw

tri2 :: (Fractional a, Real a) => Signal a
tri2 = toBipolar tri

-- | Sine waveform
sine :: Fractional a => Signal a
sine = fromBipolar sine2

sine2 :: Fractional a => Signal a
sine2 = waveform $ \t -> realToFrac $ sin ((pi :: Double) * 2 * fromRational t)

-- | Cosine waveform
cosine :: Fractional a => Signal a
cosine = _late 0.25 sine

cosine2 :: Fractional a => Signal a
cosine2 = _late 0.25 sine2

-- | Square wave
square :: Fractional a => Signal a
square = fastAppend (steady 1) (steady 0)

square2 :: Fractional a => Signal a
square2 = fastAppend (steady (-1)) (steady 1)

-- | @envL@ is a 'Signal' of continuous 'Double' values, representing
-- a linear interpolation between 0 and 1 during the first cycle, then
-- staying constant at 1 for all following cycles. Possibly only
-- useful if you're using something like the retrig function defined
-- in tidal.el.
envL :: (Fractional a, Ord a) => Signal a
envL = waveform $ \t -> max 0 $ min (fromRational t) 1

envL2 :: (Fractional a, Ord a) => Signal a
envL2 = toBipolar envL

-- | like 'envL' but reversed.
envLR :: (Fractional a, Ord a) => Signal a
envLR = (1-) <$> envL

envLR2 :: (Fractional a, Ord a) => Signal a
envLR2 = toBipolar envLR

-- | 'Equal power' version of 'env', for gain-based transitions
envEq :: (Fractional a, Ord a, Floating a) => Signal a
envEq = waveform $ \t -> sqrt (sin (pi/2 * max 0 (min (fromRational (1-t)) 1)))

envEq2 :: (Fractional a, Ord a, Floating a) => Signal a
envEq2 = toBipolar envEq

-- | Equal power reversed
envEqR :: (Fractional a, Ord a, Floating a) => Signal a
envEqR = waveform $ \t -> sqrt (cos (pi/2 * max 0 (min (fromRational (1-t)) 1)))

envEqR2 :: (Fractional a, Ord a, Floating a) => Signal a
envEqR2 = toBipolar envEqR


time :: Signal Time
time = waveform id

-- ************************************************************ --
-- Signal manipulations

splitQueries :: Signal a -> Signal a
splitQueries pat = Signal $ \state -> (concatMap (\arc -> query pat (state {sArc = arc}))
                                        $ splitArcs $ sArc state)

-- | Concatenate a list of signals, interleaving cycles.
sigSlowcat :: [Signal a] -> Signal a
sigSlowcat pats = splitQueries $ Signal queryCycle
  where queryCycle state = query (_late (offset $ sArc state) (pat $ sArc state)) state
        pat arc = pats !! (mod (floor $ aBegin $ arc) n)
        offset arc = (sam $ aBegin arc) - (sam $ aBegin arc / (toRational n))
        n = length pats

_sigFast :: Time -> Signal a -> Signal a
_sigFast t pat = withEventTime (/t) $ withQueryTime (*t) $ pat

_fastGap :: Time -> Signal a -> Signal a
_fastGap factor pat = splitQueries $ withEvent ef $ withQueryArcMaybe qf pat
  -- A bit fiddly, to drop zero-width queries at the start of the next cycle
  where qf (Arc b e) | bpos < 1 = Just $ Arc (cycle + bpos) (cycle + epos)
                     | otherwise = Nothing
          where cycle = sam b
                bpos = min 1 $ (b - cycle) * factor
                epos = min 1 $ (e - cycle) * factor
        -- Also fiddly, to maintain the right 'whole' relative to the part
        ef ev = ev {whole = w', active = a'}
          where a = active ev
                b = aBegin a
                e = aEnd a
                a' = Arc (cycle + bpos) (cycle + epos)
                  where cycle = sam b
                        bpos = min 1 $ (b - cycle) / factor
                        epos = min 1 $ (e - cycle) / factor
                w' = do w <- whole ev
                        let b' = aBegin a' - ((b - (aBegin w)) / factor)
                            e' = aEnd a' + (((aEnd w) - e) / factor)
                        return $ Arc b' e'

-- | Like @fast@, but only plays one cycle of the original signal
-- once per cycle, leaving a gap
fastGap :: Signal Time -> Signal a -> Signal a
fastGap = _patternify _fastGap


_compressArc :: Arc -> Signal a -> Signal a
_compressArc (Arc b e) pat | (b > e || b > 1 || e > 1 || b < 0 || e < 0) = silence
                           | otherwise = _late b $ _fastGap (1/(e-b)) pat

-- | Like @fastGap@, but takes the start and duration of the arc to compress the cycle into.
compress :: Signal Time -> Signal Time -> Signal a -> Signal a
compress patStart patDur pat = innerJoin $ (\s d -> _compressArc (Arc s (s+d)) pat) <$> patStart <*> patDur

_focusArc :: Arc -> Signal a -> Signal a
_focusArc (Arc b e) pat = _late (cyclePos b) $ _fast (1/(e-b)) pat

-- | Like @compress@, but doesn't leave a gap and can 'focus' on any arc (not just within a cycle)
focus :: Signal Time -> Signal Time -> Signal a -> Signal a
focus patStart patDur pat = innerJoin $ (\s d -> _focusArc (Arc s (s+d)) pat) <$> patStart <*> patDur

_early :: Time -> Signal a -> Signal a
_early t pat = withEventTime (subtract t) $ withQueryTime (+ t) $ pat

early :: Signal Time -> Signal x -> Signal x
early = _patternify _early

(<~) :: Signal Time -> Signal x -> Signal x
(<~) = early

_late :: Time -> Signal x -> Signal x
_late t = _early (0-t)

late :: Signal Time -> Signal x -> Signal x
late = _patternify _late

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

-- compressTo :: (Time,Time) -> Pattern a -> Pattern a
-- compressTo (s,e)      = compressArcTo (Arc s e)

repeatCycles :: Signal Int -> Signal a -> Signal a
repeatCycles = _patternify _repeatCycles

_repeatCycles :: Int -> Signal a -> Signal a
_repeatCycles n p = slowcat $ replicate n p

fastRepeatCycles :: Signal Int -> Signal a -> Signal a
fastRepeatCycles = _patternify _repeatCycles

_fastRepeatCycles :: Int -> Signal a -> Signal a
_fastRepeatCycles n p = fastcat $ replicate n p

sigStack :: [Signal a] -> Signal a
sigStack pats = Signal $ \s -> concatMap (\pat -> query pat s) pats

squash :: Time -> Signal a -> Signal a
squash into pat = splitQueries $ withEventArc ef $ withQueryArc qf pat
  where qf (Arc s e) = Arc (sam s + (min 1 $ (s - sam s) / into)) (sam s + (min 1 $ (e - sam s) / into))
        ef (Arc s e) = Arc (sam s + (s - sam s) * into) (sam s + (e - sam s) * into)

squashTo :: Time -> Time -> Signal a -> Signal a
squashTo b e = _late b . squash (e-b)

sigRev :: Signal a -> Signal a
sigRev pat = splitQueries $ Signal f
  where f state = withArc reflect <$> (query pat $ state {sArc = reflect $ sArc state})
          where cycle = sam $ aBegin $ sArc state
                next_cycle = nextSam cycle
                reflect (Arc b e) = Arc (cycle + (next_cycle - e)) (cycle + (next_cycle - b))


-- | A signal of whole numbers from 0 up to (and not including) the
-- given number, in a single cycle.
_sigRun :: (Enum a, Num a) => a -> Signal a
_sigRun n = fastFromList [0 .. n-1]


-- | From @1@ for the first cycle, successively adds a number until it gets up to @n@
_sigScan :: (Enum a, Num a) => a -> Signal a
_sigScan n = slowcat $ map _run [1 .. n]

-- | Similar to @fastCat@, but each signal is given a relative duration
sigTimeCat :: [(Time, Signal a)] -> Signal a
sigTimeCat tps = stack $ map (\(s,e,p) -> _compressArc (Arc (s/total) (e/total)) p) $ arrange 0 tps
    where total = sum $ map fst tps
          arrange :: Time -> [(Time, Signal a)] -> [(Time, Time, Signal a)]
          arrange _ [] = []
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
_sigPly t pat = squeezeJoin $ (_fast t . atom) <$> pat

-- | @segment n p@: 'samples' the signal @p@ at a rate of @n@
-- events per cycle. Useful for turning a continuous signal into a
-- discrete one.
segment :: Signal Time -> Signal a -> Signal a
segment = _patternify _segment

_segment :: Time -> Signal a -> Signal a
_segment n p = _fast n (atom id) <* p

--- functions relating to chords/patterns of lists

_sameDur :: Event a -> Event a -> Bool
_sameDur e1 e2 = (whole e1 == whole e2) && (active e1 == active e2)

_groupEventsBy :: Eq a => (Event a -> Event a -> Bool) -> [Event a] -> [[Event a]]
_groupEventsBy _ [] = []
_groupEventsBy f (e:es) = eqs:(_groupEventsBy f (es \\ eqs))
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
    remNo [] = []
    remNo (Nothing:cs) = remNo cs
    remNo ((Just c):cs) = c : (remNo cs)

-- | collects all events satisfying the same constraint into a list
_collectBy :: Eq a => (Event a -> Event a -> Bool) -> Signal a -> Signal [a]
_collectBy f = withEvents (_collectEventsBy f)

-- | collects all events occuring at the exact same time into a list
sigCollect :: Eq a => Signal a -> Signal [a]
sigCollect = _collectBy _sameDur

_uncollectEvent :: Event [a] -> [Event a]
_uncollectEvent e = [e {value = (value e)!!i, metadata = resolveMetadata i (metadata e)} | i <-[0..length (value e) - 1]]
  where resolveMetadata i (Metadata xs) = case length xs <= i of
                                            True -> Metadata []
                                            False -> Metadata [xs!!i]

_uncollectEvents :: [Event [a]] -> [Event a]
_uncollectEvents = concatMap _uncollectEvent

-- | merges all values in a list into one pattern by stacking the values
sigUncollect :: Signal [a] -> Signal a
sigUncollect = withEvents _uncollectEvents

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
sigEuclid :: Signal Int -> Signal Int -> Signal a -> Signal a
sigEuclid = _patternify2 _euclid

_sigEuclid :: Int -> Int -> Signal a -> Signal a
_sigEuclid n k a | n >= 0 = fastcat $ fmap (bool silence a) $ bjorklund (n,k)
                 | otherwise = fastcat $ fmap (bool a silence) $ bjorklund (-n,k)

{- | `euclidfull n k pa pb` stacks @e n k pa@ with @einv n k pb@ -}
euclidFull :: Signal Int -> Signal Int -> Signal a -> Signal a -> Signal a
euclidFull n k pa pb = stack [ euclid n k pa, euclidInv n k pb ]

_euclidBool :: Int -> Int -> Signal Bool
_euclidBool n k = fastFromList $ bjorklund (n,k)

_euclid' :: Int -> Int -> Signal a -> Signal a
_euclid' n k p = fastcat $ map (\x -> if x then p else silence) (bjorklund (n,k))

euclidOff :: Signal Int -> Signal Int -> Signal Int -> Signal a -> Signal a
euclidOff = _patternify3 _euclidOff

eoff :: Signal Int -> Signal Int -> Signal Int -> Signal a -> Signal a
eoff = euclidOff

_euclidOff :: Int -> Int -> Int -> Signal a -> Signal a
_euclidOff _ 0 _ _ = silence
_euclidOff n k s p = (_early $ fromIntegral s%fromIntegral k) (_euclid n k p)

euclidOffBool :: Signal Int -> Signal Int -> Signal Int -> Signal Bool -> Signal Bool
euclidOffBool = _patternify3 _euclidOffBool

_euclidOffBool :: Int -> Int -> Int -> Signal Bool -> Signal Bool
_euclidOffBool _ 0 _ _ = silence
_euclidOffBool n k s p = ((fromIntegral s % fromIntegral k) `_early`) ((\a b -> if b then a else not a) <$> _euclidBool n k <*> p)

distrib :: [Signal Int] -> Signal a -> Signal a
distrib ps p = do p' <- sequence ps
                  _distrib p' p

_distrib :: [Int] -> Signal a -> Signal a
_distrib xs p = boolsToPat (foldr distrib' (replicate (last xs) True) (reverse $ layers xs)) p
  where
    distrib' :: [Bool] -> [Bool] -> [Bool]
    distrib' [] _ = []
    distrib' (_:a) [] = False : distrib' a []
    distrib' (True:a) (x:b) = x : distrib' a b
    distrib' (False:a) b = False : distrib' a b
    layers = map bjorklund . (zip<*>tail)
    boolsToPat a b' = flip const <$> filterValues (== True) (fastFromList a) <*> b'

{- | `euclidInv` fills in the blanks left by `e`
 -
 @e 3 8 "x"@ -> @"x ~ ~ x ~ ~ x ~"@

 @euclidInv 3 8 "x"@ -> @"~ x x ~ x x ~ x"@
-}
euclidInv :: Signal Int -> Signal Int -> Signal a -> Signal a
euclidInv = _patternify2 _euclidInv

_euclidInv :: Int -> Int -> Signal a -> Signal a
_euclidInv n k a = _euclid (-n) k a

-- ************************************************************ --
-- Functions for getting control input as signals

valueToSignal :: Value -> Signal Value
valueToSignal (VSignal pat) = pat
valueToSignal v = pure v

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

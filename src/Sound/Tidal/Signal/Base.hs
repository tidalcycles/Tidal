{-# LANGUAGE DeriveFunctor, OverloadedStrings, FlexibleInstances #-}

-- (c) Alex McLean 2022 and contributors
-- Shared under the terms of the GNU Public License v. 3.0

module Sound.Tidal.Signal.Base where

import Data.Ratio
import Data.Fixed (mod')
import Data.Maybe (catMaybes, isJust, mapMaybe, fromJust)
import qualified Data.Map.Strict as Map
import Control.Applicative (liftA2)

import Sound.Tidal.Value
import Sound.Tidal.Event
import Sound.Tidal.Pattern

import Prelude hiding ((<*), (*>))

-- ************************************************************ --
-- State

-- | A timearc and some named control values, used to query a signal
-- with
data State = State {sArc :: Arc,
                    sControls :: ValueMap
                   }

-- ************************************************************ --
-- Signal

-- | A signal - a function from a timearc to a list of events active
-- during that timearc
-- This was known as a 'Pattern' in the previous version of Tidal. A
-- signal is a function from a timearc (possibly with some other
-- state) to events taking place in that timearc.

data Signal a = Signal {query :: State -> [Event a]}
  deriving (Functor)

instance Show a => Show (Signal a) where
  show pat = show $ queryArc pat (Arc 0 1)

-- | A control signal
type ControlSignal = Signal ValueMap


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
  _patternify f x pat = innerJoin $ (`f` pat) <$> x

-- ************************************************************ --

instance Applicative Signal where
  pure = steady
  (<*>) = app

-- | Apply a pattern of values to a pattern of functions, given a
-- function to merge the 'whole' timearcs
app :: Signal (a -> b) -> Signal a -> Signal b
app patf patv = Signal f
    where f s = concatMap (\ef -> catMaybes $ map (combine ef) $ query patv s) $ query patf s
          combine ef ev = do new_active <- maybeSect (active ef) (active ev)
                             return $ Event {metadata = metadata ef <> metadata ev,
                                             whole = liftA2 sect (whole ef) (whole ev),
                                             active = new_active,
                                             value = value ef $ value ev
                                            }

-- | Alternative definition of <*>, which takes the wholes from the
-- pattern of functions (unrelated to the <* in Prelude)
(<*) :: Signal (a -> b) -> Signal a -> Signal b
(<*) patf patv = Signal f
  where f s = concatMap (\ef -> catMaybes $ map (combine ef) $ query patv (s {sArc = wholeOrActive ef})
                        ) $ query patf s
        combine ef ev = do new_active <- maybeSect (active ef) (active ev)
                           return $ Event {metadata = metadata ef <> metadata ev,
                                           whole = whole ef,
                                           active = new_active,
                                           value = value ef $ value ev
                                          }
        
-- | Alternative definition of <*>, which takes the wholes from the
-- pattern of functions (unrelated to the <* in Prelude)
(*>) :: Signal (a -> b) -> Signal a -> Signal b
(*>) patf patv = Signal f
  where f s = concatMap (\ev -> catMaybes $ map (combine ev) $ query patf (s {sArc = wholeOrActive ev})
                        ) $ query patv s
        combine ev ef = do new_active <- maybeSect (active ef) (active ev)
                           return $ Event {metadata = metadata ef <> metadata ev,
                                           whole = whole ev,
                                           active = new_active,
                                           value = value ef $ value ev
                                          }

infixl 4 <*, *>

-- ************************************************************ --

instance Monad Signal where
  (>>=) = bind

bind :: Signal a -> (a -> Signal b) -> Signal b
bind = bindWhole (liftA2 sect)

innerBind :: Signal a -> (a -> Signal b) -> Signal b
innerBind = bindWhole const

innerJoin :: Signal (Signal a) -> Signal a
innerJoin s = innerBind s id

outerBind :: Signal a -> (a -> Signal b) -> Signal b
outerBind = bindWhole (flip const)

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
             mapMaybe (munge m w p) $ query (_compressArc (cycleArc $ wholeOrActive e) v) st {sArc = p}
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
                ) $ query (_late (timeF $ (begin $ wholeOrActive outerEvent)) (value outerEvent)) state

trigJoin :: Signal (Signal a) -> Signal a
trigJoin = _trigTimeJoin id

trigZeroJoin :: Signal (Signal a) -> Signal a
trigZeroJoin = _trigTimeJoin cyclePos

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

discreteOnly :: Signal a -> Signal a
discreteOnly = filterEvents $ isJust . whole

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

withQueryArc :: (Arc -> Arc) -> Signal a -> Signal a
withQueryArc arcf = withQuery (\state -> state {sArc = arcf $ sArc state})

withQueryTime :: (Time -> Time) -> Signal a -> Signal a
withQueryTime timef = withQueryArc (withArcTime timef)

-- ************************************************************ --
-- Fundamental signals

sigSilence :: Signal a
sigSilence = Signal (\_ -> [])

-- | Repeat discrete value once per cycle
sigAtom :: a -> Signal a
sigAtom v = Signal $ \state -> map
                               (\arc -> Event {metadata = mempty,
                                                whole = Just $ wholeCycle $ begin arc,
                                                active = arc,
                                                value = v
                                               }
                               )
                               (splitArcs $ sArc state)
  where wholeCycle :: Time -> Arc
        wholeCycle t = Arc (sam t) (nextSam t)

-- | Hold a continuous value
steady :: a -> Signal a
steady v = waveform (const v)

-- ************************************************************ --
-- Waveforms

-- | A continuous pattern as a function from time to values. Takes the
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

-- | @envL@ is a 'Pattern' of continuous 'Double' values, representing
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

-- ************************************************************ --
-- Signal manipulations

splitQueries :: Signal a -> Signal a
splitQueries pat = Signal $ \state -> (concatMap (\arc -> query pat (state {sArc = arc}))
                                        $ splitArcs $ sArc state)

-- | Concatenate a list of patterns, interleaving cycles.
sigSlowcat :: [Signal a] -> Signal a
sigSlowcat pats = splitQueries $ Signal queryCycle
  where queryCycle state = query (_late (offset $ sArc state) (pat $ sArc state)) state
        pat arc = pats !! (mod (floor $ begin $ arc) n)
        offset arc = (sam $ begin arc) - (sam $ begin arc / (toRational n))
        n = length pats

_sigFast :: Time -> Signal a -> Signal a
_sigFast t pat = withEventTime (/t) $ withQueryTime (*t) $ pat

_fastGap :: Time -> Signal a -> Signal a
_fastGap factor pat = splitQueries $ withEventArc (scale $ 1/factor) $ withQueryArc (scale factor) pat
  where scale factor' arc = Arc b e
          where cycle = sam $ begin arc
                b = cycle + (min 1 $ (begin arc - cycle) * factor)
                e = cycle + (min 1 $ (end   arc - cycle) * factor)

-- | Like @fast@, but only plays one cycle of the original pattern
-- once per cycle, leaving a gap
fastGap :: Signal Time -> Signal a -> Signal a
fastGap = _patternify _fastGap


_compressArc :: Arc -> Signal a -> Signal a
_compressArc (Arc b e) pat | (b > e || b > 1 || e > 1 || b < 0 || e < 0) = silence
                           | otherwise = _late b $ _fastGap (1/(e-b)) pat

-- | Like @fastGap@, but takes a start and end time to compress the cycle into.
compress :: Signal Time -> Signal Time -> Signal a -> Signal a
compress patStart patDur pat = innerJoin $ (\s d -> _compressArc (Arc s (s+d)) pat) <$> patStart <*> patDur

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

{- | Plays a portion of a pattern, specified by start and duration
The new resulting pattern is played over the time period of the original pattern:

@
d1 $ zoom 0.25 0.75 $ sound "bd*2 hh*3 [sn bd]*2 drum"
@

In the pattern above, `zoom` is used with an arc from 25% to 75%. It is equivalent to this pattern:

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
          where cycle = sam $ begin $ sArc state
                next_cycle = nextSam cycle
                reflect (Arc b e) = Arc (cycle + (next_cycle - e)) (cycle + (next_cycle - b))


-- | A pattern of whole numbers from 0 up to (and not including) the
-- given number, in a single cycle.
_sigRun :: (Enum a, Num a) => a -> Signal a
_sigRun n = fastFromList [0 .. n-1]


-- | From @1@ for the first cycle, successively adds a number until it gets up to @n@
_sigScan :: (Enum a, Num a) => a -> Signal a
_sigScan n = slowcat $ map _run [1 .. n]

-- | Similar to @fastCat@, but each pattern is given a relative duration
sigTimeCat :: [(Time, Signal a)] -> Signal a
sigTimeCat tps = stack $ map (\(s,e,p) -> _compressArc (Arc (s/total) (e/total)) p) $ arrange 0 tps
    where total = sum $ map fst tps
          arrange :: Time -> [(Time, Signal a)] -> [(Time, Time, Signal a)]
          arrange _ [] = []
          arrange t ((t',p):tps') = (t,t+t',p) : arrange (t+t') tps'

-- ************************************************************ --
-- Higher order transformations

--every :: Int -> (Signal a -> Signal a) -> Signal a -> Signal a
--every n f pat = splitQueries $ Signal 

--- ************************************************************ --


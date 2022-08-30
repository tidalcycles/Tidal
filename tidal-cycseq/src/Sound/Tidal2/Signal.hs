{-# LANGUAGE DeriveFunctor, OverloadedStrings #-}

-- (c) Alex McLean 2022
-- Shared under the terms of the GNU Public License v. 3.0

module Sound.Tidal2.Signal where

import Data.Ratio
import Data.Fixed (mod')
import Data.Maybe (catMaybes)
import qualified Data.Map.Strict as Map
import Control.Applicative (liftA2)

import Sound.Tidal2.Time
import Sound.Tidal2.Value
import Sound.Tidal2.Pattern

import Prelude hiding ((<*), (*>))

-- ************************************************************ --
-- Core definition of a Signal

-- | An event - a value, its 'whole' timespan, and the timespan that
-- its active (called a 'part' in tidal v1)
data Event a = Event {whole :: Maybe Span,
                      active :: Span, value :: a
                     }
  deriving (Show, Functor)

-- | A pattern - a function from a timespan to a list of events active
-- during that timespan
data Signal a = Signal {query :: Span -> [Event a]}
  deriving (Functor)

instance Show a => Show (Signal a) where
  show pat = show $ query pat (Span 0 1)

-- ************************************************************ --

-- | A control pattern as a map from strings to values
type ControlSignal = Signal (Map.Map String Value)

-- ************************************************************ --

instance Applicative Signal where
  pure = steady
  (<*>) = app (liftA2 sect)

-- | Apply a pattern of values to a pattern of functions, given a
-- function to merge the 'whole' timespans
app :: (Maybe Span -> Maybe Span -> Maybe Span) -> Signal (a -> b) -> Signal a -> Signal b
app wf patf patv = Signal f
    where f s = concatMap (\ef -> catMaybes $ map (apply ef) evs) efs
            where efs = (query patf s)
                  evs = (query patv s)
                  apply ef ev = apply' ef ev (maybeSect (active ef) (active ev))
                  apply' ef ev Nothing = Nothing
                  apply' ef ev (Just s') = Just $ Event (wf (whole ef) (whole ev)) s' (value ef $ value ev)

-- | Alternative definition of <*>, which takes the wholes from the
-- pattern of functions (unrelated to the <* in Prelude)
(<*) :: Signal (a -> b) -> Signal a -> Signal b
(<*) = app const

-- | Alternative definition of <*>, which takes the wholes from the
-- pattern of values (unrelated to the *> in Prelude)
(*>) :: Signal (a -> b) -> Signal a -> Signal b
(*>) = app (flip const)

-- ************************************************************ --

instance Monad Signal where
  (>>=) = bind

bind :: Signal a -> (a -> Signal b) -> Signal b
bind = bindWhole (liftA2 sect)

bindInner :: Signal a -> (a -> Signal b) -> Signal b
bindInner = bindWhole const

joinInner :: Signal (Signal a) -> Signal a
joinInner s = bindInner s id

bindOuter :: Signal a -> (a -> Signal b) -> Signal b
bindOuter = bindWhole (flip const)

joinOuter :: Signal (Signal a) -> Signal a
joinOuter s = bindOuter s id

bindWhole :: (Maybe Span -> Maybe Span -> Maybe Span) -> Signal a -> (a -> Signal b) -> Signal b
bindWhole chooseWhole pv f = Signal $ \s -> concatMap (match s) $ query pv s
  where match s e = map (withWhole e) $ query (f $ value e) (active e)
        withWhole e e' = e' {whole = chooseWhole (whole e) (whole e')}

-- ************************************************************ --
-- Pattern instance

instance Pattern Signal where
  slowcat = sigSlowcat
  _fast   = _sigFast
  _early  = _sigEarly
  silence = sigSilence
  atom    = sigAtom
  stack   = sigStack
  _patternify f x pat = joinInner $ (`f` pat) <$> x

-- ************************************************************ --
-- General hacks

instance Show (a -> b) where
  show _ = "<function>"

-- ************************************************************ --
-- Time utilities

withEventSpan :: (Span -> Span) -> Signal a -> Signal a
withEventSpan spanf pat = Signal f
  where f s = map (\e -> e {active = spanf $ active e,
                            whole = spanf <$> whole e
                           }) $ query pat s

withEventTime :: (Rational -> Rational) -> Signal a -> Signal a
withEventTime timef pat = Signal f
  where f s = map (\e -> e {active = withSpanTime timef $ active e,
                            whole = withSpanTime timef <$> whole e
                           }) $ query pat s

withSpanTime :: (Rational -> Rational) -> Span -> Span
withSpanTime timef (Span b e) = Span (timef b) (timef e)

withQuery :: (Span -> Span) -> Signal a -> Signal a
withQuery spanf pat = Signal $ \s -> query pat $ spanf s

withQueryTime :: (Rational -> Rational) -> Signal a -> Signal a
withQueryTime timef = withQuery (withSpanTime timef)

-- ************************************************************ --
-- Fundamental patterns

sigSilence :: Signal a
sigSilence = Signal (\_ -> [])

-- | Repeat discrete value once per cycle
sigAtom :: a -> Signal a
sigAtom v = Signal f
  where f s = map (\s' -> Event (Just $ wholeCycle $ begin s') s' v) (splitSpans s)
        wholeCycle :: Rational -> Span
        wholeCycle t = Span (sam t) (nextSam t)

-- | A continuous value
steady :: a -> Signal a
steady v = waveform (const v)

-- ************************************************************ --
-- Waveforms

-- | A continuous pattern as a function from time to values. Takes the
-- midpoint of the given query as the time value.
waveform :: (Rational -> a) -> Signal a
waveform timeF = Signal {query = f}
  where f (Span b e) = [Event {whole = Nothing,
                               active = (Span b e),
                               value = timeF $ b+((e - b)/2)
                              }
                       ]

-- | Sawtooth waveform
saw :: Signal Rational
saw = waveform $ \t -> mod' t 1

saw2 :: Signal Rational
saw2 = toBipolar saw

-- | Sine waveform
sine :: Fractional a => Signal a
sine = fromBipolar sine2

sine2 :: Fractional a => Signal a
sine2 = waveform $ \t -> realToFrac $ sin ((pi :: Double) * 2 * fromRational t)

-- ************************************************************ --
-- Signal manipulations

splitQueries :: Signal a -> Signal a
splitQueries pat = Signal $ \s -> (concatMap (query pat) $ splitSpans s)

-- | Concatenate a list of patterns (works a little differently from
-- 'real' tidal, needs some work)
sigSlowcat :: [Signal a] -> Signal a
sigSlowcat pats = splitQueries $ Signal queryCycle
  where queryCycle s = query (pats !! (mod (floor $ begin s) n)) s
        n = length pats

_sigFast :: Rational -> Signal a -> Signal a
_sigFast t pat = withEventTime (/t) $ withQueryTime (*t) $ pat

_sigEarly :: Rational -> Signal a -> Signal a
_sigEarly t pat = withEventTime (subtract t) $ withQueryTime (+ t) $ pat

sigStack :: [Signal a] -> Signal a
sigStack pats = Signal $ \s -> concatMap (\pat -> query pat s) pats

squash :: Rational -> Signal a -> Signal a
squash into pat = splitQueries $ withEventSpan ef $ withQuery qf pat
  where qf (Span s e) = Span (sam s + (min 1 $ (s - sam s) / into)) (sam s + (min 1 $ (e - sam s) / into))
        ef (Span s e) = Span (sam s + (s - sam s) * into) (sam s + (e - sam s) * into)

squashTo :: Rational -> Rational -> Signal a -> Signal a
squashTo b e = _late b . squash (e-b)

-- ************************************************************ --
-- Higher order transformations

--every :: Int -> (Signal a -> Signal a) -> Signal a -> Signal a
--every n f pat = splitQueries $ Signal 

-- ************************************************************ --

(#) :: ControlSignal -> ControlSignal -> ControlSignal
(#) a b = Map.union <$> a <*> b

(|+|) :: Num a => Signal a -> Signal a -> Signal a
(|+|) a b = (+) <$> a <*> b

(|+) :: Num a => Signal a -> Signal a -> Signal a
(|+) a b = ((+) <$> a) <* b

(+|) :: Num a => Signal a -> Signal a -> Signal a
(+|) a b = ((+) <$> a) *> b

-- ************************************************************ --

sound :: Signal String -> ControlSignal
sound pat = (Map.singleton "sound" . S) <$> pat

note :: Signal Double -> ControlSignal
note pat = (Map.singleton "note" . F) <$> pat

-- ************************************************************ --

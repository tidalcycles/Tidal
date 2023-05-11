-- (c) Alex McLean 2022 and contributors
-- Shared under the terms of the GNU Public License v. 3.0

-- Fundamental continuous waveforms, like sinewaves, triangular waves
-- and so on

module Sound.Tidal.Signal.Waveform where

import           Data.Fixed              (mod')
import           Sound.Tidal.Pattern
import           Sound.Tidal.Signal.Base
import           Sound.Tidal.Types

-- ************************************************************ --
-- Waveforms

-- | A continuous signal as a function from time to values. Takes the
-- midpoint of the given query as the time value.
waveform :: (Time -> a) -> Signal a
waveform timeF = Signal $ \(State (Arc b e) _) ->
  [Event {metadata = mempty,
          whole = Nothing,
          active = Arc b e,
          value = timeF $ b+((e - b)/2)
         }
  ]

-- | Hold the given value as a continuous waveform
steady :: a -> Signal a
steady v = waveform (const v)

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
square = fastAppend (steady 0) (steady 1)

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

time :: Fractional a => Signal a
time = waveform fromRational

{-# LANGUAGE BangPatterns #-}

module Sound.Tidal.Transition where

import Control.Concurrent.MVar (readMVar, swapMVar)
import qualified Data.Map.Strict as Map
-- import Data.Maybe (fromJust)

import qualified Sound.Tidal.Clock as Clock
import Sound.Tidal.Control (_stut)
import Sound.Tidal.Core
import Sound.Tidal.ID (ID (fromID))
import Sound.Tidal.Params (gain, pan)
import Sound.Tidal.Pattern
import Sound.Tidal.Stream.Config (Config (cClockConfig))
import Sound.Tidal.Stream.Types
  ( PlayState (PlayState, psHistory, psMute, psPattern, psSolo),
    Stream (sClockRef, sConfig, sPMapMV),
  )
-- import Sound.Tidal.Tempo as T
import Sound.Tidal.UI (fadeInFrom, fadeOutFrom)
import Sound.Tidal.Utils (enumerate)
import Prelude hiding ((*>), (<*))

{-
    Transition.hs - A library for handling transitions between patterns
    Copyright (C) 2020, Alex McLean and contributors

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

type TransitionMapper = Time -> [ControlPattern] -> ControlPattern

-- Evaluation of pat is forced so exceptions are picked up here, before replacing the existing pattern.
-- the "historyFlag" determines if the new pattern should be placed on the history stack or not
transition :: Stream -> Bool -> TransitionMapper -> ID -> ControlPattern -> IO ()
transition stream historyFlag mapper patId !pat = do
  let appendPat flag = if flag then (pat :) else id
      updatePS (Just playState) = playState {psHistory = appendPat historyFlag (psHistory playState)}
      updatePS Nothing =
        PlayState
          { psPattern = silence,
            psMute = False,
            psSolo = False,
            psHistory = appendPat historyFlag [silence]
          }
      transition' pat' = do
        t <- Clock.getCycleTime (cClockConfig $ sConfig stream) (sClockRef stream)
        return $! mapper t pat'
  pMap <- readMVar (sPMapMV stream)
  let playState = updatePS $ Map.lookup (fromID patId) pMap
  pat' <- transition' $ appendPat (not historyFlag) (psHistory playState)
  let pMap' = Map.insert (fromID patId) (playState {psPattern = pat'}) pMap
  _ <- swapMVar (sPMapMV stream) pMap'
  return ()

_mortalOverlay :: Time -> Time -> [Pattern a] -> Pattern a
_mortalOverlay _ _ [] = silence
_mortalOverlay t now (pat : ps) = overlay (pop ps) (playFor s (s + t) pat)
  where
    pop [] = silence
    pop (x : _) = x
    s = sam (now - fromIntegral (floor now `mod` floor t :: Int)) + sam t

-- | Washes away the current pattern after a certain delay by applying a
--    function to it over time, then switching over to the next pattern to
--    which another function is applied.
_wash :: (Pattern a -> Pattern a) -> (Pattern a -> Pattern a) -> Time -> Time -> Time -> Time -> [Pattern a] -> Pattern a
_wash _ _ _ _ _ _ [] = silence
_wash _ _ _ _ _ _ [pat] = pat
_wash fout fin delay durin durout now (pat : pat' : _) =
  stack
    [ filterWhen (< (now + delay)) pat',
      filterWhen (between (now + delay) (now + delay + durin)) $ fout pat',
      filterWhen (between (now + delay + durin) (now + delay + durin + durout)) $ fin pat,
      filterWhen (>= (now + delay + durin + durout)) pat
    ]
  where
    between lo hi x = (x >= lo) && (x < hi)

_washIn :: (Pattern a -> Pattern a) -> Time -> Time -> [Pattern a] -> Pattern a
_washIn f durin now pats = _wash f id 0 durin 0 now pats

_xfadeIn :: Time -> Time -> [ControlPattern] -> ControlPattern
_xfadeIn _ _ [] = silence
_xfadeIn _ _ [pat] = pat
_xfadeIn t now (pat : pat' : _) = overlay (pat |* gain (now `rotR` _slow t envEqR)) (pat' |* gain (now `rotR` _slow t envEq))

-- | Pans the last n versions of the pattern across the field
_histpan :: Int -> Time -> [ControlPattern] -> ControlPattern
_histpan _ _ [] = silence
_histpan 0 _ _ = silence
_histpan n _ ps = stack $ map (\(i, pat) -> pat # pan (pure $ fromIntegral i / fromIntegral n')) (enumerate ps')
  where
    ps' = take n ps
    n' = length ps' -- in case there's fewer patterns than requested

-- | Just stop for a bit before playing new pattern
_wait :: Time -> Time -> [ControlPattern] -> ControlPattern
_wait _ _ [] = silence
_wait t now (pat : _) = filterWhen (>= nextSam (now + t - 1)) pat

-- | Just as `wait`, `waitT` stops for a bit and then applies the given transition to the playing pattern
--
-- @
-- d1 $ sound "bd"
--
-- t1 (waitT (xfadeIn 8) 4) $ sound "hh*8"
-- @
_waitT :: (Time -> [ControlPattern] -> ControlPattern) -> Time -> Time -> [ControlPattern] -> ControlPattern
_waitT _ _ _ [] = silence
_waitT f t now pats = filterWhen (>= nextSam (now + t - 1)) (f (now + t) pats)

-- |
-- Jumps directly into the given pattern, this is essentially the _no transition_-transition.
--
-- Variants of @jump@ provide more useful capabilities, see @jumpIn@ and @jumpMod@
_jump :: Time -> [ControlPattern] -> ControlPattern
_jump = _jumpIn 0

-- | Sharp `jump` transition after the specified number of cycles have passed.
--
-- @
-- t1 (jumpIn 2) $ sound "kick(3,8)"
-- @
_jumpIn :: Int -> Time -> [ControlPattern] -> ControlPattern
_jumpIn n = _wash id id (fromIntegral n) 0 0

-- | Unlike `jumpIn` the variant `jumpIn'` will only transition at cycle boundary (e.g. when the cycle count is an integer).
_jumpIn' :: Int -> Time -> [ControlPattern] -> ControlPattern
_jumpIn' n now = _wash id id (nextSam now - now + fromIntegral n) 0 0 now

-- | Sharp `jump` transition at next cycle boundary where cycle mod n == 0
_jumpMod :: Int -> Time -> [ControlPattern] -> ControlPattern
_jumpMod n now = _jumpIn' ((n - 1) - (floor now `mod` n)) now

-- | Sharp `jump` transition at next cycle boundary where cycle mod n == p
_jumpMod' :: Int -> Int -> Time -> [ControlPattern] -> ControlPattern
_jumpMod' n p now = _jumpIn' ((n - 1) - (floor now `mod` n) + p) now

-- | Degrade the new pattern over time until it ends in silence
_mortal :: Time -> Time -> Time -> [ControlPattern] -> ControlPattern
_mortal _ _ _ [] = silence
_mortal lifespan release now (p : _) = overlay (filterWhen (< (now + lifespan)) p) (filterWhen (>= (now + lifespan)) (fadeOutFrom (now + lifespan) release p))

_interpolate :: Time -> [ControlPattern] -> ControlPattern
_interpolate = _interpolateIn 4

_interpolateIn :: Time -> Time -> [ControlPattern] -> ControlPattern
_interpolateIn _ _ [] = silence
_interpolateIn _ _ [p] = p
_interpolateIn t now (pat : pat' : _) = f <$> pat' *> pat <* automation
  where
    automation = now `rotR` _slow t envL
    f a b x =
      Map.unionWith
        ( fNum2
            (\a' b' -> floor $ fromIntegral a' * x + fromIntegral b' * (1 - x))
            (\a' b' -> a' * x + b' * (1 - x))
        )
        b
        a

-- |
-- Degrades the current pattern while undegrading the next.
--
-- This is like @xfade@ but not by gain of samples but by randomly removing events from the current pattern and slowly adding back in missing events from the next one.
--
-- @
-- d1 $ sound "bd(3,8)"
--
-- t1 clutch $ sound "[hh*4, odx(3,8)]"
-- @
--
-- @clutch@ takes two cycles for the transition, essentially this is @clutchIn 2@.
_clutch :: Time -> [Pattern a] -> Pattern a
_clutch = _clutchIn 2

-- |
-- Also degrades the current pattern and undegrades the next.
-- To change the number of cycles the transition takes, you can use @clutchIn@ like so:
--
-- @
-- d1 $ sound "bd(5,8)"
--
-- t1 (clutchIn 8) $ sound "[hh*4, odx(3,8)]"
-- @
--
-- will take 8 cycles for the transition.
_clutchIn :: Time -> Time -> [Pattern a] -> Pattern a
_clutchIn _ _ [] = silence
_clutchIn _ _ [p] = p
_clutchIn t now (p : p' : _) = overlay (fadeOutFrom now t p') (fadeInFrom now t p)

-- | same as `anticipate` though it allows you to specify the number of cycles until dropping to the new pattern, e.g.:
--
-- @
-- d1 $ sound "jvbass(3,8)"
--
-- t1 (anticipateIn 4) $ sound "jvbass(5,8)"
-- @
_anticipateIn :: Time -> Time -> [ControlPattern] -> ControlPattern
_anticipateIn t now pats = _washIn (innerJoin . (\pat -> (\v -> _stut 8 0.2 v pat) <$> (now `rotR` _slow t (toRational <$> envLR)))) t now pats

-- wash :: (Pattern a -> Pattern a) -> (Pattern a -> Pattern a) -> Time -> Time -> Time -> Time -> [Pattern a] -> Pattern a

-- | `anticipate` is an increasing comb filter.
--
-- Build up some tension, culminating in a _drop_ to the new pattern after 8 cycles.
_anticipate :: Time -> [ControlPattern] -> ControlPattern
_anticipate = _anticipateIn 8

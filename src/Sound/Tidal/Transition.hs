{-# LANGUAGE BangPatterns #-}

module Sound.Tidal.Transition where

import Prelude hiding ((<*), (*>))

import qualified Data.List as L
import Control.Concurrent.MVar (modifyMVar_)

import qualified Data.Map.Strict as Map
-- import Data.Maybe (fromJust)

import Sound.Tidal.Control
import Sound.Tidal.Core
import Sound.Tidal.ID
import Sound.Tidal.Params (gain, pan)
import Sound.Tidal.Pattern
import Sound.Tidal.Stream
import Sound.Tidal.Tempo as T
import Sound.Tidal.UI (fadeOutFrom, fadeInFrom)
import Sound.Tidal.Utils (enumerate)

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

-- Evaluation of pat is forced so exceptions are picked up here, before replacing the existing pattern.
-- the "historyFlag" determines if the new pattern should be placed on the history stack or not
transition :: Stream -> Bool -> (Time -> [ControlPattern] -> ControlPattern) -> ID -> ControlPattern -> IO ()
transition stream historyFlag f patId !pat =
  modifyMVar_ (sActionsMV stream) (\actions -> return $! (T.Transition historyFlag f patId pat) : actions)

mortalOverlay :: Time -> Time -> [Pattern a] -> Pattern a
mortalOverlay _ _ [] = silence
mortalOverlay t now (pat:ps) = overlay (pop ps) (playFor s (s+t) pat) where
  pop [] = silence
  pop (x:_) = x
  s = sam (now - fromIntegral (floor now `mod` floor t :: Int)) + sam t

scheduleToPat ::
  [ ( -- ^ A schedule, with offsets relative to the current time.
    Time, -- ^ Absolute time, not time relative to now.
          -- It is when the new pattern starts, not when the old one ends.
          -- PITFALL: Each should be distinct.
    Pattern a -- ^ What starts when the associated `Time` is reached.
  ) ]
  -> Pattern a
scheduleToPat s =
  let
    between lo hi x = (x >= lo) && (x < hi)
    lastPat = filterWhen (>= t) p
      where (t, p) = last s
    patternsBeforeLast = [ filterWhen (between t0 t1) p
                         | ( (t0,p),
                             (t1,_) ) <- zip s $ tail s ]
  in stack $
     lastPat : patternsBeforeLast -- order doesn't matter to `stack`

delaySchedule_toAbsoluteSchedule ::
  Time ->                  -- ^ now
  [ (Time, Pattern a) ] -> -- ^ a schedule defined by times relative to now
  [ (Time, Pattern a) ]    -- ^ a schedule defined by absolute times
delaySchedule_toAbsoluteSchedule now s =
  [ (t + now, p) | (t,p) <- s ]

jumpFrac :: Time        -- ^ how long to wait
         -> Time        -- ^ not supplied by user!
         -> [Pattern a] -- ^ not supplied by user!
         -> Pattern a
jumpFrac _ _ [] = silence
jumpFrac _ _ (pat:[]) = pat
jumpFrac wait now (pat':pat:_) =
  stack [ filterWhen (<  (now + wait)) pat
        , filterWhen (>= (now + wait)) pat' ]

{-| Washes away the current pattern after a certain delay by applying a
    function to it over time, then switching over to the next pattern to
    which another function is applied.
-}
wash :: (Pattern a -> Pattern a) -> (Pattern a -> Pattern a) -> Time -> Time -> Time -> Time -> [Pattern a] -> Pattern a
wash _ _ _ _ _ _ [] = silence
wash _ _ _ _ _ _ (pat:[]) = pat
wash fout fin delay durin durout now (pat:pat':_) =
   stack [(filterWhen (< (now + delay)) pat'),
          (filterWhen (between (now + delay) (now + delay + durin)) $ fout pat'),
          (filterWhen (between (now + delay + durin) (now + delay + durin + durout)) $ fin pat),
          (filterWhen (>= (now + delay + durin + durout)) $ pat)
         ]
 where
   between lo hi x = (x >= lo) && (x < hi)

washIn :: (Pattern a -> Pattern a) -> Time -> Time -> [Pattern a] -> Pattern a
washIn f durin now pats = wash f id 0 durin 0 now pats

xfadeIn :: Time -> Time -> [ControlPattern] -> ControlPattern
xfadeIn _ _ [] = silence
xfadeIn _ _ (pat:[]) = pat
xfadeIn t now (pat:pat':_) = overlay (pat |* gain (now `rotR` (_slow t envEqR))) (pat' |* gain (now `rotR` (_slow t (envEq))))

-- | Pans the last n versions of the pattern across the field
histpan :: Int -> Time -> [ControlPattern] -> ControlPattern
histpan _ _ [] = silence
histpan 0 _ _ = silence
histpan n _ ps = stack $ map (\(i,pat) -> pat # pan (pure $ (fromIntegral i) / (fromIntegral n'))) (enumerate ps')
  where ps' = take n ps
        n' = length ps' -- in case there's fewer patterns than requested

-- | Just stop for a bit before playing new pattern
wait :: Time -> Time -> [ControlPattern] -> ControlPattern
wait _ _ [] = silence
wait t now (pat:_) = filterWhen (>= (nextSam (now+t-1))) pat

{- | Just as `wait`, `waitT` stops for a bit and then applies the given transition to the playing pattern

@
d1 $ sound "bd"

t1 (waitT (xfadeIn 8) 4) $ sound "hh*8"
@
-}
waitT :: (Time -> [ControlPattern] -> ControlPattern) -> Time -> Time -> [ControlPattern] -> ControlPattern
waitT _ _ _ [] = silence
waitT f t now pats = filterWhen (>= (nextSam (now+t-1))) (f (now + t) pats)

{- |
Jumps directly into the given pattern, this is essentially the _no transition_-transition.

Variants of @jump@ provide more useful capabilities, see @jumpIn@ and @jumpMod@
-}
jump :: Time -> [ControlPattern] -> ControlPattern
jump = jumpIn 0

{- | Sharp `jump` transition after the specified number of cycles have passed.

@
t1 (jumpIn 2) $ sound "kick(3,8)"
@
-}
jumpIn :: Int -> Time -> [ControlPattern] -> ControlPattern
jumpIn n = wash id id (fromIntegral n) 0 0

{- | Unlike `jumpIn` the variant `jumpIn'` will only transition at cycle boundary (e.g. when the cycle count is an integer).
-}
jumpIn' :: Int -> Time -> [ControlPattern] -> ControlPattern
jumpIn' n now = wash id id ((nextSam now) - now + (fromIntegral n)) 0 0 now

-- | Sharp `jump` transition at next cycle boundary where cycle mod n == 0
jumpMod :: Int -> Time -> [ControlPattern] -> ControlPattern
jumpMod n now = jumpIn' ((n-1) - ((floor now) `mod` n)) now

-- | Sharp `jump` transition at next cycle boundary where cycle mod n == p
jumpMod' :: Int -> Int -> Time -> [ControlPattern] -> ControlPattern
jumpMod' n p now = Sound.Tidal.Transition.jumpIn' ((n-1) - ((floor now) `mod` n) + p) now

-- | Degrade the new pattern over time until it ends in silence
mortal :: Time -> Time -> Time -> [ControlPattern] -> ControlPattern
mortal _ _ _ [] = silence
mortal lifespan release now (p:_) = overlay (filterWhen (<(now+lifespan)) p) (filterWhen (>= (now+lifespan)) (fadeOutFrom (now + lifespan) release p))


interpolate :: Time -> [ControlPattern] -> ControlPattern
interpolate = interpolateIn 4

interpolateIn :: Time -> Time -> [ControlPattern] -> ControlPattern
interpolateIn _ _ [] = silence
interpolateIn _ _ (p:[]) = p
interpolateIn t now (pat:pat':_) = f <$> pat' *> pat <* automation
  where automation = now `rotR` (_slow t envL)
        f = (\a b x -> Map.unionWith (fNum2 (\a' b' -> floor $ (fromIntegral a') * x + (fromIntegral b') * (1-x))
                                            (\a' b' -> a' * x + b' * (1-x))
                                     )
                       b a
            )

{-|
Degrades the current pattern while undegrading the next.

This is like @xfade@ but not by gain of samples but by randomly removing events from the current pattern and slowly adding back in missing events from the next one.

@
d1 $ sound "bd(3,8)"

t1 clutch $ sound "[hh*4, odx(3,8)]"
@

@clutch@ takes two cycles for the transition, essentially this is @clutchIn 2@.
-}
clutch :: Time -> [Pattern a] -> Pattern a
clutch = clutchIn 2

{-|
Also degrades the current pattern and undegrades the next.
To change the number of cycles the transition takes, you can use @clutchIn@ like so:

@
d1 $ sound "bd(5,8)"

t1 (clutchIn 8) $ sound "[hh*4, odx(3,8)]"
@

will take 8 cycles for the transition.
-}
clutchIn :: Time -> Time -> [Pattern a] -> Pattern a
clutchIn _ _ [] = silence
clutchIn _ _ (p:[]) = p
clutchIn t now (p:p':_) = overlay (fadeOutFrom now t p') (fadeInFrom now t p)

{-| same as `anticipate` though it allows you to specify the number of cycles until dropping to the new pattern, e.g.:

@
d1 $ sound "jvbass(3,8)"

t1 (anticipateIn 4) $ sound "jvbass(5,8)"
@-}
anticipateIn :: Time -> Time -> [ControlPattern] -> ControlPattern
anticipateIn t now pats = washIn (innerJoin . (\pat -> (\v -> _stut 8 0.2 v pat) <$> (now `rotR` (_slow t $ toRational <$> envLR)))) t now pats

-- wash :: (Pattern a -> Pattern a) -> (Pattern a -> Pattern a) -> Time -> Time -> Time -> Time -> [Pattern a] -> Pattern a

{- | `anticipate` is an increasing comb filter.

Build up some tension, culminating in a _drop_ to the new pattern after 8 cycles.
-}
anticipate :: Time -> [ControlPattern] -> ControlPattern
anticipate = anticipateIn 8

module Sound.Tidal.Transition where

import Sound.Tidal.Stream
import Sound.Tidal.Pattern
import Sound.Tidal.Time
import Sound.Tidal.Params
import Sound.Tidal.Utils

import Control.Concurrent.MVar
import Control.Applicative
import Data.Maybe

import qualified Data.Map.Strict as Map
import Data.Monoid

transition :: (IO Time) -> MVar (ParamPattern, [ParamPattern]) -> (Time -> [ParamPattern] -> ParamPattern) -> ParamPattern -> IO ()
transition getNow mv f p =
  do now <- getNow
     ps <- takeMVar mv
     let p' = f now (p:snd ps)
     -- don't put the transition in history, only
     -- the target pattern, or things get overcomplex
     -- (transitions of transitions)
     putMVar mv (p', (p:snd ps))
     return ()

-- | Pans the last n versions of the pattern across the field
histpan :: Int -> Time -> [ParamPattern] -> ParamPattern
histpan _ _ [] = silence
histpan 0 _ _ = silence
histpan n _ ps = stack $ map (\(i,p) -> p # pan (atom $ (fromIntegral i) / (fromIntegral n'))) (enumerate ps')
  where ps' = take n ps
        n' = length ps' -- in case there's fewer patterns than requested

{-|
A generalization of `wash`. Washes away the current pattern after a certain delay by applying a function to it over time, then switching over to the next pattern to which another function is applied.
-}
superwash :: (Pattern a -> Pattern a) -> (Pattern a -> Pattern a) -> Time -> Time -> Time -> Time -> [Pattern a] -> Pattern a
superwash _ _ _ _ _ _ [] = silence
superwash _ _ _ _ _ _ (p:[]) = p
superwash fout fin delay durin durout now (p:p':_) =
   stack [(playWhen (< (now + delay)) p'),
          (playWhen (between (now + delay) (now + delay + durin)) $ fout p'),
          (playWhen (between (now + delay + durin) (now + delay + durin + durout)) $ fin p),
          (playWhen (>= (now + delay + durin + durout)) $ p)
         ]
 where
   between lo hi x = (x >= lo) && (x < hi)

{-|
Wash away the current pattern by applying a function to it over time, then switching over to the next.

@
d1 $ sound "feel ! feel:1 feel:2"

t1 (wash (chop 8) 4) $ sound "feel*4 [feel:2 sn:2]"
@

Note that `chop 8` is applied to `sound "feel ! feel:1 feel:2"` for 4 cycles and then the whole pattern is replaced by `sound "feel*4 [feel:2 sn:2]`
-}
wash :: (Pattern a -> Pattern a) -> Time -> Time -> [Pattern a] -> Pattern a
wash _ _ _ [] = silence
wash _ _ _ (p:[]) = p
wash f t now (p:p':_) = overlay (playWhen (< (now + t)) $ f p') (playWhen (>= (now + t)) p)


-- | Just stop for a bit before playing new pattern
wait :: Time -> Time -> [ParamPattern] -> ParamPattern
wait _ _ [] = silence
wait t now (p:_) = playWhen (>= (nextSam (now+t-1))) p

{- | Just as `wait`, `wait'` stops for a bit and then applies the given transition to the playing pattern

@
d1 $ sound "bd"

t1 (wait' (xfadeIn 8) 4) $ sound "hh*8"
@
-}
wait' :: (Time -> [ParamPattern] -> ParamPattern) -> Time -> Time -> [ParamPattern] -> ParamPattern
wait' _ t _ [] = silence
wait' f t now ps@(p:_) = playWhen (>= (nextSam (now+t-1))) (f (now + t) ps)


{- |
Jumps directly into the given pattern, this is essentially the _no transition_-transition.

Variants of `jump` provide more useful capabilities, see `jumpIn` and `jumpMod`
-}
jump :: Time -> [ParamPattern] -> ParamPattern
jump = jumpIn 0

{- | Sharp `jump` transition after the specified number of cycles have passed.

@
t1 (jumpIn 2) $ sound "kick(3,8)"
@
-}
jumpIn :: Int -> Time -> [ParamPattern] -> ParamPattern
jumpIn n = superwash id id (fromIntegral n) 0 0

{- | Unlike `jumpIn` the variant `jumpIn'` will only transition at cycle boundary (e.g. when the cycle count is an integer).
-}
jumpIn' :: Int -> Time -> [ParamPattern] -> ParamPattern
jumpIn' n now = superwash id id ((nextSam now) - now + (fromIntegral n)) 0 0 now

-- | Sharp `jump` transition at next cycle boundary where cycle mod n == 0
jumpMod :: Int -> Time -> [ParamPattern] -> ParamPattern
jumpMod n now = jumpIn ((n-1) - ((floor now) `mod` n)) now

-- | Degrade the new pattern over time until it ends in silence
mortal :: Time -> Time -> Time -> [ParamPattern] -> ParamPattern
mortal _ _ _ [] = silence
mortal lifespan release now (p:_) = overlay (playWhen (<(now+lifespan)) p) (playWhen (>= (now+lifespan)) (fadeOut' (now + lifespan) release p))


interpolateIn :: Time -> Time -> [ParamPattern] -> ParamPattern
interpolateIn _ _ [] = silence
interpolateIn _ _ (p:[]) = p
interpolateIn t now (p:p':_) = do n <- now `rotR` (_slow t envL)
                                  combineV (mixNums n) <$> p <*> p'
  where mixNums v (VF a) (VF b) = VF $ (a * v) + (b * (1-v))
        mixNums v (VI a) (VI b) = VI $ floor $ (fromIntegral a * v) + (fromIntegral b * (1-v))
        mixNums v _ b = b
        combineV :: (Value -> Value -> Value) -> ParamMap -> ParamMap -> ParamMap
        combineV f a b = Map.mapWithKey pairUp a
          where pairUp k v | Map.notMember k b = v
                           | otherwise = f v (fromJust $ Map.lookup k b)

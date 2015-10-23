module Sound.Tidal.Transition where

import Sound.Tidal.Dirt
import Sound.Tidal.Stream
import Sound.Tidal.Pattern
import Sound.Tidal.Time
import Sound.Tidal.Params
import Sound.Tidal.Utils

import Control.Concurrent.MVar
import Control.Applicative

import Data.Monoid

transition :: (IO Time) -> MVar (OscPattern, [OscPattern]) -> (Time -> [OscPattern] -> OscPattern) -> OscPattern -> IO ()
transition getNow mv f p =
  do now <- getNow
     ps <- takeMVar mv
     let p' = f now (p:snd ps)
     -- don't put the transition in history, only
     -- the target pattern, or things get overcomplex
     -- (transitions of transitions)
     putMVar mv (p', (p:snd ps))
     return ()

-- Pans the last n versions of the pattern across the field
histpan :: Int -> Time -> [OscPattern] -> OscPattern
histpan _ _ [] = silence
histpan 0 _ _ = silence
histpan n _ ps = stack $ map (\(i,p) -> p # pan (atom $ (fromIntegral i) / (fromIntegral n'))) (enumerate ps')
  where ps' = take n ps
        n' = length ps' -- in case there's fewer patterns than requested

-- generalizing wash to use pattern transformers on fadeout, fadein, and delay
-- to start of transition
superwash :: (Pattern a -> Pattern a) -> (Pattern a -> Pattern a) -> Time -> Time -> Time -> [Pattern a] -> Pattern a
superwash _ _ _ _ _ [] = silence
superwash _ _ _ _ _ (p:[]) = p
superwash fout fin delay dur now (p:p':_) = 
   (playWhen (< (now + delay)) p') <>
   (playWhen (between (now + delay) (now + delay + dur)) $ fout p') <>
   (playWhen (>= (now + delay + dur)) $ fin p)
 where
   between lo hi x = (x >= lo) && (x < hi)

wash :: (Pattern a -> Pattern a) -> Time -> Time -> [Pattern a] -> Pattern a
wash _ _ _ [] = silence
wash _ _ _ (p:[]) = p
wash f t now (p:p':_) = overlay (playWhen (< (now + t)) $ f p') (playWhen (>= (now + t)) p)

-- Increase comb filter to anticipate 'drop' to next pattern
anticipateIn :: Time -> Time -> [OscPattern] -> OscPattern
anticipateIn t now = wash (spread' (stut 8 0.2) (now ~> (slow t $ (toRational . (1-)) <$> envL))) t now

anticipate :: Time -> [OscPattern] -> OscPattern
anticipate = anticipateIn 8

-- Just stop for a bit before playing new pattern
wait :: Time -> Time -> [OscPattern] -> OscPattern
wait t _ [] = silence
wait t now (p:_) = playWhen (>= (nextSam (now+t-1))) p

-- transition at cycle boundary after n cycles
jumpIn' :: Int -> Time -> [OscPattern] -> OscPattern
jumpIn' n now = superwash id id ((nextSam now) - now + (fromIntegral n)) 0 now

-- sharp transition a certain number of cycles in the future
jumpIn :: Int -> Time -> [OscPattern] -> OscPattern
jumpIn n = superwash id id (fromIntegral n) 0

jump :: Time -> [OscPattern] -> OscPattern
jump = jumpIn 0

-- transition at next cycle boundary where cycle mod n == 0
jumpMod :: Int -> Time -> [OscPattern] -> OscPattern
jumpMod n now = jumpIn ((n-1) - ((floor now) `mod` n)) now

-- Degrade the new pattern over time until it goes to nothing
mortal :: Time -> Time -> Time -> [OscPattern] -> OscPattern
mortal _ _ _ [] = silence
mortal lifespan release now (p:_) = overlay (playWhen (<(now+lifespan)) p) (playWhen (>= (now+lifespan)) (fadeOut' (now + lifespan) release p))

dirtSetters :: IO Time -> IO (OscPattern -> IO (), (Time -> [OscPattern] -> OscPattern) -> OscPattern -> IO ())
dirtSetters getNow = do ds <- dirtState
                        return (setter ds, transition getNow ds)

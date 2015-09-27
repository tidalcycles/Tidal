module Sound.Tidal.Transition where

import Sound.Tidal.Dirt
import Sound.Tidal.Stream
import Sound.Tidal.Pattern
import Sound.Tidal.Time
import Sound.Tidal.Params
import Sound.Tidal.Utils

import Control.Concurrent.MVar
import Control.Applicative

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
histpan n _ ps = stack $ map (\(i,p) -> p |+| pan (atom $ (fromIntegral i) / (fromIntegral n'))) (enumerate ps')
  where ps' = take n ps
        n' = length ps' -- in case there's fewer patterns than requested

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

-- Degrade the new pattern over time until it goes to nothing
mortal :: Time -> Time -> Time -> [OscPattern] -> OscPattern
mortal _ _ _ [] = silence
mortal lifespan release now (p:_) = overlay (playWhen (<(now+lifespan)) p) (playWhen (>= (now+lifespan)) (fadeOut' (now + lifespan) release p))

dirtSetters :: IO Time -> IO (OscPattern -> IO (), (Time -> [OscPattern] -> OscPattern) -> OscPattern -> IO ())
dirtSetters getNow = do ds <- dirtState
                        return (setter ds, transition getNow ds)

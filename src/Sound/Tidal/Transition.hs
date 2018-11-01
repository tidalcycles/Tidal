module Sound.Tidal.Transition where

import Control.Concurrent.MVar (readMVar)

import qualified Sound.OSC.FD as O
import qualified Data.Map.Strict as Map

import Sound.Tidal.Control
import Sound.Tidal.Core
import Sound.Tidal.Params (gain, s, pan)
import Sound.Tidal.Parse (p)
import Sound.Tidal.Pattern
import Sound.Tidal.Stream
import Sound.Tidal.Tempo (timeToCycles)
import Sound.Tidal.Utils (enumerate)

transition :: Show a => Stream -> (Time -> [ControlPattern] -> ControlPattern) -> a -> ControlPattern -> IO ()
transition stream f patId pat = do pMap <- readMVar (sPMapMV stream)
                                   let match = Map.lookup (show patId) pMap
                                   run pMap match
  where
    run pMap (Just playState) = do let context = pat:(pattern playState):(history playState)
                                   tempo <- readMVar $ sTempoMV stream
                                   now <- O.time
                                   let c = timeToCycles tempo now
                                   streamReplace stream patId $ f c context
    run _ Nothing = putStrLn $ "No such pattern: " ++ show patId
     

xfadeIn :: Time -> Time -> [ControlPattern] -> ControlPattern
xfadeIn _ _ [] = silence
xfadeIn _ _ (p:[]) = p
xfadeIn t now (p:p':_) = overlay (p |*| gain (now `rotR` (_slow t envEqR))) (p' |*| gain (now `rotR` (_slow t (envEq))))

-- | Pans the last n versions of the pattern across the field
histpan :: Int -> Time -> [ControlPattern] -> ControlPattern
histpan _ _ [] = silence
histpan 0 _ _ = silence
histpan n _ ps = stack $ map (\(i,p) -> p # pan (pure $ (fromIntegral i) / (fromIntegral n'))) (enumerate ps')
  where ps' = take n ps
        n' = length ps' -- in case there's fewer patterns than requested

-- | Just stop for a bit before playing new pattern
wait :: Time -> Time -> [ControlPattern] -> ControlPattern
wait _ _ [] = silence
wait t now (p:_) = filterWhen (>= (nextSam (now+t-1))) p

{-# LANGUAGE NoMonomorphismRestriction #-}
module Sound.Tidal.Dirt where

import Sound.OSC.FD (Datum)
import qualified Data.Map as Map
import Control.Applicative
import Control.Concurrent.MVar
--import Visual
import Data.Colour.SRGB
import Data.Colour.Names
import Data.Hashable
import Data.Bits
import Data.Maybe
import Data.Fixed
import Data.Ratio
import System.Process

import Sound.Tidal.Stream
import Sound.Tidal.OscStream
import Sound.Tidal.Pattern
import Sound.Tidal.Parse
import Sound.Tidal.Params
import Sound.Tidal.Time
import Sound.Tidal.Tempo
import Sound.Tidal.Transition (transition, wash)
import Sound.Tidal.Utils (enumerate)

dirt :: Shape
dirt = Shape {   params = [ s_p,
                            offset_p,
                            begin_p,
                            end_p,
                            speed_p,
                            pan_p,
                            velocity_p,
                            vowel_p,
                            cutoff_p,
                            resonance_p,
                            accelerate_p,
                            shape_p,
                            kriole_p,
                            gain_p,
                            cut_p,
                            delay_p,
                            delaytime_p,
                            delayfeedback_p,
                            crush_p,
                            coarse_p,
                            hcutoff_p,
                            hresonance_p,
                            bandf_p,
                            bandq_p,
                            unit_p,
                            loop_p,
                            n_p
                          ],
                 cpsStamp = True,
                 latency = 0.04
                }

dirtSlang = OscSlang {
  path = "/play",
  timestamp = MessageStamp,
  namedParams = False,
  preamble = []
  }

superDirtSlang = dirtSlang { timestamp = BundleStamp, path = "/play2", namedParams = True }

superDirtBackend port = do
  s <- makeConnection "127.0.0.1" port superDirtSlang
  return $ Backend s

superDirtState port = do
  backend <- superDirtBackend port
  Sound.Tidal.Stream.state backend dirt

dirtBackend = do
  s <- makeConnection "127.0.0.1" 7771 dirtSlang
  return $ Backend s

-- dirtstart name = start "127.0.0.1" 7771 dirt

dirtStream = do
  backend <- dirtBackend
  stream backend dirt

dirtState = do
  backend <- dirtBackend
  Sound.Tidal.Stream.state backend dirt

dirtSetters :: IO Time -> IO (ParamPattern -> IO (), (Time -> [ParamPattern] -> ParamPattern) -> ParamPattern -> IO ())
dirtSetters getNow = do ds <- dirtState
                        return (setter ds, transition getNow ds)

superDirtSetters :: IO Time -> IO (ParamPattern -> IO (), (Time -> [ParamPattern] -> ParamPattern) -> ParamPattern -> IO ())
superDirtSetters getNow = do ds <- superDirtState 57120
                             return (setter ds, transition getNow ds)


superDirts :: [Int]  -> IO [(ParamPattern -> IO (), (Time -> [ParamPattern] -> ParamPattern) -> ParamPattern -> IO ())]
superDirts ports = do (_, getNow) <- bpsUtils
                      states <- mapM (superDirtState) ports
                      return $ map (\state -> (setter state, transition getNow state)) states

-- -- disused parameter..
dirtstream _ = dirtStream

-- doubledirt = do remote <- stream "178.77.72.138" 7777 dirt
--                 local <- stream "192.168.0.102" 7771 dirt
--                 return $ \p -> do remote p
--                                   local p
--                                   return ()


dirtToColour :: ParamPattern -> Pattern ColourD
dirtToColour p = s
  where s = fmap (\x -> maybe black (maybe black datumToColour) (Map.lookup (param dirt "sound") x)) p

showToColour :: Show a => a -> ColourD
showToColour = stringToColour . show

datumToColour :: Value -> ColourD
datumToColour = showToColour

stringToColour :: String -> ColourD
stringToColour s = sRGB (r/256) (g/256) (b/256)
  where i = (hash s) `mod` 16777216
        r = fromIntegral $ (i .&. 0xFF0000) `shiftR` 16;
        g = fromIntegral $ (i .&. 0x00FF00) `shiftR` 8;
        b = fromIntegral $ (i .&. 0x0000FF);

{-
visualcallback :: IO (ParamPattern -> IO ())
visualcallback = do t <- ticker
                    mv <- startVis t
                    let f p = do let p' = dirtToColour p
                                 swapMVar mv p'
                                 return ()
                    return f
-}

--dirtyvisualstream name = do cb <- visualcallback
--                            streamcallback cb "127.0.0.1" "127.0.0.1" name "127.0.0.1" 7771 dirt

pick :: String -> Int -> String
pick name n = name ++ ":" ++ (show n)

{- | Striate is a kind of granulator, for example:

~~~~ {haskell}

d1 $ striate 3 $ sound "ho ho:2 ho:3 hc"

~~~~

This plays the loop the given number of times, but triggering
progressive portions of each sample. So in this case it plays the loop
three times, the first time playing the first third of each sample,
then the second time playing the second third of each sample, etc..
With the highhat samples in the above example it sounds a bit like
reverb, but it isn't really.

You can also use striate with very long samples, to cut it into short
chunks and pattern those chunks. This is where things get towards
granular synthesis. The following cuts a sample into 128 parts, plays
it over 8 cycles and manipulates those parts by reversing and rotating
the loops.

~~~~ {haskell}

d1 $  slow 8 $ striate 128 $ sound "bev"

~~~~

-}
striate :: Int -> ParamPattern -> ParamPattern
striate n p = cat $ map (\x -> off (fromIntegral x) p) [0 .. n-1]
  where off i p = p
                  # begin (atom (fromIntegral i / fromIntegral n))
                  # end (atom (fromIntegral (i+1) / fromIntegral n))

{-|
The `striate'` function is a variant of `striate` with an extra
parameter, which specifies the length of each part. The `striate'`
function still scans across the sample over a single cycle, but if
each bit is longer, it creates a sort of stuttering effect. For
example the following will cut the bev sample into 32 parts, but each
will be 1/16th of a sample long:

~~~~ {haskell}

d1 $ slow 32 $ striate' 32 (1/16) $ sound "bev"

~~~~

Note that `striate` uses the `begin` and `end` parameters
internally. This means that if you're using `striate` (or `striate'`)
you probably shouldn't also specify `begin` or `end`. -}
striate' :: Int -> Double -> ParamPattern -> ParamPattern
striate' n f p = cat $ map (\x -> off (fromIntegral x) p) [0 .. n-1]
  where off i p = p # begin (atom (slot * i) :: Pattern Double) # end (atom ((slot * i) + f) :: Pattern Double)
        slot = (1 - f) / (fromIntegral n)

{- | _not sure what this does_, variant of `striate` -}
striateO :: ParamPattern -> Int -> Double -> ParamPattern
striateO p n o = cat $ map (\x -> off (fromIntegral x) p) [0 .. n-1]
  where off i p = p # begin ((atom $ (fromIntegral i / fromIntegral n) + o) :: Pattern Double) # end ((atom $ (fromIntegral (i+1) / fromIntegral n) + o) :: Pattern Double)

{- | Just like `striate`, but also loops each sample chunk a number of times specified in the second argument.
The primed version is just like `striate'`, where the loop count is the third argument. For example:

@
d1 $ striateL' 3 0.125 4 $ sound "feel sn:2"
@

Like `striate`, these use the `begin` and `end` parameters internally, as well as the `loop` parameter for these versions.
-}
striateL :: Int -> Int -> ParamPattern -> ParamPattern
striateL n l p = striate n p # loop (atom $ fromIntegral l)
striateL' n f l p = striate' n f p # loop (atom $ fromIntegral l)

metronome = slow 2 $ sound (p "[odx, [hh]*8]")

clutchIn :: Time -> Time -> [Pattern a] -> Pattern a
clutchIn _ _ [] = silence
clutchIn _ _ (p:[]) = p
clutchIn t now (p:p':_) = overlay (fadeOut' now t p') (fadeIn' now t p)

clutch :: Time -> [Pattern a] -> Pattern a
clutch = clutchIn 2

xfadeIn :: Time -> Time -> [ParamPattern] -> ParamPattern
xfadeIn _ _ [] = silence
xfadeIn _ _ (p:[]) = p
xfadeIn t now (p:p':_) = overlay (p |*| gain (now ~> (slow t envEqR))) (p' |*| gain (now ~> (slow t (envEq))))

xfade :: Time -> [ParamPattern] -> ParamPattern
xfade = xfadeIn 2

stut :: Integer -> Double -> Rational -> ParamPattern -> ParamPattern
stut steps feedback time p = stack (p:(map (\x -> (((x%steps)*time) ~> (p |*| gain (pure $ scale (fromIntegral x))))) [1..(steps-1)]))
  where scale x
          = ((+feedback) . (*(1-feedback)) . (/(fromIntegral steps)) . ((fromIntegral steps)-)) x

stut' :: Integer -> Time -> (ParamPattern -> ParamPattern) -> ParamPattern -> ParamPattern
stut' steps steptime f p | steps <= 0 = p
                         | otherwise = overlay (f (steptime ~> stut' (steps-1) steptime f p)) p

-- Increase comb filter to anticipate 'drop' to next pattern
anticipateIn :: Time -> Time -> [ParamPattern] -> ParamPattern
anticipateIn t now = wash (spread' (stut 8 0.2) (now ~> (slow t $ (toRational . (1-)) <$> envL))) t now

anticipate :: Time -> [ParamPattern] -> ParamPattern
anticipate = anticipateIn 8

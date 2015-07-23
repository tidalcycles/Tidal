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
import Sound.Tidal.Pattern
import Sound.Tidal.Parse
import Sound.Tidal.Time
import Sound.Tidal.Utils (enumerate)

dirt :: OscShape
dirt = OscShape {path = "/play",
                 params = [ S "sound" Nothing,
                            F "offset" (Just 0),
                            F "begin" (Just 0),
                            F "end" (Just 1),
                            F "speed" (Just 1),
                            F "pan" (Just 0.5),
                            F "velocity" (Just 0),
                            S "vowel" (Just ""),
                            F "cutoff" (Just 0),
                            F "resonance" (Just 0),
                            F "accelerate" (Just 0),
                            F "shape" (Just 0),
                            I "kriole" (Just 0),
                            F "gain" (Just 1),
                            I "cut" (Just (0)),
                            F "delay" (Just (0)),
                            F "delaytime" (Just (-1)),
                            F "delayfeedback" (Just (-1)),
                            F "crush" (Just 0),
                            I "coarse" (Just 0),
                            F "hcutoff" (Just 0),
                            F "hresonance" (Just 0),
                            F "bandf" (Just 0),
                            F "bandq" (Just 0),
                            S "unit" (Just "rate"),
                            I "loop" (Just 1)
                          ],
                 cpsStamp = True,
                 timestamp = MessageStamp,
                 latency = 0.04,
                 namedParams = False,
                 preamble = []
                }

kriole :: OscShape
kriole = OscShape {path = "/trigger",
                 params = [ I "ksymbol" Nothing,
                            F "kpitch" (Just 1)
                          ],
                 cpsStamp = False,
                 timestamp = MessageStamp,
                 latency = 0.04,
                 namedParams = False,
                 preamble = []
                }

dirtstart name = start "127.0.0.1" 7771 dirt
dirtStream = stream "127.0.0.1" 7771 dirt
dirtState = Sound.Tidal.Stream.state "127.0.0.1" 7771 dirt

-- disused parameter..
dirtstream _ = dirtStream

kstream name = stream "127.0.0.1" 6040 kriole

doubledirt = do remote <- stream "178.77.72.138" 7777 dirt
                local <- stream "192.168.0.102" 7771 dirt
                return $ \p -> do remote p
                                  local p
                                  return ()


dirtToColour :: OscPattern -> Pattern ColourD
dirtToColour p = s
  where s = fmap (\x -> maybe black (maybe black datumToColour) (Map.lookup (param dirt "sound") x)) p

showToColour :: Show a => a -> ColourD
showToColour = stringToColour . show

datumToColour :: Datum -> ColourD
datumToColour = showToColour

stringToColour :: String -> ColourD
stringToColour s = sRGB (r/256) (g/256) (b/256)
  where i = (hash s) `mod` 16777216
        r = fromIntegral $ (i .&. 0xFF0000) `shiftR` 16;
        g = fromIntegral $ (i .&. 0x00FF00) `shiftR` 8;
        b = fromIntegral $ (i .&. 0x0000FF);


{-
visualcallback :: IO (OscPattern -> IO ())
visualcallback = do t <- ticker
                    mv <- startVis t
                    let f p = do let p' = dirtToColour p
                                 swapMVar mv p'
                                 return ()
                    return f
-}

--dirtyvisualstream name = do cb <- visualcallback
--                            streamcallback cb "127.0.0.1" "127.0.0.1" name "127.0.0.1" 7771 dirt
                            

sound        = makeS dirt "sound"
offset       = makeF dirt "offset"
begin        = makeF dirt "begin"
end          = makeF dirt "end"
speed        = makeF dirt "speed"
pan          = makeF dirt "pan"
velocity     = makeF dirt "velocity"
vowel        = makeS dirt "vowel"
cutoff       = makeF dirt "cutoff"
resonance    = makeF dirt "resonance"
accelerate   = makeF dirt "accelerate"
shape        = makeF dirt "shape"
gain         = makeF dirt "gain"
delay        = makeF dirt "delay"
delaytime    = makeF dirt "delaytime"
delayfeedback = makeF dirt "delayfeedback"
crush        = makeF dirt "crush"

coarse :: Pattern Int -> OscPattern
coarse       = makeI dirt "coarse"
hcutoff      = makeF dirt "hcutoff"
hresonance   = makeF dirt "hresonance"
bandf        = makeF dirt "bandf"
bandq        = makeF dirt "bandq"
unit         = makeS dirt "unit"
loop         = makeI dirt "loop"

cut :: Pattern Int -> OscPattern
cut = makeI dirt "cut"

ksymbol      = makeF kriole "ksymbol"
kpitch       = makeF kriole "kpitch"


pick :: String -> Int -> String
pick name n = name ++ ":" ++ (show n)

striate :: Int -> OscPattern -> OscPattern
striate n p = cat $ map (\x -> off (fromIntegral x) p) [0 .. n-1]
  where off i p = p 
                  |+| begin (atom (fromIntegral i / fromIntegral n)) 
                  |+| end (atom (fromIntegral (i+1) / fromIntegral n))

striate' :: Int -> Double -> OscPattern -> OscPattern
striate' n f p = cat $ map (\x -> off (fromIntegral x) p) [0 .. n-1]
  where off i p = p |+| begin (atom (slot * i) :: Pattern Double) |+| end (atom ((slot * i) + f) :: Pattern Double)
        slot = (1 - f) / (fromIntegral n)

striateO :: OscPattern -> Int -> Double -> OscPattern
striateO p n o = cat $ map (\x -> off (fromIntegral x) p) [0 .. n-1]
  where off i p = p |+| begin ((atom $ (fromIntegral i / fromIntegral n) + o) :: Pattern Double) |+| end ((atom $ (fromIntegral (i+1) / fromIntegral n) + o) :: Pattern Double)

striateL :: Int -> Int -> OscPattern -> OscPattern
striateL n l p = striate n p |+| loop (atom $ fromIntegral l)
striateL' n f l p = striate' n f p |+| loop (atom $ fromIntegral l)

metronome = slow 2 $ sound (p "[odx, [hh]*8]")

dirtSetters :: IO Time -> IO (OscPattern -> IO (), (Time -> [OscPattern] -> OscPattern) -> OscPattern -> IO ())
dirtSetters getNow = do ds <- dirtState
                        return (setter ds, transition getNow ds)

clutchIn :: Time -> Time -> [Pattern a] -> Pattern a
clutchIn _ _ [] = silence
clutchIn _ _ (p:[]) = p
clutchIn t now (p:p':_) = overlay (fadeOut' now t p') (fadeIn' now t p)

clutch :: Time -> [Pattern a] -> Pattern a
clutch = clutchIn 2

xfadeIn :: Time -> Time -> [OscPattern] -> OscPattern
xfadeIn _ _ [] = silence
xfadeIn _ _ (p:[]) = p
xfadeIn t now (p:p':_) = overlay (p |+| gain (now ~> (slow t envEqR))) (p' |+| gain (now ~> (slow t (envEq))))

xfade :: Time -> [OscPattern] -> OscPattern
xfade = xfadeIn 2

stut :: Integer -> Double -> Rational -> OscPattern -> OscPattern
stut steps feedback time p = stack (p:(map (\x -> (((x%steps)*time) ~> (p |+| gain (pure $ scale (fromIntegral x))))) [1..(steps-1)])) 
  where scale x 
          = ((+feedback) . (*(1-feedback)) . (/(fromIntegral steps)) . ((fromIntegral steps)-)) x

anticipateIn :: Time -> Time -> [OscPattern] -> OscPattern
anticipateIn _ _ [] = silence
anticipateIn _ _ (p:[]) = p
anticipateIn t now (p:p':_) = overlay (playWhen (< (now + t)) $ spread' (stut 8 0.2) (now ~> (slow t $ toRational <$> envL)) p') (playWhen (>= (now + t)) p)

anticipate :: Time -> [OscPattern] -> OscPattern
anticipate = anticipateIn 4

histpan :: Int -> Time -> [OscPattern] -> OscPattern
histpan _ _ [] = silence
histpan 0 _ _ = silence
histpan n _ ps = stack $ map (\(i,p) -> p |+| pan (atom $ (fromIntegral i) / (fromIntegral n'))) (enumerate ps')
  where ps' = take n ps
        n' = length ps' -- in case there's fewer patterns than requested

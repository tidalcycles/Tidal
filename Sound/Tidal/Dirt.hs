{-# LANGUAGE NoMonomorphismRestriction #-}
  
module Sound.Tidal.Dirt where

import Sound.OSC.FD
import qualified Data.Map as Map
import Control.Applicative
import Control.Concurrent.MVar
--import Visual
import Data.Colour.SRGB
import Data.Colour.Names
import Data.Hashable
import Data.Bits
import Data.Maybe
import System.Process

import Sound.Tidal.Stream
import Sound.Tidal.Pattern
import Sound.Tidal.Parse

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
                            F "delayfeedback" (Just (-1))
                          ],
                 timestamp = True
                }


kriole :: OscShape
kriole = OscShape {path = "/trigger",
                 params = [ I "ksymbol" Nothing,
                            F "kpitch" (Just 1)
                          ],
                 timestamp = True
                }

dirtstart name = start "127.0.0.1" 7771 dirt
dirtStream = stream "127.0.0.1" 7771 dirt

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


cut :: Pattern Int -> OscPattern
cut = makeI dirt "cut"

ksymbol      = makeF kriole "ksymbol"
kpitch       = makeF kriole "kpitch"


pick :: String -> Int -> String
pick name n = name ++ "/" ++ (show n)

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

metronome = slow 2 $ sound (p "[odx, [hh]*8]")

interlace :: OscPattern -> OscPattern -> OscPattern
interlace a b = weave 16 (shape $ ((* 0.9) <$> sinewave1)) [a, b]

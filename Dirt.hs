{-# LANGUAGE NoMonomorphismRestriction #-}
  
module Dirt where

import Stream
import Pattern
import Parse
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
                            F "accellerate" (Just 0),
                            F "shape" (Just 0),
                            I "kriole" (Just 0)
                          ],
                 timestamp = True
                }


kriole :: OscShape
kriole = OscShape {path = "/kriole",
                 params = [ S "kdur" Nothing,
                            F "kstart" (Just 0),
                            F "kstop" (Just 0)
                          ],
                 timestamp = True
                }


dirtstream name = stream "127.0.0.1" 7771 dirt
kstream name = stream "127.0.0.1" 7771 kriole

dirtToColour :: OscPattern -> Pattern ColourD
dirtToColour p = s
  where s = fmap (\x -> maybe black (maybe black datumToColour) (Map.lookup (param dirt "sound") x)) p

datumToColour :: Datum -> ColourD
datumToColour = stringToColour . show

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
accellerate  = makeF dirt "accellerate"
shape        = makeF dirt "shape"


kdur         = makeF kriole "kdur"
kstart       = makeF kriole "kstart"
kstop        = makeF kriole "kstop"

pick :: String -> Int -> String
pick name n = name ++ "/" ++ (show n)

striate :: Int -> OscPattern -> OscPattern
striate n p = cat $ map (\x -> off (fromIntegral x) p) [0 .. n-1]
  where off i p = p 
                  |+| begin (atom (fromIntegral i / fromIntegral n)) 
                  |+| end (atom (fromIntegral (i+1) / fromIntegral n))

striate' :: Int -> Double -> OscPattern -> OscPattern
striate' n f p = slowcat $ map (\x -> off (fromIntegral x) p) [0 .. n-1]
  where off i p = p |+| begin (atom (slot * i) :: Pattern Double) |+| end (atom ((slot * i) + f) :: Pattern Double)
        slot = (1 - f) / (fromIntegral n)


striateO :: OscPattern -> Int -> Double -> OscPattern
striateO p n o = cat $ map (\x -> off (fromIntegral x) p) [0 .. n-1]
  where off i p = p |+| begin ((atom $ (fromIntegral i / fromIntegral n) + o) :: Pattern Double) |+| end ((atom $ (fromIntegral (i+1) / fromIntegral n) + o) :: Pattern Double)

metronome = slow 2 $ sound (p "[odx, [hh]*8]")

interlace :: OscPattern -> OscPattern -> OscPattern
interlace a b = weave 16 (shape sinewave1) [a, b]

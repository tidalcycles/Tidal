{-# LANGUAGE NoMonomorphismRestriction #-}
module VideoDirt where

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
import Data.List (elemIndex)

import Sound.Tidal.Stream
import Sound.Tidal.OscStream
import Sound.Tidal.Pattern
import Sound.Tidal.Parse
import Sound.Tidal.Params
import Sound.Tidal.Time
import Sound.Tidal.Tempo
import Sound.Tidal.Transition (transition, wash)
import Sound.Tidal.Utils (enumerate)
import Sound.Tidal.Dirt

(xpos, xpos_p)            = pF "xpos"       (Just 0)
(ypos, ypos_p)            = pF "ypos"       (Just 0)
(zpos, zpos_p)            = pF "zpos"       (Just 0)
(mirror, mirror_p)        = pI "mirror"     (Just 0)
(opacity, opacity_p)      = pF "opacity"    (Just 1)
(blendmode, blendmode_p)  = pI "blendmode"  (Just 0)

videoDirt :: Shape
videoDirt = Shape {   params = [ s_p,
                            n_p,
                            begin_p,
                            end_p,
                            speed_p,
                            xpos_p,
                            ypos_p,
                            zpos_p,
                            mirror_p,
                            opacity_p,
                            blendmode_p
                          ],
                 cpsStamp = True,
                 latency = 0.3
                }

videoDirtSlang = dirtSlang { timestamp = BundleStamp, path = "/playVideo", namedParams = True }

videoDirtBackend = do
  s <- makeConnection "127.0.0.1" 7772 videoDirtSlang
  return $ Backend s (\_ _ _ -> return ())

videoDirtStream = do
  backend <- videoDirtBackend
  stream backend videoDirt

videoDirtState = do
  backend <- videoDirtBackend
  Sound.Tidal.Stream.state backend videoDirt

videoDirtSetters :: IO Time -> IO (ParamPattern -> IO (), (Time -> [ParamPattern] -> ParamPattern) -> ParamPattern -> IO ())
videoDirtSetters getNow = do
                    ds <- videoDirtState
                    return (setter ds, transition getNow ds)


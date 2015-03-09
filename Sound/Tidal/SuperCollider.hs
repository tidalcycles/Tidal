{-# LANGUAGE NoMonomorphismRestriction #-}

module Sound.Tidal.SuperCollider where

import Sound.Tidal.Stream
import Sound.Tidal.Pattern
import Sound.Tidal.Parse
import Sound.OSC.FD

supercollider :: String -> [Param] -> Double -> OscShape
supercollider n ps l = OscShape { 
  -- The OSC path
  path = "/s_new",
preamble = [string n, int32 (-1), int32 1, int32 1],
  namedParams = True,
  params = ps,
  cpsStamp = False,
  timestamp = BundleStamp,
  latency = l
  }

scStream n ps l = do let shape = (supercollider n ps l)
                     sc <- stream "127.0.0.1" 57110 shape
                     return (sc, shape)


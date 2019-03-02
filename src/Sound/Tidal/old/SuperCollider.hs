{-# LANGUAGE NoMonomorphismRestriction #-}

module Sound.Tidal.SuperCollider where

import Sound.Tidal.Stream
import Sound.Tidal.Pattern
import Sound.Tidal.Parse
import Sound.OSC.Datum
import Sound.Tidal.OscStream

supercollider :: [Param] -> Double -> Shape
supercollider ps l = Shape {
  params = ps,
  cpsStamp = False,
  latency = l
  }

scSlang :: String -> OscSlang
scSlang n = OscSlang {
  -- The OSC path
  path = "/s_new",
  preamble = [string n, int32 (-1), int32 1, int32 1],
  namedParams = True,
  timestamp = BundleStamp
  }

scBackend :: String -> IO (Backend a)
scBackend n = do
  s <- makeConnection "127.0.0.1" 57110 (scSlang n)
  return $ Backend s (\_ _ _ -> return ())

scStream :: String -> [Param] -> Double -> IO (ParamPattern -> IO (), Shape)
scStream n ps l = do let shape = (supercollider ps l)
                     backend <- scBackend n
                     sc <- stream backend shape
                     return (sc, shape)

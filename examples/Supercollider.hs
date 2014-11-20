{-# LANGUAGE NoMonomorphismRestriction #-}
  
module SuperCollider where

import Sound.Tidal.Stream
import Sound.Tidal.Pattern
import Sound.Tidal.Parse

sctest :: OscShape
sctest = OscShape { 
                   -- The OSC path
                   path = "/player",
                   -- The OSC parameters, for each give the type (S =
                   -- string, F = floating point, I = integer), a
                   -- name, and it's default value.  The default value
                   -- is either specified as Just then the value, or
                   -- Nothing if there is no default.
                   params = [ S "sample" Nothing,
                              F "pan" (Just 0),
                              F "crackle" (Just 0),
                              F "noise" (Just 0),
                              F "ts" (Just 1),
                              F "browndel" (Just 0),
                              F "rate" (Just 1)
                            ],
                   -- How to timestamp the message - either
                   -- BundleStamp if messages should be wrapped in a
                   -- timestamped bundle (this is how supercollider
                   -- does it), or MessageStamp to have two additional
                   -- parameters inserted at the start of every OSC
                   -- message, being the number of seconds and number
                   -- of microseconds to give the time since epoch. If
                   -- you don't want any timestamp, use NoStamp
                   timestamp = BundleStamp,
                   -- Amount of latency to build in, i.e. messages
                   -- will be sent ahead of time by this number of seconds
                   latency = 0.2
                  }

-- This gives the IP address and port number
sctestStream = stream "127.0.0.1" 57120 sctest

-- You have to make one of these for each parameter, getting the type
-- right. This could be better with lenses..
sample   = makeS sctest "sample"
pan      = makeF sctest "pan"
crackle  = makeF sctest "crackle"
noise    = makeF sctest "noise"
ts       = makeF sctest "ts"
browndel = makeF sctest "browndel"
rate     = makeF sctest "rate"


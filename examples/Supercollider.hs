{-# LANGUAGE NoMonomorphismRestriction #-}
  
module SuperCollider where

-- import Sound.OSC.FD
-- import qualified Data.Map as Map
-- import Control.Applicative
-- import Control.Concurrent.MVar
-- import Visual
-- import Data.Hashable
-- import Data.Bits
-- import Data.Maybe
-- import System.Process

import Sound.Tidal.Stream
import Sound.Tidal.Pattern
import Sound.Tidal.Parse

sctest :: OscShape
sctest = OscShape {path = "/sctest",
                   params = [ S "thing" Nothing,
                              F "wobble" (Just 0),
                              I "pings" (Just 0)
                            ],
                   timestamp = BundleStamp,
                   latency = 0.02
                  }

sctestStream = stream "127.0.0.1" 57120 sctest

-- could be better with lenses
thing        = makeS sctest "thing"
wobble       = makeF sctest "wobble"
pings        = makeI sctest "pings"



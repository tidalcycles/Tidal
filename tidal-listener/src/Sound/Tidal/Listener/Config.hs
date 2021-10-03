
module Sound.Tidal.Listener.Config where

import Data.Default

data ListenerConfig = ListenerConfig {
    listenPort :: Int  -- ^ UDP port for tidal-listener
  , remotePort :: Int  -- ^ UDP port for tidal
  , doDeltaMini:: Bool -- ^ Apply @deltaMini@ to patterns
  } deriving (Eq, Show)

instance Default ListenerConfig where
  def = ListenerConfig {
    listenPort  = 6011
  , remotePort  = 6012
  , doDeltaMini = True
  }

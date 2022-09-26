module Sound.Tidal.StreamTypes where

import qualified Data.Map.Strict as Map
import Sound.Tidal.Types
import Sound.Tidal.Show ()

data PlayState = PlayState {pattern :: ControlSignal,
                            mute :: Bool,
                            solo :: Bool,
                            history :: [ControlSignal]
                           }
               deriving Show

type PatId = String
type PlayMap = Map.Map PatId PlayState

data TickState = TickState {
                    tickArc   :: Arc,
                    tickNudge :: Double
                   }
  deriving Show

module Sound.Tidal.StreamTypes where

import qualified Data.Map.Strict   as Map
import           Sound.Tidal.Show  ()
import           Sound.Tidal.Types

data PlayState = PlayState {pattern :: Signal ValueMap,
                            mute    :: Bool,
                            solo    :: Bool,
                            history :: [Signal ValueMap]
                           }
               deriving Show

type PatId = String
type PlayMap = Map.Map PatId PlayState

data TickState = TickState {
                    tickSpan  :: Span,
                    tickNudge :: Double
                   }
  deriving Show

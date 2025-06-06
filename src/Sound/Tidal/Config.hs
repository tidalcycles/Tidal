module Sound.Tidal.Config where

import Control.Monad (when)
import Data.Int (Int64)
import Foreign.C (CDouble)
import qualified Sound.Tidal.Clock as Clock

{-
    Config.hs - For default Tidal configuration values.
    Copyright (C) 2020, Alex McLean and contributors

    This library is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this library.  If not, see <http://www.gnu.org/licenses/>.
-}

data Config = Config
  { cCtrlListen :: Bool,
    cCtrlAddr :: String,
    cCtrlPort :: Int,
    cCtrlBroadcast :: Bool,
    cVerbose :: Bool,
    cQuantum :: CDouble,
    cBeatsPerCycle :: CDouble,
    cFrameTimespan :: Double,
    cEnableLink :: Bool,
    cSkipTicks :: Int64,
    cProcessAhead :: Double
  }

defaultConfig :: Config
defaultConfig =
  Config
    { cCtrlListen = True,
      cCtrlAddr = "127.0.0.1",
      cCtrlPort = 6010,
      cCtrlBroadcast = False,
      cVerbose = True,
      cFrameTimespan = Clock.clockFrameTimespan Clock.defaultConfig,
      cEnableLink = Clock.clockEnableLink Clock.defaultConfig,
      cProcessAhead = Clock.clockProcessAhead Clock.defaultConfig,
      cSkipTicks = Clock.clockSkipTicks Clock.defaultConfig,
      cQuantum = Clock.clockQuantum Clock.defaultConfig,
      cBeatsPerCycle = Clock.clockBeatsPerCycle Clock.defaultConfig
    }

toClockConfig :: Config -> Clock.ClockConfig
toClockConfig conf =
  Clock.ClockConfig
    { Clock.clockFrameTimespan = cFrameTimespan conf,
      Clock.clockEnableLink = cEnableLink conf,
      Clock.clockProcessAhead = cProcessAhead conf,
      Clock.clockSkipTicks = cSkipTicks conf,
      Clock.clockQuantum = cQuantum conf,
      Clock.clockBeatsPerCycle = cBeatsPerCycle conf
    }

verbose :: Config -> String -> IO ()
verbose c s = when (cVerbose c) $ putStrLn s

setFrameTimespan :: Double -> Config -> Config
setFrameTimespan n c =
  c {cFrameTimespan = n}

setProcessAhead :: Double -> Config -> Config
setProcessAhead n c =
  c
    { cProcessAhead = n
    }

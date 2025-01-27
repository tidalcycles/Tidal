module Sound.Tidal.Stream.Config where

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
    -- cTempoAddr :: String,
    -- cTempoPort :: Int,
    -- cTempoClientPort :: Int,
    cVerbose :: Bool,
    cClockConfig :: Clock.ClockConfig
  }

defaultConfig :: Config
defaultConfig =
  Config
    { cCtrlListen = True,
      cCtrlAddr = "127.0.0.1",
      cCtrlPort = 6010,
      cCtrlBroadcast = False,
      -- cTempoAddr = "127.0.0.1",
      -- cTempoPort = 9160,
      -- cTempoClientPort = 0, -- choose at random
      cVerbose = True,
      cClockConfig = Clock.defaultConfig
    }

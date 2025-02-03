module Sound.Tidal.Stream.Main where

import Control.Concurrent (forkIO, newMVar)
import qualified Data.Map as Map
import qualified Sound.Tidal.Clock as Clock
import Sound.Tidal.Stream.Config
  ( Config (cClockConfig, cCtrlAddr, cCtrlPort),verbose
  )
import Sound.Tidal.Stream.Listen
  ( ctrlResponder,
    openListener
  )
import Sound.Tidal.Stream.Process (doTick)
import Sound.Tidal.Stream.Target (getCXs, superdirtShape)
import Sound.Tidal.Stream.Types (OSC, Stream (..), Target)
import Sound.Tidal.Version (tidal_status_string)
import System.IO (hPutStrLn, stderr)

{-
    Main.hs - Start tidals stream, listen and act on incoming messages
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

-- Start an instance of Tidal with superdirt OSC
startTidal :: Target -> Config -> IO Stream
startTidal target config = startStream config [(target, [superdirtShape])]

-- Start an instance of Tidal
-- Spawns a thread within Tempo that acts as the clock
-- Spawns a thread that listens to and acts on OSC control messages
startStream :: Config -> [(Target, [OSC])] -> IO Stream
startStream config oscmap = do
  sMapMV <- newMVar Map.empty
  pMapMV <- newMVar Map.empty
  globalFMV <- newMVar id

  tidal_status_string >>= verbose config
  verbose config $ "Listening for external controls on " ++ cCtrlAddr config ++ ":" ++ show (cCtrlPort config)
  listen <- openListener config

  cxs <- getCXs config oscmap

  clockRef <- Clock.clocked (cClockConfig config) (doTick sMapMV pMapMV globalFMV cxs)

  let stream =
        Stream
          { sConfig = config,
            sStateMV = sMapMV,
            sClockRef = clockRef,
            -- sLink = abletonLink,
            sListen = listen,
            sPMapMV = pMapMV,
            -- sActionsMV = actionsMV,
            sGlobalFMV = globalFMV,
            sCxs = cxs
          }

  -- Spawn a thread to handle OSC control messages
  _ <- forkIO $ ctrlResponder config stream
  return stream

startMulti :: [Target] -> Config -> IO ()
startMulti _ _ = hPutStrLn stderr "startMulti has been removed, please check the latest documentation on tidalcycles.org"

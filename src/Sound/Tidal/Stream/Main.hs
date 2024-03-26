module Sound.Tidal.Stream.Main where

import qualified Data.Map as Map
import qualified Sound.Tidal.Clock as Clock
import           Control.Concurrent.MVar
import           Control.Concurrent
import           System.IO (hPutStrLn, stderr)


import           Sound.Tidal.Version (tidal_status_string)
import           Sound.Tidal.Stream.Config
import           Sound.Tidal.Stream.Types
import           Sound.Tidal.Stream.Listen
import           Sound.Tidal.Stream.Target
import           Sound.Tidal.Stream.Process
import           Sound.Tidal.Stream.UI

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
       bussesMV <- newMVar []
       globalFMV <- newMVar id

       tidal_status_string >>= verbose config
       verbose config $ "Listening for external controls on " ++ cCtrlAddr config ++ ":" ++ show (cCtrlPort config)
       listen <- openListener config

       cxs <- getCXs config oscmap

       clockRef <- Clock.clocked (cClockConfig config) (doTick sMapMV bussesMV pMapMV globalFMV cxs listen)

       let stream = Stream {sConfig = config,
                            sBusses = bussesMV,
                            sStateMV  = sMapMV,
                            sClockRef = clockRef,
                            -- sLink = abletonLink,
                            sListen = listen,
                            sPMapMV = pMapMV,
                            -- sActionsMV = actionsMV,
                            sGlobalFMV = globalFMV,
                            sCxs = cxs
                           }

       sendHandshakes stream

       -- Spawn a thread to handle OSC control messages
       _ <- forkIO $ ctrlResponder 0 config stream
       return stream

startMulti :: [Target] -> Config -> IO ()
startMulti _ _ = hPutStrLn stderr $ "startMulti has been removed, please check the latest documentation on tidalcycles.org"

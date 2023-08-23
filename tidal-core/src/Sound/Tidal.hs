module Sound.Tidal (module T, tidal_version, tidal_status, tidal_status_string) where

import           Paths_tidal_core

import           Sound.Tidal.Bjorklund       as T
import           Sound.Tidal.Chords          as T
import           Sound.Tidal.Compose         as T
import           Sound.Tidal.Event           as T
import           Sound.Tidal.InstanceHacks   as T
import           Sound.Tidal.Mininotation    as T
-- import           Sound.Tidal.Params          as T
import           Sound.Tidal.Pattern         as T
import           Sound.Tidal.Sequence        as T
import           Sound.Tidal.Show            as T
import           Sound.Tidal.Signal          as T
import           Sound.Tidal.Signal.Input    as T
import           Sound.Tidal.Signal.Random   as T
import           Sound.Tidal.Signal.Waveform as T
import           Sound.Tidal.Span            as T
import           Sound.Tidal.Time            as T
import           Sound.Tidal.Types           as T
import           Sound.Tidal.Utils           as T
import           Sound.Tidal.Value           as T

tidal_version :: String
tidal_version = "2.0.0-pre"

tidal_status :: IO ()
tidal_status = tidal_status_string >>= putStrLn

tidal_status_string :: IO String
tidal_status_string = do datadir <- getDataDir
                         return $ "[TidalCycles version " ++ tidal_version ++ "]\nInstalled in " ++ datadir

module Sound.Tidal.Version where

import Paths_tidal

{-
    Version.hs - For giving the current tidal version.
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

tidal_version :: String
tidal_version = "1.10.0"

tidal_status :: IO ()
tidal_status = tidal_status_string >>= putStrLn 

tidal_status_string :: IO String
tidal_status_string = do datadir <- getDataDir
                         return $ "[TidalCycles version " ++ tidal_version ++ "]\nInstalled in " ++ datadir


{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-
    Simple.hs - Things for making Tidal extra-simple to use, originally made for 8 year olds.
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

module Sound.Tidal.Simple where

import Sound.Tidal.Control (chop, hurry)
import Sound.Tidal.Core ((#), (|*), (<~), silence, rev)
import Sound.Tidal.Params (crush, gain, pan, speed, s)
import Sound.Tidal.ParseBP (parseBP_E)
import Sound.Tidal.Pattern (ControlPattern)
import GHC.Exts ( IsString(..) )

instance {-# OVERLAPPING #-} IsString ControlPattern where
  fromString = s . parseBP_E

crunch :: ControlPattern -> ControlPattern
crunch = (# crush 3)

scratch :: ControlPattern -> ControlPattern
scratch = rev . chop 32

louder :: ControlPattern -> ControlPattern
louder = (|* gain 1.2)

quieter :: ControlPattern -> ControlPattern
quieter = (|* gain 0.8)

silent :: ControlPattern -> ControlPattern
silent = const silence

skip :: ControlPattern -> ControlPattern
skip = (0.25 <~)

left :: ControlPattern -> ControlPattern
left = (# pan 0)

right :: ControlPattern -> ControlPattern
right = (# pan 1)

higher :: ControlPattern -> ControlPattern
higher = (|* speed 1.5)

lower :: ControlPattern -> ControlPattern
lower = (|* speed 0.75)

faster :: ControlPattern -> ControlPattern
faster = hurry 2

slower :: ControlPattern -> ControlPattern
slower = hurry 0.5

module Sound.Tidal.Simple where

import Sound.Tidal.Control (chop, hurry)
import Sound.Tidal.Core ((#), (|*), (<~), silence, rev)
import Sound.Tidal.Params (crush, gain, pan, speed)
import Sound.Tidal.Pattern (ControlPattern)

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

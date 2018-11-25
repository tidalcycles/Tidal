module Sound.Tidal.Simple where

import Sound.Tidal.Context

crunch :: ControlPattern -> ControlPattern
crunch = (# crush 3)

scratch :: ControlPattern -> ControlPattern
scratch = rev . chop 32

louder :: ControlPattern -> ControlPattern
louder = (|*| gain 1.2)

quieter :: ControlPattern -> ControlPattern
quieter = (|*| gain 0.8)

mute :: ControlPattern -> ControlPattern
mute = const silence

jump :: ControlPattern -> ControlPattern
jump = (0.25 <~)

left :: ControlPattern -> ControlPattern
left = (# pan 0)

right :: ControlPattern -> ControlPattern
right = (# pan 1)

higher :: ControlPattern -> ControlPattern
higher = (|*| speed 1.5)

lower :: ControlPattern -> ControlPattern
lower = (|*| speed 0.75)

faster :: ControlPattern -> ControlPattern
faster = hurry 2

slower :: ControlPattern -> ControlPattern
slower = hurry 0.5

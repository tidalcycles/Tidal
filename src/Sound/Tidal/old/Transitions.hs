{-|
Module: Transitions
Description: progressively move from on to another pattern

During live coding evaluation of changes will take effect immediately, that is, if you run:

@
d1 silence
@

Sound from `d1` will mute a soon as possible.

However, if you want to _schedule_ another pattern for playback, this might not be intended.

Transitions are functions that describe _how_ one pattern will transform into another. These range from simple behaviours like, jumping right into the new pattern after a while:

@
t1 (jumpIn 2) $ sound "bd(3,8)"
@

to complex combinations of both the current and the new pattern:

@
t1 anticipate $ slow 4 $ sound "bd sn" # delay "0.5" # room "0.3"
@

-}
module Sound.Tidal.Transitions (
  jump,
  jumpIn,
  jumpIn',
  jumpMod,
  anticipate,
  anticipateIn,
  clutch,
  clutchIn,
  histpan,
  mortal,
  wait,
  xfade,
  xfadeIn) where

import Sound.Tidal.Transition
import Sound.Tidal.Dirt

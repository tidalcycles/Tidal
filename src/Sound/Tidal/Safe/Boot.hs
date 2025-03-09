{-# LANGUAGE NoMonomorphismRestriction #-}
{-
    Safe/Boot.hs - as in BootTidal but in the Op monad
    Copyright (C) 2021 Johannes Waldmann and contributors

    Forked from:
    https://github.com/jwaldmann/safe-tidal-cli/

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
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Sound.Tidal.Safe.Boot where

import Sound.Tidal.Safe.Context
import qualified Sound.Tidal.Transition

-- everyone is missing the tidal :: Stream argument
-- this will be provided by the Reader monad

p = streamReplace

hush = streamHush

list = streamList

mute = streamMute

unmute = streamUnmute

solo = streamSolo

unsolo = streamUnsolo

once = streamOnce

first = streamFirst

asap = once

nudgeAll = streamNudgeAll

all = streamAll

-- |
--  Resets the cycle count back to 0.
--  Useful to make sure a pattern or set of patterns start from the beginning:
--
--  > do
--  >   resetCycles
--  >   d1 $ s "bd hh hh hh"
--  >   d2 $ s "ade" # cut 1
--
--  Cycle count affects all patterns, so if there are any active, all of them will immediately jump to the beginning.
--  @resetCycles@ is also userful in multi-user Tidal.
--
--  Also see 'setCycle', 'getnow'.
resetCycles = streamResetCycles

-- |
--  Adjusts the number of cycles per second, i.e., tempo.
--  Accepts integers, decimals, and fractions.
--
--  The default number of cycles per second is 0.5625, equivalent to 135\/60\/4, i.e.,
--  135 beats per minute if there are 4 beats per cycle.
--
--  Representing cycles per second using fractions has the advantage of being more
--  human-readable and more closely aligned with how tempo is commonly represented
--  in music as beats per minute (bpm). For example, techno has a typical range of
--  120-140 bpm and house has a range of 115-130 bpm. To set the tempo in Tidal to
--  fast house, e.g.,: @setcps (130\/60\/4)@.
--
--  The following sound the same:
--
--  > setcps (130/60/4)
--  > d1 $ n "1" # s "kick kick kick kick"
--
--  and
--
--  > setcps (130/60/1)
--  > d1 $ n "1" # s "kick"
setcps = asap . cps

-- * Transitions

xfade i = transition True (Sound.Tidal.Transition._xfadeIn 4) i

xfadeIn i t = transition True (Sound.Tidal.Transition._xfadeIn t) i

histpan i t = transition True (Sound.Tidal.Transition._histpan t) i

wait i t = transition True (Sound.Tidal.Transition._wait t) i

waitT i f t = transition True (Sound.Tidal.Transition._waitT f t) i

jump i = transition True (Sound.Tidal.Transition._jump) i

jumpIn i t = transition True (Sound.Tidal.Transition._jumpIn t) i

jumpIn' i t = transition True (Sound.Tidal.Transition._jumpIn' t) i

jumpMod i t = transition True (Sound.Tidal.Transition._jumpMod t) i

mortal i lifespan releaseTime = transition True (Sound.Tidal.Transition._mortal lifespan releaseTime) i

interpolate i = transition True (Sound.Tidal.Transition._interpolate) i

interpolateIn i t = transition True (Sound.Tidal.Transition._interpolateIn t) i

clutch i = transition True (Sound.Tidal.Transition._clutch) i

clutchIn i t = transition True (Sound.Tidal.Transition._clutchIn t) i

anticipate i = transition True (Sound.Tidal.Transition._anticipate) i

anticipateIn i t = transition True (Sound.Tidal.Transition._anticipateIn t) i

forId i t = transition False (Sound.Tidal.Transition._mortalOverlay t) i

d1 = p 1 . (|< orbit 0)

d2 = p 2 . (|< orbit 1)

d3 = p 3 . (|< orbit 2)

d4 = p 4 . (|< orbit 3)

d5 = p 5 . (|< orbit 4)

d6 = p 6 . (|< orbit 5)

d7 = p 7 . (|< orbit 6)

d8 = p 8 . (|< orbit 7)

d9 = p 9 . (|< orbit 8)

d10 = p 10 . (|< orbit 9)

d11 = p 11 . (|< orbit 10)

d12 = p 12 . (|< orbit 11)

d13 = p 13

d14 = p 14

d15 = p 15

d16 = p 16

setI = streamSetI

setF = streamSetF

setS = streamSetS

setR = streamSetR

setB = streamSetB

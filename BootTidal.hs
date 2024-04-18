:set -fno-warn-orphans
:set -XMultiParamTypeClasses
:set -XOverloadedStrings
:set prompt ""

default (Signal String, Integer, Double)

-- Import all the boot functions and aliases.
import Sound.Tidal.Boot

-- Create a Tidal Stream with the default settings.
-- Use 'mkTidalWith' to customize these settings.
tidalInst <- mkTidal

-- This orphan instance makes the boot aliases work!
-- It has to go after you define 'tidalInst'.
instance Tidally where tidal = tidalInst

-- You can add your own aliases in this file. Here are some examples:
-- :{
-- let xfade i = transition tidal True (Sound.Tidal.Transition.xfadeIn 4) i
--     xfadeIn i t = transition tidal True (Sound.Tidal.Transition.xfadeIn t) i
--     histpan i t = transition tidal True (Sound.Tidal.Transition.histpan t) i
--     wait i t = transition tidal True (Sound.Tidal.Transition.wait t) i
--     waitT i f t = transition tidal True (Sound.Tidal.Transition.waitT f t) i
--     jump i = transition tidal True (Sound.Tidal.Transition.jump) i
--     jumpIn i t = transition tidal True (Sound.Tidal.Transition.jumpIn t) i
--     jumpIn' i t = transition tidal True (Sound.Tidal.Transition.jumpIn' t) i
--     jumpMod i t = transition tidal True (Sound.Tidal.Transition.jumpMod t) i
--     jumpMod' i t p = transition tidal True (Sound.Tidal.Transition.jumpMod' t p) i
--     mortal i lifespan release = transition tidal True (Sound.Tidal.Transition.mortal lifespan release) i
--     interpolate i = transition tidal True (Sound.Tidal.Transition.interpolate) i
--     interpolateIn i t = transition tidal True (Sound.Tidal.Transition.interpolateIn t) i
--     clutch i = transition tidal True (Sound.Tidal.Transition.clutch) i
--     clutchIn i t = transition tidal True (Sound.Tidal.Transition.clutchIn t) i
--     anticipate i = transition tidal True (Sound.Tidal.Transition.anticipate) i
--     anticipateIn i t = transition tidal True (Sound.Tidal.Transition.anticipateIn t) i
--     forId i t = transition tidal False (Sound.Tidal.Transition.mortalOverlay t) i
-- :}

:set -fwarn-orphans
:set prompt "tidal> "
:set prompt-cont ""

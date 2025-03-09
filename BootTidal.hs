:set -fno-warn-orphans
:set -XMultiParamTypeClasses
:set -XOverloadedStrings
:set prompt ""

-- Import all the boot functions and aliases.
import Sound.Tidal.Boot

default (Pattern String, Integer, Double)

-- Create a Tidal Stream with the default settings.
-- Use 'mkTidalWith' to customize these settings.
tidalInst <- mkTidal

-- This orphan instance makes the boot aliases work!
-- It has to go after you define 'tidalInst'.
instance Tidally where tidal = tidalInst

-- `enableLink` and `disableLink` can be used to toggle synchronisation using the Link protocol.
-- Uncomment the next line to enable Link on startup.
-- enableLink

-- You can also add your own aliases in this file. For example:
-- fastsquizzed pat = fast 2 $ pat # squiz 1.5

:set -fwarn-orphans
:set prompt "tidal> "
:set prompt-cont ""

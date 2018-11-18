:set -XOverloadedStrings
:set prompt ""
:set prompt-cont ""

import Sound.Tidal.Context

tidal <- startTidal (superdirtTarget {oLatency = 0.04}) defaultConfig

let p = streamReplace tidal
    hush = streamHush tidal
    list = streamList tidal
    mute = streamMute tidal
    unmute = streamUnmute tidal
    solo = streamSolo tidal
    unsolo = streamUnsolo tidal
    once = streamOnce tidal False
    asap = streamOnce tidal True
    setcps = asap . cps
    xfade = transition tidal (Sound.Tidal.Transition.xfadeIn 4)
    xfadeIn t = transition tidal (Sound.Tidal.Transition.xfadeIn t)
    histpan t = transition tidal (Sound.Tidal.Transition.histpan t)
    wait t = transition tidal (Sound.Tidal.Transition.wait t)
    waitT f t = transition tidal (Sound.Tidal.Transition.waitT f t)
    jump = transition tidal (Sound.Tidal.Transition.jump)
    jumpIn t = transition tidal (Sound.Tidal.Transition.jumpIn t)
    jumpIn' t = transition tidal (Sound.Tidal.Transition.jumpIn' t)
    jumpMod t = transition tidal (Sound.Tidal.Transition.jumpMod t)
    mortal lifespan release = transition tidal (Sound.Tidal.Transition.mortal lifespan release)
    interpolate = transition tidal (Sound.Tidal.Transition.interpolate)
    interpolateIn t = transition tidal (Sound.Tidal.Transition.interpolateIn t)
    clutch = transition tidal (Sound.Tidal.Transition.clutch)
    clutchIn t = transition tidal (Sound.Tidal.Transition.clutchIn t)
    anticipate = transition tidal (Sound.Tidal.Transition.anticipate)
    anticipateIn t = transition tidal (Sound.Tidal.Transition.anticipateIn t)
    d1 = p "1"
    d2 = p "2"
    d3 = p "3"
    d4 = p "4"
    d5 = p "5"
    d6 = p "6"
    d7 = p "7"
    d8 = p "8"
    d9 = p "9"
    d10 = p "10"
    d11 = p "11"
    d12 = p "12"
    d13 = p "13"
    d14 = p "14"
    d15 = p "15"
    d16 = p "16"

:set prompt "tidal> "

import Sound.Tidal.Context

tidal <- startTidal superdirtTarget defaultConfig

let p = streamReplace tidal
    hush = streamHush tidal
    list = streamList tidal
    mute = streamMute tidal
    unmute = streamUnmute tidal
    once = streamOnce tidal False
    asap = streamOnce tidal True
    setcps = asap . cps
    xfade = transition tidal (Sound.Tidal.Transition.xfadeIn 4)
    xfadeIn t = transition tidal (Sound.Tidal.Transition.xfadeIn t)
    histpan t = transition tidal (Sound.Tidal.Transition.histpan t)
    wait t = transition tidal (Sound.Tidal.Transition.wait t)
    waitT t = transition tidal (Sound.Tidal.Transition.waitT t)
    jump t = transition tidal (Sound.Tidal.Transition.jump t)
    jump' t = transition tidal (Sound.Tidal.Transition.jump' t)
    jumpIn t = transition tidal (Sound.Tidal.Transition.jumpIn t)
    jumpMod t = transition tidal (Sound.Tidal.Transition.jumpMod t)
    mortal t = transition tidal (Sound.Tidal.Transition.mortal t)
    interpolate t = transition tidal (Sound.Tidal.Transition.interpolate t)
    interpolateIn t = transition tidal (Sound.Tidal.Transition.interpolateIn t)
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

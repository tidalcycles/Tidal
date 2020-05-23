:set -XOverloadedStrings
:set prompt ""

import Sound.Tidal.Context

import System.IO (hSetEncoding, stdout, utf8)

hSetEncoding stdout utf8

-- total latency = oLatency + cFrameTimespan
tidal <- startTidal (superdirtTarget {oLatency = 0.2, oAddress = "127.0.0.1", oPort = 57120}) (defaultConfig {cFrameTimespan = 1/20})
:{
{-tidal <- startMulti [superdirtTarget {oLatency = 0.2, oAddress = "127.0.0.1", oPort = 57120
                                     },
                     superdirtTarget {oLatency = 0.2, oAddress = "127.0.0.1", oPort = 2020,
                                      oTimestamp = NoStamp
                                     }
                    ] (defaultConfig {cFrameTimespan = 1/20})-}
:}

:{
let only = (hush >>)
    p = streamReplace tidal
    hush = streamHush tidal
    list = streamList tidal
    mute = streamMute tidal
    unmute = streamUnmute tidal
    unmuteAll = streamUnmuteAll tidal
    solo = streamSolo tidal
    unsolo = streamUnsolo tidal
    once = streamOnce tidal
    first = streamFirst tidal
    asap = once
    nudgeAll = streamNudgeAll tidal
    all = streamAll tidal
    resetCycles = streamResetCycles tidal
    setcps = asap . cps
    xfade i = transition tidal True (Sound.Tidal.Transition.xfadeIn 4) i
    xfadeIn i t = transition tidal True (Sound.Tidal.Transition.xfadeIn t) i
    histpan i t = transition tidal True (Sound.Tidal.Transition.histpan t) i
    wait i t = transition tidal True (Sound.Tidal.Transition.wait t) i
    waitT i f t = transition tidal True (Sound.Tidal.Transition.waitT f t) i
    jump i = transition tidal True (Sound.Tidal.Transition.jump) i
    jumpIn i t = transition tidal True (Sound.Tidal.Transition.jumpIn t) i
    jumpIn' i t = transition tidal True (Sound.Tidal.Transition.jumpIn' t) i
    jumpMod i t = transition tidal True (Sound.Tidal.Transition.jumpMod t) i
    mortal i lifespan release = transition tidal True (Sound.Tidal.Transition.mortal lifespan release) i
    interpolate i = transition tidal True (Sound.Tidal.Transition.interpolate) i
    interpolateIn i t = transition tidal True (Sound.Tidal.Transition.interpolateIn t) i
    clutch i = transition tidal True (Sound.Tidal.Transition.clutch) i
    clutchIn i t = transition tidal True (Sound.Tidal.Transition.clutchIn t) i
    anticipate i = transition tidal True (Sound.Tidal.Transition.anticipate) i
    anticipateIn i t = transition tidal True (Sound.Tidal.Transition.anticipateIn t) i
    forId i t = transition tidal False (Sound.Tidal.Transition.mortalOverlay t) i
    munge i = selectF (cF 0 (show $ 50 + i)) [id, (# (delayfb (cF 0 (show $ 30 + i)) # delayt (select (cF 0 (show $ 70+i)) [1/3, 1/6]) # lock 1 # delay (cF 0 (show $ 20 + i))))] . selectF (cF 0 (show $ 90 + i)) [id, (# (room (cF 0 (show $ 40 + i)) # sz 0.8))] . selectF (cF 0 (show $ 80 + i)) [id, (# djf (cF 0.5 (show $ 40 + i)))]
    d1 = p 1 . munge 1 . (|< orbit 0) 
    d2 = p 2 . munge 2 . (|< orbit 1) 
    d3 = p 3 . munge 3 . (|< orbit 2) 
    d4 = p 4 . munge 4 . (|< orbit 3) 
    d5 = p 5 . munge 5 . (|< orbit 4) 
    d6 = p 6 . munge 6 . (|< orbit 5) 
    d7 = p 7 . munge 7 . (|< orbit 6) 
    d8 = p 8 . munge 8 . (|< orbit 7) 
    d9 = p 9 . (|< orbit 8)
    d10 = p 10 . (|< orbit 9)
    d11 = p 11 . (|< orbit 10)
    d12 = p 12 . (|< orbit 11)
    d13 = p 13
    d14 = p 14
    d15 = p 15
    d16 = p 16
:}

:{
let setI = streamSetI tidal
    setF = streamSetF tidal
    setS = streamSetS tidal
    setR = streamSetR tidal
    setB = streamSetB tidal
:}

:set prompt "tidal> "
:set prompt-cont ""

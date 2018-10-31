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

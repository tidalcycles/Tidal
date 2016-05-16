module Sound.Tidal.Synth where

import Sound.Tidal.Params
import Sound.Tidal.MIDI.Control
import Sound.Tidal.MIDI.Params

synthController :: ControllerShape
synthController = ControllerShape {
  controls = [
    mCC modwheel_p 1,
    mCC pan_p 10,
    mCC expression_p 11,
    mCC sustainpedal_p 64
     ],
  latency = 0.04
  }

synth = toShape synthController

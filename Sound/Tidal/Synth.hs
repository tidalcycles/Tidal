module Sound.Tidal.Synth where

import Sound.Tidal.Params
import Sound.Tidal.MIDI.Control
import Sound.Tidal.MIDI.Params

                          -- mCC "modwheel" 1,
                          -- mCC "balance" 10,
                          -- mCC "expression" 11,
                          -- mCC "sustainpedal" 64

synthController :: ControllerShape
synthController = ControllerShape {
  controls = [
    mCC modwheel_p 1,
    mCC pan_p 10,
    mCC expression_p 11,
    mCC sustainpedal_p 64
     ],
  latency = 0.1
  }

synth = toOscShape synthController

-- oscKeys = toOscShape keys

-- modwheel     = makeF oscKeys "modwheel"
-- balance          = makeF oscKeys "balance"
-- expression   = makeF oscKeys "expression"
-- sustainpedal = makeF oscKeys "sustainpedal"

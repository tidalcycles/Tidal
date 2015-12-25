module Sound.Tidal.MIDI.Params where

import Sound.Tidal.Pattern
import Sound.Tidal.Stream
import Sound.Tidal.Params

dur :: Pattern Double -> ParamPattern
dur = make' VF dur_p
dur_p = F "dur" (Just 0.05)

note :: Pattern Int -> ParamPattern
note = make' VI note_p
note_p = I "note" Nothing

modwheel :: Pattern Double -> ParamPattern
modwheel = make' VF modwheel_p
modwheel_p = F "modwheel" (Just 0)

expression :: Pattern Double -> ParamPattern
expression = make' VF expression_p
expression_p = F "expression" (Just 1)

sustainpedal :: Pattern Double -> ParamPattern
sustainpedal = make' VF sustainpedal_p
sustainpedal_p = F "sustainpedal_p" (Just 0)

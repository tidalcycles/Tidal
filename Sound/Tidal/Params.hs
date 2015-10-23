module Sound.Tidal.Params where

import Sound.Tidal.Stream
import Sound.Tidal.Pattern
import Sound.OSC.FD
import Sound.OSC.Datum
import Data.Map as Map

make' :: (a -> Datum) -> Param -> Pattern a -> OscPattern
make' toOsc par p = fmap (\x -> Map.singleton par (defaultV x)) p
  where defaultV a = Just $ toOsc a

sound :: Pattern String -> OscPattern
sound = make' string sound_p
sound_p = S "sound" Nothing

offset :: Pattern Double -> OscPattern
offset = make' float offset_p
offset_p = F "offset" (Just 0)

begin :: Pattern Double -> OscPattern
begin = make' float begin_p
begin_p = F "begin" (Just 0)

end :: Pattern Double -> OscPattern
end = make' float end_p
end_p = F "end" (Just 1)

speed :: Pattern Double -> OscPattern
speed = make' float speed_p
speed_p = F "speed" (Just 1)

pan :: Pattern Double -> OscPattern
pan = make' float pan_p
pan_p = F "pan" (Just 0.5)

velocity :: Pattern Double -> OscPattern
velocity = make' float velocity_p
velocity_p = F "velocity" (Just 0)

vowel :: Pattern String -> OscPattern
vowel = make' string vowel_p
vowel_p = S "vowel" (Just "")

cutoff :: Pattern Double -> OscPattern
cutoff = make' float cutoff_p
cutoff_p = F "cutoff" (Just 0)

resonance :: Pattern Double -> OscPattern
resonance = make' float resonance_p
resonance_p = F "resonance" (Just 0)

accelerate :: Pattern Double -> OscPattern
accelerate = make' float accelerate_p
accelerate_p = F "accelerate" (Just 0)

shape :: Pattern Double -> OscPattern
shape = make' float shape_p
shape_p = F "shape" (Just 0)

kriole :: Pattern Int -> OscPattern
kriole = make' int32 kriole_p
kriole_p = I "kriole" (Just 0)

gain :: Pattern Double -> OscPattern
gain = make' float gain_p
gain_p = F "gain" (Just 1)

cut :: Pattern Int -> OscPattern
cut = make' int32 cut_p
cut_p = I "cut" (Just (0))

delay :: Pattern Double -> OscPattern
delay = make' float delay_p
delay_p = F "delay" (Just (0))

delaytime :: Pattern Double -> OscPattern
delaytime = make' float delaytime_p
delaytime_p = F "delaytime" (Just (-1))

delayfeedback :: Pattern Double -> OscPattern
delayfeedback = make' float delayfeedback_p
delayfeedback_p = F "delayfeedback" (Just (-1))

crush :: Pattern Double -> OscPattern
crush = make' float crush_p
crush_p = F "crush" (Just 0)

coarse :: Pattern Int -> OscPattern
coarse = make' int32 coarse_p
coarse_p = I "coarse" (Just 0)

hcutoff :: Pattern Double -> OscPattern
hcutoff = make' float hcutoff_p
hcutoff_p = F "hcutoff" (Just 0)

hresonance :: Pattern Double -> OscPattern
hresonance = make' float hresonance_p
hresonance_p = F "hresonance" (Just 0)

bandf :: Pattern Double -> OscPattern
bandf = make' float bandf_p
bandf_p = F "bandf" (Just 0)

bandq :: Pattern Double -> OscPattern
bandq = make' float bandq_p
bandq_p = F "bandq" (Just 0)

unit :: Pattern String -> OscPattern
unit = make' string unit_p
unit_p = S "unit" (Just "rate")

loop :: Pattern Int -> OscPattern
loop = make' int32 loop_p
loop_p = I "loop" (Just 1)


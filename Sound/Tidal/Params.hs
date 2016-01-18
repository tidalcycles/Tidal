module Sound.Tidal.Params where

import Sound.Tidal.Stream
import Sound.Tidal.Pattern
import Data.Map as Map

make' :: (a -> Value) -> Param -> Pattern a -> ParamPattern
make' toValue par p = fmap (\x -> Map.singleton par (defaultV x)) p
  where defaultV a = Just $ toValue a

sound :: Pattern String -> ParamPattern
sound = make' VS sound_p
sound_p = S "sound" Nothing

-- "s" stands for sample, or synth
s :: Pattern String -> ParamPattern
s = make' VS s_p
s_p = S "s" Nothing

-- "n" stands for sample number, or note
n :: Pattern Int -> ParamPattern
n = make' VI n_p
n_p = I "n" Nothing

nudge :: Pattern Double -> ParamPattern
nudge = make' VF nudge_p
nudge_p = (F "nudge" (Just 0))

offset :: Pattern Double -> ParamPattern
offset = make' VF offset_p
offset_p = F "offset" (Just 0)

begin :: Pattern Double -> ParamPattern
begin = make' VF begin_p
begin_p = F "begin" (Just 0)

end :: Pattern Double -> ParamPattern
end = make' VF end_p
end_p = F "end" (Just 1)

speed :: Pattern Double -> ParamPattern
speed = make' VF speed_p
speed_p = F "speed" (Just 1)

pan :: Pattern Double -> ParamPattern
pan = make' VF pan_p
pan_p = F "pan" (Just 0.5)

velocity :: Pattern Double -> ParamPattern
velocity = make' VF velocity_p
velocity_p = F "velocity" (Just 0.5)

vowel :: Pattern String -> ParamPattern
vowel = make' VS vowel_p
vowel_p = S "vowel" (Just "")

cutoff :: Pattern Double -> ParamPattern
cutoff = make' VF cutoff_p
cutoff_p = F "cutoff" (Just 0)

resonance :: Pattern Double -> ParamPattern
resonance = make' VF resonance_p
resonance_p = F "resonance" (Just 0)

accelerate :: Pattern Double -> ParamPattern
accelerate = make' VF accelerate_p
accelerate_p = F "accelerate" (Just 0)

shape :: Pattern Double -> ParamPattern
shape = make' VF shape_p
shape_p = F "shape" (Just 0)

kriole :: Pattern Int -> ParamPattern
kriole = make' VI kriole_p
kriole_p = I "kriole" (Just 0)

gain :: Pattern Double -> ParamPattern
gain = make' VF gain_p
gain_p = F "gain" (Just 1)

cut :: Pattern Int -> ParamPattern
cut = make' VI cut_p
cut_p = I "cut" (Just (0))

delay :: Pattern Double -> ParamPattern
delay = make' VF delay_p
delay_p = F "delay" (Just (0))

delaytime :: Pattern Double -> ParamPattern
delaytime = make' VF delaytime_p
delaytime_p = F "delaytime" (Just (-1))

delayfeedback :: Pattern Double -> ParamPattern
delayfeedback = make' VF delayfeedback_p
delayfeedback_p = F "delayfeedback" (Just (-1))

crush :: Pattern Double -> ParamPattern
crush = make' VF crush_p
crush_p = F "crush" (Just 0)

coarse :: Pattern Int -> ParamPattern
coarse = make' VI coarse_p
coarse_p = I "coarse" (Just 0)

hcutoff :: Pattern Double -> ParamPattern
hcutoff = make' VF hcutoff_p
hcutoff_p = F "hcutoff" (Just 0)

hresonance :: Pattern Double -> ParamPattern
hresonance = make' VF hresonance_p
hresonance_p = F "hresonance" (Just 0)

bandf :: Pattern Double -> ParamPattern
bandf = make' VF bandf_p
bandf_p = F "bandf" (Just 0)

bandq :: Pattern Double -> ParamPattern
bandq = make' VF bandq_p
bandq_p = F "bandq" (Just 0)

unit :: Pattern String -> ParamPattern
unit = make' VS unit_p
unit_p = S "unit" (Just "rate")

loop :: Pattern Int -> ParamPattern
loop = make' VI loop_p
loop_p = I "loop" (Just 1)

channel :: Pattern Int -> ParamPattern
channel = make' VI channel_p
channel_p = I "channel" Nothing

bandq :: Pattern Double -> ParamPattern
room = make' VF room_p
room_p = F "room" Nothing

bandq :: Pattern Double -> ParamPattern
size = make' VF size_p
size_p = F "size" Nothing

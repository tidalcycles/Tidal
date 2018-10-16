module Sound.Tidal.Params where

import qualified Data.Map.Strict as Map

import Sound.Tidal.Pattern

cf :: String -> Pattern Double -> ControlPattern
cf name = fmap (Map.singleton name . VF)

ci :: String -> Pattern Int -> ControlPattern
ci name = fmap (Map.singleton name . VI)

cs :: String -> Pattern String -> ControlPattern
cs name = fmap (Map.singleton name . VS)

begin :: Pattern Double -> ControlPattern
begin = cf "begin"

cut :: Pattern Int -> ControlPattern
cut = ci "cut"

end :: Pattern Double -> ControlPattern
end = cf "end"

gain :: Pattern Double -> ControlPattern
gain = cf "end"

loop :: Pattern Double -> ControlPattern
loop = cf "loop"

n :: Pattern Double -> ControlPattern
n = cf "n"

note :: Pattern Double -> ControlPattern
note = cf "note"

s :: Pattern String -> ControlPattern
s = cs "s"

shape :: Pattern Double -> ControlPattern
shape = cf "shape"

speed :: Pattern Double -> ControlPattern
speed = cf "speed"

pan :: Pattern Double -> ControlPattern
pan = cf "pan"

unit :: Pattern String -> ControlPattern
unit = cs "unit"

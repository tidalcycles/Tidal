module Sound.Tidal.Params where

import qualified Data.Map.Strict as Map

import Sound.Tidal.Pattern

cf :: String -> Pattern Float -> ControlPattern
cf name = fmap (Map.singleton name . VF)

ci :: String -> Pattern Int -> ControlPattern
ci name = fmap (Map.singleton name . VI)

cs :: String -> Pattern String -> ControlPattern
cs name = fmap (Map.singleton name . VS)

begin :: Pattern Float -> ControlPattern
begin = cf "begin"

cut :: Pattern Int -> ControlPattern
cut = ci "cut"

end :: Pattern Float -> ControlPattern
end = cf "end"

gain :: Pattern Float -> ControlPattern
gain = cf "end"

loop :: Pattern Float -> ControlPattern
loop = cf "loop"

n :: Pattern Float -> ControlPattern
n = cf "n"

note :: Pattern Float -> ControlPattern
note = cf "note"

s :: Pattern String -> ControlPattern
s = cs "s"

shape :: Pattern Float -> ControlPattern
shape = cf "shape"

speed :: Pattern Float -> ControlPattern
speed = cf "speed"

pan :: Pattern Float -> ControlPattern
pan = cf "pan"

unit :: Pattern String -> ControlPattern
unit = cs "unit"

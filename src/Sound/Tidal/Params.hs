module Sound.Tidal.Params where

import qualified Data.Map.Strict as Map

import Sound.Tidal.Pattern

pF :: String -> Pattern Double -> ControlPattern
pF name = fmap (Map.singleton name . VF)

pI :: String -> Pattern Int -> ControlPattern
pI name = fmap (Map.singleton name . VI)

pS :: String -> Pattern String -> ControlPattern
pS name = fmap (Map.singleton name . VS)

begin :: Pattern Double -> ControlPattern
begin = pF "begin"

cut :: Pattern Int -> ControlPattern
cut = pI "cut"

end :: Pattern Double -> ControlPattern
end = pF "end"

gain :: Pattern Double -> ControlPattern
gain = pF "gain"

loop :: Pattern Double -> ControlPattern
loop = pF "loop"

n :: Pattern Double -> ControlPattern
n = pF "n"

note :: Pattern Double -> ControlPattern
note = pF "note"

s :: Pattern String -> ControlPattern
s = pS "s"

shape :: Pattern Double -> ControlPattern
shape = pF "shape"

speed :: Pattern Double -> ControlPattern
speed = pF "speed"

pan :: Pattern Double -> ControlPattern
pan = pF "pan"

unit :: Pattern String -> ControlPattern
unit = pS "unit"

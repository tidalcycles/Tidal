module Sound.Tidal.Params where

import qualified Data.Map.Strict as Map

import Sound.Tidal.Pattern

ci :: String -> Pattern Int -> ControlPattern
ci name = fmap (Map.singleton name . VI)

cf :: String -> Pattern Float -> ControlPattern
cf name = fmap (Map.singleton name . VF)

cs :: String -> Pattern String -> ControlPattern
cs name = fmap (Map.singleton name . VS)

s = cs "s"
n = cf "n"
note = cf "note"

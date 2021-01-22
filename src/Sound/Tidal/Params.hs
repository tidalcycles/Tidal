module Sound.Tidal.Params where

{-
    Params.hs - Provides the basic control patterns available to TidalCycles by default
    Copyright (C) 2020, Alex McLean and contributors

    This library is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this library.  If not, see <http://www.gnu.org/licenses/>.
-}

import qualified Data.Map.Strict as Map

import Sound.Tidal.Pattern
import Sound.Tidal.Core ((#))
import Sound.Tidal.Utils
import Data.Maybe (fromMaybe)
import Data.Word (Word8)

-- | group multiple params into one
grp :: [String -> ControlMap] -> Pattern String -> ControlPattern
grp [] _ = empty
grp fs p = splitby <$> p
  where splitby name = Map.unions $ map (\(v, f) -> f v) $ zip (split name) fs
        split :: String -> [String]
        split = wordsBy (==':')

mF :: String -> String -> ControlMap
mF name v = fromMaybe Map.empty $ do f <- readMaybe v
                                     return $ Map.singleton name (VF f)

mI :: String -> String -> ControlMap
mI name v = fromMaybe Map.empty $ do i <- readMaybe v
                                     return $ Map.singleton name (VI i)

mS :: String -> String -> ControlMap
mS name v = Map.singleton name (VS v)

-- | Param makers

pF :: String -> Pattern Double -> ControlPattern
pF name = fmap (Map.singleton name . VF)

pI :: String -> Pattern Int -> ControlPattern
pI name = fmap (Map.singleton name . VI)

pB :: String -> Pattern Bool -> ControlPattern
pB name = fmap (Map.singleton name . VB)
 
pR :: String -> Pattern Rational -> ControlPattern
pR name = fmap (Map.singleton name . VR)

pN :: String -> Pattern Note -> ControlPattern
pN name = fmap (Map.singleton name . VN)

pS :: String -> Pattern String -> ControlPattern
pS name = fmap (Map.singleton name . VS)

pX :: String -> Pattern [Word8] -> ControlPattern
pX name = fmap (Map.singleton name . VX)


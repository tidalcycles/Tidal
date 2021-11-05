module Sound.Tidal.ID (ID(..)) where

{-
    ID.hs - Polymorphic pattern identifiers
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

import GHC.Exts ( IsString(..) )

-- | Wrapper for literals that can be coerced to a string and used as an identifier.
-- | Similar to Show typeclass, but constrained to strings and integers and designed
-- | so that similar cases (such as 1 and "1") convert to the same value.
newtype ID = ID { fromID :: String }

noOv :: String -> a
noOv meth = error $ meth ++ ": not supported for ids"

instance Num ID where
  fromInteger = ID . show
  (+) = noOv "+"
  (*) = noOv "*"
  abs = noOv "abs"
  signum = noOv "signum"
  (-) = noOv "-"

instance IsString ID where
  fromString = ID

{-# LANGUAGE DeriveFunctor #-} 
{-# LANGUAGE DeriveGeneric #-}

-- (c) Alex McLean 2022
-- Shared under the terms of the GNU Public License v. 3.0

module Sound.Tidal.Value where 

import Data.Ratio
import qualified Data.Map.Strict as Map
import           GHC.Generics
import           Data.Typeable (Typeable)
import           Data.Data (Data) -- toConstr

-- | Note is Double, but with a different parser
newtype Note = Note { unNote :: Double }
  deriving (Typeable, Data, Generic, Eq, Ord, Enum, Num, Fractional, Floating, Real, RealFrac)

instance Show Note where
  show n = (show . unNote $ n) ++ "n (" ++ pitchClass ++ octave ++ ")"
    where
      pitchClass = pcs !! mod noteInt 12
      octave = show $ div noteInt 12 + 5
      noteInt = round . unNote $ n
      pcs = ["c", "cs", "d", "ds", "e", "f", "fs", "g", "gs", "a", "as", "b"]

data Value = S String
           | F Double
           | R Rational
           | N Note
       deriving (Show)

type ValueMap = (Map.Map String Value)

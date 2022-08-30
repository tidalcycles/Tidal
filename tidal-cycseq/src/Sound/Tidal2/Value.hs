{-# LANGUAGE DeriveFunctor #-} 

-- (c) Alex McLean 2022
-- Shared under the terms of the GNU Public License v. 3.0

module Sound.Tidal2.Value where 

import Data.Ratio
import qualified Data.Map.Strict as Map

data Value = S String
           | F Double
           | R Rational
       deriving (Show)

type ValueMap = (Map.Map String Value)

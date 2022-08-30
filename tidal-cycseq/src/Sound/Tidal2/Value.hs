{-# LANGUAGE DeriveFunctor #-} 

-- (c) Alex McLean 2022
-- Shared under the terms of the GNU Public License v. 3.0

module Sound.Tidal2.Value where 

import Data.Ratio

data Value = S String
           | F Double
           | R Rational
       deriving (Show)

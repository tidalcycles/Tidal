{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.CoreTest where

import Data.List (sort)
import Data.Ratio
import qualified Data.Map as Map
import Sound.Tidal.Context
import Test.Microspec
import TestUtils
import Prelude hiding ((*>), (<*))

run :: Microspec ()
run =
  describe "Sound.Tidal.Core" $ do
    

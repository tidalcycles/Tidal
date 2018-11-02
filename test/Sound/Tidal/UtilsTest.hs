{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.UtilsTest where

import Test.Microspec

import Prelude hiding ((<*), (*>))

import Data.Ratio
import Data.List (sort)

import Sound.Tidal.Control
import Sound.Tidal.Core
import Sound.Tidal.Params
import Sound.Tidal.Pattern
import Sound.Tidal.Utils

import qualified Data.Map.Strict as Map

run :: Microspec ()
run =
  describe "Sound.Tidal.Utils" $ do
    describe "delta" $ do
      it "subtracts the second element of a tuple from the first" $ do
        property $ delta (3,10) === (7 :: Int)

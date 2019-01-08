{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.ControlTest where

import TestUtils
import Test.Microspec

import Prelude hiding ((<*), (*>))

import qualified Data.Map.Strict as Map

-- import Sound.Tidal.Pattern
import Sound.Tidal.Control
import Sound.Tidal.Core
import Sound.Tidal.Params
import Sound.Tidal.Pattern
import Sound.Tidal.UI

run :: Microspec ()
run =
  describe "Sound.Tidal.Control" $ do
    describe "stutWith" $ do
      it "can mimic stut" $ do
        comparePD (Arc 0 1)
          (stutWith 4 0.25 (# gain 1) $ sound "bd")
          (stut 4 1 0.25 $ sound "bd")
        

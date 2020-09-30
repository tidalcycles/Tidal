{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.ParamsTest where

import Test.Microspec
import TestUtils
import Sound.Tidal.Core
import Sound.Tidal.Params
import Sound.Tidal.Pattern

run :: Microspec ()
run =
  describe "Sound.Tidal.Params" $ do
    describe "VF params" $ do
      it "should parse fractional ratio" $ do
        compareP (Arc 0 1)
          (sound "bd" # delay "e")
          (sound "bd" # delay (1/8))

      it "should parse correctly floating point number" $ do
        compareP (Arc 0 1)
          (sound "bd" # delay "0.5")
          (sound "bd" # delay (1/2))

    describe "VN params" $ do
        it "should parse note value" $ do
          compareP (Arc 0 1)
            (sound "bd" # note "e")
            (sound "bd" # note 4)

        it "should parse correctly floating point number" $ do
          compareP (Arc 0 1)
            (sound "bd" # note "0.5")
            (sound "bd" # note (1/2))


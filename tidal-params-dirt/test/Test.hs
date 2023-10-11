{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Sound.Tidal.Compose     ((#))
import           Sound.Tidal.Params.Dirt
import           Sound.Tidal.TestUtils
import           Sound.Tidal.Types
import           Test.Microspec

main :: IO ()
main = microspec $
  describe "Sound.Tidal.Params" $ do
    describe "VF params" $ do
      it "should parse fractional ratio" $ do
        compareP (Span 0 1)
          (sound "bd" # delay "e")
          (sound "bd" # delay (1/8))

      it "should parse correctly floating point number" $ do
        compareP (Span 0 1)
          (sound "bd" # delay "0.5")
          (sound "bd" # delay (1/2))

    describe "VN params" $ do
        it "should parse note value" $ do
          compareP (Span 0 1)
            (sound "bd" # note "e")
            (sound "bd" # note 4)

        it "should parse n value" $ do
          compareP (Span 0 1)
            (sound "bd" # n "e")
            (sound "bd" # n 4)

        it "should parse correctly floating point number" $ do
          compareP (Span 0 1)
            (sound "bd" # note "0.5")
            (sound "bd" # note (1/2))


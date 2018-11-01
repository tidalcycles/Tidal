{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.UITest where

import TestUtils
import Test.Microspec

import Prelude hiding ((<*), (*>))

import Sound.Tidal.Control
import Sound.Tidal.Core
import Sound.Tidal.Params
import Sound.Tidal.UI

run :: Microspec ()
run =
  describe "Sound.Tidal.UI" $ do
    describe "_chop" $ do
      it "can chop in two bits" $ do
        compareP (0,1)
          (_chop 2 $ s (pure "a"))
          (begin (fastcat [pure 0, pure 0.5]) # end (fastcat [pure 0.5, pure 1]) # (s (pure "a")))
      it "be slowed" $ do
        compareP (0,1)
          (slow 2 $ _chop 2 $ s (pure "a"))
          (begin (pure 0) # end (pure 0.5) # (s (pure "a")))
      it "can chop a chop" $
        property $ compareTol (0,1) (_chop 6 $ s $ pure "a") (_chop 2 $ _chop 3 $ s $ pure "a")

    describe "sometimesBy" $ do
      it "does nothing when set at 0% probability" $ do
        let
          overTimeSpan = (0, 1)
          testMe = sometimesBy 0 (rev) (ps "bd*2 hh sn")
          expectedResult = ps "bd*2 hh sn"
          in
            compareP overTimeSpan testMe expectedResult

      it "applies the 'rev' function sometimes when set at 50% probability" $ pending
      -- do
      --   let
      --     overTimeSpan = (0, 3)
      --     testMe = sometimesBy 0.5 (rev) (ps "bd bd hh sn")
      --     expectedResult = cat ["bd bd hh sd",  "sn hh bd bd", "bd bd hh sn"]
      --     in
      --       compareP overTimeSpan testMe expectedResult

      it "applies the 'rev' function when set at 100% probability" $ do
        let
          overTimeSpan = (0, 1)
          testMe = sometimesBy 1 (rev) (ps "bd*2 hh cp")
          expectedResult = ps "cp hh bd*2"
          in
            compareP overTimeSpan testMe expectedResult

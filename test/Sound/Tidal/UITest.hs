{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.UITest where

import TestUtils
import Test.Microspec

import Prelude hiding ((<*), (*>))

import Sound.Tidal.Control
import Sound.Tidal.Core
import Sound.Tidal.Params
import Sound.Tidal.Pattern
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

      it "applies the 'rev' function when set at 100% probability" $ do
        let
          overTimeSpan = (0, 1)
          testMe = sometimesBy 1 (rev) (ps "bd*2 hh cp")
          expectedResult = ps "cp hh bd*2"
          in
            compareP overTimeSpan testMe expectedResult

    describe "scale" $ do
      describe "scales a pattern to the supplied range" $ do
        describe "from 3 to 4" $ do
          it "at the start of a cycle" $
            (queryArc (Sound.Tidal.UI.scale 3 4 saw) (0, 0)) `shouldBe`
              [(((0, 0), (0, 0)), 3 :: Float)]
          it "at 1/4 of a cycle" $
            (queryArc (Sound.Tidal.UI.scale 3 4 saw) (0.25, 0.25)) `shouldBe`
              [(((0.25, 0.25), (0.25, 0.25)), 3.25 :: Float)]
          it "at 3/4 of a cycle" $
            (queryArc (Sound.Tidal.UI.scale 3 4 saw) (0.75, 0.75)) `shouldBe`
              [(((0.75, 0.75), (0.75, 0.75)), 3.75 :: Float)]

        describe "from -1 to 1" $ do
          it "at 1/2 of a cycle" $
            (queryArc (Sound.Tidal.UI.scale (-1) 1 saw) (0.5, 0.5)) `shouldBe`
              [(((0.5, 0.5), (0.5, 0.5)), 0 :: Float)]

        describe "from 4 to 2" $ do
          it "at the start of a cycle" $
            (queryArc (Sound.Tidal.UI.scale 4 2 saw) (0, 0)) `shouldBe`
              [(((0, 0), (0, 0)), 4 :: Float)]
          it "at 1/4 of a cycle" $
            (queryArc (Sound.Tidal.UI.scale 4 2 saw) (0.25, 0.25)) `shouldBe`
              [(((0.25, 0.25), (0.25, 0.25)), 3.5 :: Float)]
          it "at 3/4 of a cycle" $
            (queryArc (Sound.Tidal.UI.scale 4 2 saw) (0.75, 0.75)) `shouldBe`
              [(((0.75, 0.75), (0.75, 0.75)), 2.5 :: Float)]

        describe "from 10 to 10" $ do
          it "at 1/2 of a cycle" $
            (queryArc (Sound.Tidal.UI.scale 10 10 saw) (0.5, 0.5)) `shouldBe`
              [(((0.5, 0.5), (0.5, 0.5)), 10 :: Float)]

{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.UITest where

import TestUtils
import Test.Microspec

import Prelude hiding ((<*), (*>))

import Sound.Tidal.Pattern
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
      it "can be slowed" $ do
        compareP (0,1)
          (slow 2 $ _chop 2 $ s (pure "a"))
          (begin (pure 0) # end (pure 0.5) # (s (pure "a")))
      it "can chop a chop" $
        property $ compareTol (0,1) (_chop 6 $ s $ pure "a") (_chop 2 $ _chop 3 $ s $ pure "a")

    describe "discretise" $ do
      it "can turn a single event into multiple events" $ do
        compareP (0,3)
          (discretise 4 "x")
          ("x*4" :: Pattern String)
      it "can turn a continuous into multiple discrete events" $ do
        compareP (0,3)
          (discretise 4 saw)
          ("0 0.25 0.5 0.75" :: Pattern Double)
      it "can hold a value over multiple cycles" $ do
        comparePD (0,8)
          (discretise 0.5 saw)
          (slow 2 "0" :: Pattern Double)
      it "holding values over multiple cycles works in combination" $ do
        comparePD (0,8)
          ("0*4" |+ (_discretise (1/8) $ saw))
          ("0*4" :: Pattern Double)

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

    describe "rand" $ do
      it "generates a (pseudo-)random number between zero & one" $ do
        it "at the start of a cycle" $
          (queryArc rand (0, 0)) `shouldBe` [(((0, 0), (0, 0)), 0.5000844 :: Float)]
        it "at 1/4 of a cycle" $
          (queryArc rand (0.25, 0.25)) `shouldBe`
            [(((0.25, 0.25), (0.25, 0.25)), 0.8587171 :: Float)]
        it "at 3/4 of a cycle" $
          (queryArc rand (0.75, 0.75)) `shouldBe`
            [(((0.75, 0.75), (0.75, 0.75)), 0.7277789 :: Float)]

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
    describe "rot" $ do
      it "rotates values in a pattern irrespective of structure" $
        property $ comparePD (0,2)
          (rot 1 "a ~ b c" :: Pattern String)
          ( "b ~ c a" :: Pattern String)
      it "works with negative values" $
        property $ comparePD (0,2)
          (rot (-1) "a ~ b c" :: Pattern String)
          ( "c ~ a b" :: Pattern String)
      it "works with complex patterns" $
        property $ comparePD (0,2)
          (rot (1) "a ~ [b [c ~ d]] [e <f g>]" :: Pattern String)
          ( "b ~ [c [d ~ e]] [<f g> a]" :: Pattern String)

    describe "fix" $ do
      it "can apply functions conditionally" $ do
        compareP (0,1)
          (fix (|+ n 1) (s "sn") (s "bd sn cp" # n 1))
          (s "bd sn cp" # n "1 2 1")
      it "works with complex matches" $ do
        compareP (0,1)
          (fix (|+ n 2) (s "sn" # n 2) (s "bd sn*4 cp" # n "1 2"))
          (s "bd sn*4 cp" # n "1 [1 4] 2")
      it "leaves unmatched controls in place" $ do
        compareP (0,1)
          (fix (|+ n 2) (s "sn" # n 2) (s "bd sn*4 cp" # n "1 2" # speed (sine + 1)))
          (s "bd sn*4 cp" # n "1 [1 4] 2" # speed (sine + 1))

    describe "unfix" $ do
      it "does the opposite of fix" $ do
        compareP (0,1)
          (unfix (|+ n 2) (s "sn" # n 2) (s "bd sn*4 cp" # n "1 2" # speed (sine + 1)))
          (s "bd sn*4 cp" # n "3 [3 2] 4" # speed (sine + 1))

    describe "contrast" $ do
      it "does both fix and unfix" $ do
        compareP (0,1)
          (contrast (|+ n 2) (|+ n 10) (s "sn" # n 2) (s "bd sn*4 cp" # n "1 2" # speed (sine + 1)))
          (s "bd sn*4 cp" # n "11 [11 4] 12" # speed (sine + 1))

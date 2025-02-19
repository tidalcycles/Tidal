{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.StepwiseTest where

import Sound.Tidal.Control (chop, hurry, striate)
import Sound.Tidal.Core ()
import Sound.Tidal.Params (sound)
import Sound.Tidal.ParseBP ()
import Sound.Tidal.Pattern
  ( ArcF (Arc),
    Pattern (tactus),
    fast,
    rev,
  )
import Sound.Tidal.Stepwise (expand, stepcat, stepdrop, steptake)
import Sound.Tidal.UI (inv, iter, linger, segment)
import Test.Hspec
import TestUtils (compareP, firstCycleValues)
import Prelude hiding ((*>), (<*))

run :: Spec
run =
  describe "Sound.Tidal.Stepwise" $ do
    describe "stepcat" $ do
      it "can stepwise cat" $ do
        compareP (Arc 0 8) (stepcat ["a b c", "d e" :: Pattern String]) "a b c d e"
    describe "expand" $ do
      it "can pattern expands" $ do
        compareP (Arc 0 8) (expand "2 1" ("a b c" :: Pattern Char)) "a@2 b@2 c@2 a b c"
    describe "steptake" $ do
      it "can pattern takes" $ do
        compareP (Arc 0 8) (steptake "1 2 3 4" ("a b c d" :: Pattern Char)) "a a b a b c a b c d"
      it "can pattern reverse takes" $ do
        compareP (Arc 0 8) (steptake "-1 -2 -3 -4" ("a b c d" :: Pattern Char)) "d c d b c d a b c d"
    describe "stepdrop" $ do
      it "can pattern drops" $ do
        compareP (Arc 0 8) (stepdrop "0 1 2 3" ("a b c d" :: Pattern Char)) "a b c d a b c a b a"
      it "can pattern reverse drops" $ do
        compareP (Arc 0 8) (stepdrop "0 -1 -2 -3" ("a b c d" :: Pattern Char)) "a b c d b c d c d d"
    describe "tactus is correctly preserved/calculated through transformations" $ do
      it "linger" $ (firstCycleValues <$> tactus (linger 4 "a b c" :: Pattern Char)) `shouldBe` Just [3]
      it "iter" $ (firstCycleValues <$> tactus (iter 4 "a b c" :: Pattern Char)) `shouldBe` Just [3]
      it "fast" $ (firstCycleValues <$> tactus (fast 4 "a b c" :: Pattern Char)) `shouldBe` Just [3]
      it "hurry" $ (firstCycleValues <$> tactus (hurry 4 $ sound "a b c")) `shouldBe` Just [3]
      it "rev" $ (firstCycleValues <$> tactus (rev "a b c" :: Pattern Char)) `shouldBe` Just [3]
      it "segment" $ (firstCycleValues <$> tactus (segment 10 "a" :: Pattern Char)) `shouldBe` Just [10]
      it "invert" $ (firstCycleValues <$> tactus (inv "1 0 1" :: Pattern Bool)) `shouldBe` Just [3]
      it "chop" $ (firstCycleValues <$> tactus (chop 3 $ sound "a b")) `shouldBe` Just [6]
      it "chop" $ (firstCycleValues <$> tactus (striate 3 $ sound "a b")) `shouldBe` Just [6]


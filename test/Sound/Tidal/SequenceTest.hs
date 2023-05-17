{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.SequenceTest where

import           Test.Microspec
import           TestUtils

import           Prelude              hiding ((*>), (<*))

import           Sound.Tidal.Pattern
import           Sound.Tidal.Sequence
import           Sound.Tidal.Types

run :: Microspec ()
run =
  describe "Sound.Tidal.Sequence" $ do
    describe "pairAligned" $ do
      it "Aligns pairs of events" $ do
        (pairAligned In ("10 20", "1 2") :: Sequence (Int, Int))
          `shouldBe`
          Cat [step 1 (10,1), step 1 (20,2)]
    describe "pairAlign" $ do
      it "Can centre two sequences, paired into structure of the first one." $ do
        (pairAlign Centre In "10" "1 2")
          `shouldBe`
          (Cat [Atom 0.5 0 0 Nothing,
                Atom 0.5 0 0.5 $ Just (10,1),
                Atom 0.5 0.5 0 $ Just (10,2),
                Atom 0.5 0 0 Nothing
               ] :: Sequence (Int,Int))

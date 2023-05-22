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
    describe "alignF" $ do
      it "Can align and combine two sequences by Expansion and addition" $ do
        ((alignF Expand In (+) "0 1 2" "10 20") :: Sequence Int)
          `shouldBe`
          (Cat [Atom 1 0 0 $ Just 10, Atom 0.5 0 0.5 $ Just 11, Atom 0.5 0.5 0 $ Just 21,
                Atom 1 0 0 $ Just 22])
      it "Can align and combine subsequences by Expansion and addition" $ do
        ((alignF Expand In (+) "0 [1 2] 3" "10 20") :: Sequence Int)
          `shouldBe`
          (Cat [Atom 1 0 0 $ Just 10, Atom 0.5 0 0 $ Just 11, Atom 0.5 0 0 $ Just 22,
                Atom 1 0 0 $ Just 23])
      it "Can align and combine subsequences by Expansion and addition" $ do
        ((alignF Expand In (+) "0 [1 2] 3" "10 [20 30]") :: Sequence Int)
          `shouldBe`
          (Cat [Atom 1 0 0 $ Just 10, Atom 0.5 0 0 $ Just 11, Atom 0.5 0 0 $ Just 22,
                Atom 0.25 0 0.75 $ Just 23, Atom 0.75 0.25 0 $ Just 33])

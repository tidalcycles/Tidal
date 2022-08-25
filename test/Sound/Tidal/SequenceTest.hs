{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.SequenceTest where

import Sound.Tidal.Sequence

import Test.Microspec

import Prelude hiding ((*>), (<*))

run :: Microspec() 
run =
    describe "Sound.Tidal.Sequence" $ do
        describe "unwrapping a sequence" $
          it "can unwrap sequences" $ unwrap (Sequence [Atom 1 2, Sequence [Atom 1 3, Gap 1]]) ==  (Sequence [Atom 1 2, Atom 1 3, Gap 1])
        describe "plying a sequence" $ 
          it "can ply sequences of rationals" $ do
            unwrap (Sequence [Atom 1 2, Sequence [Atom 1 3, Gap 1]])
            `shouldBe` (Sequence [Atom 1 2, Atom 1 3, Gap 1])

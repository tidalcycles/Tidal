module Sound.Tidal.Test where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Sound.Tidal.Pattern
import Sound.Tidal.Utils
import Data.Ratio

main :: IO ()
main = hspec $ do
  describe "Sound.Tidal.Pattern.eventWhole" $ do
    it "returns the first element of a tuple inside the first element of a tuple" $ do
      property $ (1,2) == eventWhole (((1,2),(3,4)), 5)

  describe "Sound.Tidal.Pattern.eventPart" $ do
    it "returns the second element of a tuple inside the first element of a tuple" $ do
      property $ (3,4) == eventPart (((1,2),(3,4)), 5)

  describe "Sound.Tidal.Utils.delta" $ do
    it "subtracts the second element of a tuple from the first" $ do
      property $ delta (3,10) == 7

  describe "Sound.Tidal.Pattern.atom" $ do
    it "fills a whole cycle" $ do
      property $ query (atom 0) (0,1) == [(((0,1),(0,1)),0)]
    it "returns the part of an atom that you ask for, preserving the whole" $ do
      property $ query (atom 0) (0.25,0.75) == [(((0,1),(0.25,0.75)),0)]
    it "gives correct fragments when you go over cycle boundaries" $ do
      property $ query (atom 0) (0.25,1.25) == [(((0,1),(0.25,1)),0),
                                                (((1,2),(1,1.25)),0)
                                               ]

{-
  describe "Sound.Tidal.Pattern.eventL" $ do
    it "succeeds if the first event 'whole' is shorter" $ do
      property $ eventL (((0,0),(0,1)),"x") (((0,0),(0,1.1)),"x")
    it "fails if the events are the same length" $ do
      property $ not $ eventL (((0,0),(0,1)),"x") (((0,0),(0,1)),"x")
    it "fails if the second event is shorter" $ do
      property $ not $ eventL (((0,0),(0,1)),"x") (((0,0),(0,0.5)),"x")
-}

  describe "Sound.Tidal.Pattern.spanCycles" $ do
    it "leaves a unit cycle intact" $ do
      spanCycles (0,1) `shouldBe` [(0,1)]
      spanCycles (3,4) `shouldBe` [(3,4)]
    it "splits a cycle at cycle boundaries" $ do
      spanCycles (0,1.1) `shouldBe` [(0,1),(1,1.1)]
      spanCycles (1,2.1) `shouldBe` [(1,2),(2,2.1)]
      spanCycles (3 + (1%3),5.1) `shouldBe` [(3+(1%3),4),(4,5),(5,5.1)]



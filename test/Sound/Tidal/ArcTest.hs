{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.ArcTest where

import           Data.Ratio
import           Prelude           hiding ((*>), (<*))
import           Sound.Tidal.Arc
import           Sound.Tidal.Types
import           Test.Microspec

run :: Microspec ()
run =
  describe "Sound.Tidal.Arc" $ do
    describe "splitArcs" $ do
     it "leaves a unit cycle intact" $ do
       it "(0,1)" $ splitArcs (Arc 0 1) `shouldBe` [(Arc 0 1)]
       it "(3,4)" $ splitArcs (Arc 3 4) `shouldBe` [(Arc 3 4)]
     it "splits a cycle at cycle boundaries" $ do
       it "(0,1.1)" $ splitArcs (Arc 0 1.1) `shouldBe` [(Arc 0 1),(Arc 1 1.1)]
       it "(1,2,1)" $ splitArcs (Arc 1 2.1) `shouldBe` [(Arc 1 2),(Arc 2 2.1)]
       it "(3 + (1%3),5.1)" $
          splitArcs (Arc (3 + (1%3)) 5.1) `shouldBe` [(Arc (3+(1%3)) 4),(Arc 4 5),(Arc 5 5.1)]
     it "if start time is greater than end time return empty list" $ do
       let res = splitArcs (Arc 2.3 2.1)
       property $ [] === res
     it "if start time is equal to end time, still return zero-width arc" $ do
       let res = splitArcs (Arc 3 3)
       property $ [Arc 3 3] === res
     it "if start and end time round down to same value return list of (start, end)" $ do
       let res = splitArcs (Arc 2.1 2.3)
       property $ [(Arc 2.1 2.3)] === res
     it "if start time is less than end time and start time does not round down to same value as end time" $ do
       let res = splitArcs (Arc 2.1 3.3)
       property $ [(Arc 2.1 3.0), (Arc 3.0 3.3)] === res

    describe "mapCycle" $ do
      it "Apply a function to the Arc values minus the start value rounded down (sam'), adding both results to sam' to obtain the new Arc value" $ do
        let res = mapCycle (*2) (Arc 3.3 5)
        property $ ((Arc 3.6 7.0) :: Arc) === res

    describe "isIn" $ do
      it "Check given Time is inside a given Arc value, Time is greater than start and less than end Arc values" $ do
        let res = isIn (Arc 2.0 2.8) 2.5
        property $ True === res
      it "Given Time is equal to the Arc start value" $ do
        let res = isIn (Arc 2.0 2.8) 2.0
        property $ True === res
      it "Given Time is less than the Arc start value" $ do
        let res = isIn (Arc 2.0 2.8) 1.4
        property $ False === res
      it "Given Time is greater than the Arc end value" $ do
        let res = isIn (Arc 2.0 2.8) 3.2
        property $ False === res

    describe "sect" $ do
      it "take two Arcs and return - Arc (max of two starts) (min of two ends)" $ do
        let res = sect (Arc 2.2 3) (Arc 2 2.9)
        property $ Arc 2.2 2.9 == res

    describe "hull" $ do
      it "take two Arcs and return - Arc (min of two starts) (max of two ends)" $ do
        let res = hull (Arc 2.2 3) (Arc 2 2.9)
        property $ Arc 2 3 == res

    describe "maybeSect" $ do
      it "Checks if an Arc is within another, returns Just (max $ (fst a1) (fst a2), min $ (snd a1) (snd a2)) if so, otherwise Nothing" $ do
        let res = maybeSect (Arc 2.1 2.4) (Arc 2.4 2.8)
        property $ Nothing === res
      it "if max (fst arc1) (fst arc2) <= min (snd arc1) (snd arc2) return Just (max (fst arc1) (fst arc2), min...)" $ do
        let res = maybeSect (Arc 2 2.8) (Arc 2.4 2.9)
        property $ Just (Arc 2.4 2.8) === res

    describe "timeToCycleArc" $ do
      it "given a Time value return the Arc in which it resides" $ do
        let res = timeToCycleArc 2.2
        property $ (Arc 2.0 3.0) === res

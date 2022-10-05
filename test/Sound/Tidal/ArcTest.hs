{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.ArcTest where

import           Test.Microspec
import           TestUtils

import           Prelude             hiding ((*>), (<*))

import           Data.Ratio

import           Sound.Tidal.Types
import           Sound.Tidal.Arc

import qualified Data.Map.Strict     as Map

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

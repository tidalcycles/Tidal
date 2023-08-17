{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.SpanTest where

import           Data.Ratio
import           Prelude           hiding ((*>), (<*))
import           Sound.Tidal.Show  ()
import           Sound.Tidal.Span
import           Sound.Tidal.Types
import           Test.Microspec

run :: Microspec ()
run =
  describe "Sound.Tidal.Span" $ do
    describe "splitSpans" $ do
     it "leaves a unit cycle intact" $ do
       it "(0,1)" $ splitSpans (Span 0 1) `shouldBe` [(Span 0 1)]
       it "(3,4)" $ splitSpans (Span 3 4) `shouldBe` [(Span 3 4)]
     it "splits a cycle at cycle boundaries" $ do
       it "(0,1.1)" $ splitSpans (Span 0 1.1) `shouldBe` [(Span 0 1),(Span 1 1.1)]
       it "(1,2,1)" $ splitSpans (Span 1 2.1) `shouldBe` [(Span 1 2),(Span 2 2.1)]
       it "(3 + (1%3),5.1)" $
          splitSpans (Span (3 + (1%3)) 5.1) `shouldBe` [(Span (3+(1%3)) 4),(Span 4 5),(Span 5 5.1)]
     it "if start time is greater than end time return empty list" $ do
       let res = splitSpans (Span 2.3 2.1)
       property $ [] === res
     it "if start time is equal to end time, still return zero-width span" $ do
       let res = splitSpans (Span 3 3)
       property $ [Span 3 3] === res
     it "if start and end time round down to same value return list of (start, end)" $ do
       let res = splitSpans (Span 2.1 2.3)
       property $ [(Span 2.1 2.3)] === res
     it "if start time is less than end time and start time does not round down to same value as end time" $ do
       let res = splitSpans (Span 2.1 3.3)
       property $ [(Span 2.1 3.0), (Span 3.0 3.3)] === res

    describe "mapCycle" $ do
      it "Apply a function to the Span values minus the start value rounded down (sam'), adding both results to sam' to obtain the new Span value" $ do
        let res = mapCycle (*2) (Span 3.3 5)
        property $ ((Span 3.6 7.0) :: Span) === res

    -- describe "isIn" $ do
    --   it "Check given Time is inside a given Span value, Time is greater than start and less than end Span values" $ do
    --     let res = isIn (Span 2.0 2.8) 2.5
    --     property $ True === res
    --   it "Given Time is equal to the Span start value" $ do
    --     let res = isIn (Span 2.0 2.8) 2.0
    --     property $ True === res
    --   it "Given Time is less than the Span start value" $ do
    --     let res = isIn (Span 2.0 2.8) 1.4
    --     property $ False === res
    --   it "Given Time is greater than the Span end value" $ do
    --     let res = isIn (Span 2.0 2.8) 3.2
    --     property $ False === res

    describe "sect" $ do
      it "take two Spans and return - Span (max of two starts) (min of two ends)" $ do
        let res = sect (Span 2.2 3) (Span 2 2.9)
        property $ Span 2.2 2.9 == res

    -- describe "hull" $ do
    --   it "take two Spans and return - Span (min of two starts) (max of two ends)" $ do
    --     let res = hull (Span 2.2 3) (Span 2 2.9)
    --     property $ Span 2 3 == res

    describe "maybeSect" $ do
      it "Checks if an Span is within another, returns Just (max $ (fst a1) (fst a2), min $ (snd a1) (snd a2)) if so, otherwise Nothing" $ do
        let res = maybeSect (Span 2.1 2.4) (Span 2.4 2.8)
        property $ Nothing === res
      it "if max (fst span1) (fst span2) <= min (snd span1) (snd span2) return Just (max (fst span1) (fst span2), min...)" $ do
        let res = maybeSect (Span 2 2.8) (Span 2.4 2.9)
        property $ Just (Span 2.4 2.8) === res

    describe "timeToCycle" $ do
      it "given a Time value return the Span in which it resides" $ do
        let res = timeToCycle 2.2
        property $ (Span 2.0 3.0) === res

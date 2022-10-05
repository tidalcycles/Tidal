{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.TypesTest where

import           Test.Microspec
import           TestUtils

import           Prelude             hiding ((*>), (<*))

import           Data.Ratio

import           Sound.Tidal.Types

import qualified Data.Map.Strict     as Map

run :: Microspec ()
run =
  describe "Sound.Tidal.Types" $ do
    describe "Arc" $ do
      it "Arc is a Functor: Apply a given function to the start and end values of an Arc" $ do
        let res = fmap (+1) (Arc 3 5)
        property $ ((Arc 4 6) :: Arc) === res

    describe "Event" $ do
      describe "whole" $ do
        it "returns the whole Arc in an Event" $ do
          property $ (Just $ Arc 1 2) === whole (Event (Metadata []) (Just $ Arc 1 2) (Arc 3 4) 5 :: Event Int)

      describe "active" $ do
        it "returns the active Arc in an Event" $ do
          property $ (Arc 3 4) === active (Event (Metadata []) (Just $ Arc 1 2) (Arc 3 4) 5 :: Event Int)

      describe "value" $ do
        it "returns the event value in an Event" $ do
          property $ 5 === value (Event (Metadata []) (Just $ Arc (1 :: Rational) 2) (Arc 3 4) ( 5 :: Int))
{-
    describe "wholeStart" $ do 
      it "retrieve first element of a tuple, inside first element of a tuple, inside the first of another" $ do 
        property $ 1 === wholeStart (Event (Metadata []) (Just $ Arc 1 2) (Arc 3 4) (5 :: Int))

    describe "wholeStop" $ do
      it "retrieve the end time from the first Arc in an Event" $ do
        property $ 2 === wholeStop (Event (Metadata []) (Just $ Arc 1 2) (Arc 3 4) (5 :: Int))
    describe "eventActiveStart" $ do 
      it "retrieve the start time of the second Arc in an Event" $ do 
        property $ 3 === eventActiveStart (Event (Metadata []) (Just $ Arc 1 2) (Arc 3 4) (5 :: Int))

    describe "eventActiveStop" $ do 
      it "retrieve the end time of the second Arc in an Event" $ do 
        property $ 4 === eventActiveStop (Event (Metadata []) (Just $ Arc 1 2) (Arc 3 4) (5 :: Int))
    
    describe "eventActive" $ do 
      it "retrieve the second Arc in an Event" $ do 
        property $ Arc 3 4 === eventActive (Event (Metadata []) (Just $ Arc 1 2) (Arc 3 4) (5 :: Int))
    
    describe "eventValue" $ do
      it "retrieve the second value from a tuple" $ do 
        property $ 5 === eventValue (Event (Metadata []) (Just $ Arc 1 2) (Arc 3 4) (5 :: Int))

    describe "arcCyclesZW" $ do
      it "if start and end time are equal return list of (start, end)" $ do
        let res = arcCyclesZW (Arc 2.5 2.5)
        property $ [(Arc 2.5 2.5)] === res
      it "if start and end time are not equal call arcCycles (start, end) with same rules as above" $ do
        let res = arcCyclesZW (Arc 2.3 2.1)
        property $ [] === res
      it "if start time is less than end time" $ do
        let res = arcCyclesZW (Arc 2.1 2.3)
        property $ [(Arc 2.1 2.3)] === res
      it "if start time is greater than end time" $ do
        let res = arcCyclesZW (Arc 2.1 3.3)
        property $ [(Arc 2.1 3.0), (Arc 3.0 3.3)] === res

    describe "toTime" $ do
      it "Convert a number of type Real to a Time value of type Rational, Int test" $ do
        let res = toTime (3 :: Int)
        property $ (3 % 1 :: Time) === res
      it "Convert a number of type Double to a Time value of type Rational" $ do
        let res = toTime (3.2 :: Double)
        property $ (3602879701896397 % 1125899906842624 :: Time) === res
-}

{-


    describe "getI" $ do 
      it "get Just value when Int value is supplied" $ do
        let res = getI (VI 3)
        property $ (Just 3) === res
      it "get floored value when float value is supplied" $ do
        let res = getI (VF 3.5)
        property $ (Just 3) === res
      it "get if String value is supplied" $ do
        let res = getI (VS "3")
        property $ Nothing === res

    describe "getF" $ do 
     it "get Just value when Float value is supplied" $ do
       let res = getF (VF 3)
       property $ (Just 3.0) === res
     it "get converted value if Int value is supplied" $ do
       let res = getF (VI 3)
       property $ (Just 3.0) === res

    describe "getS" $ do 
     it "get Just value when String value is supplied" $ do
       let res = getS (VS "Tidal")
       property $ (Just "Tidal") === res
     it "get Nothing if Int value is not supplied" $ do
       let res = getS (VI 3) 
       property $ Nothing === res

    describe "filterValues" $ do 
     it "remove Events above given threshold" $ do 
       let fil = filterValues (<2) $ fastCat [atom 1, atom 2, atom 3] :: Signal Time 
       let res = queryArc fil (Arc 0.5 1.5)
       property $ fmap toEvent [(((1, 4%3), (1, 4%3)), 1%1)] === res

     it "remove Events below given threshold" $ do 
       let fil = filterValues (>2) $ fastCat [atom 1, atom 2, atom 3] :: Signal Time 
       let res = queryArc fil (Arc 0.5 1.5)
       property $ fmap toEvent [(((2%3, 1), (2%3, 1)), 3%1)] === res

    describe "filterWhen" $ do 
      it "filter below given threshold" $ do 
        let fil = filterWhen (<0.5) $ struct "t*4" $ (tri :: Signal Double) + 1
        let res = queryArc fil (Arc 0.5 1.5)
        property $ [] === res

      it "filter above given threshold" $ do 
        let fil = stripMetadata $ filterWhen (>0.5) $ struct "t*4" $ (tri :: Signal Double) + 1
        let res = queryArc fil (Arc 0.5 1.5)
        property $ fmap toEvent [(((3%4, 1), (3%4, 1)), 1.25), (((1, 5%4), (1, 5%4)), 1.25), (((5%4, 3%2), (5%4, 3%2)), 1.75)] === res

    describe "compressArc" $ do
      it "return empty if start time is greater than end time" $ do 
        let res = queryArc (compressArc (Arc 0.8 0.1) (fast "1 2" "3 4" :: Signal Time) ) (Arc 1 2)
        property $ [] === res

      it "return empty if start time or end time are greater than 1" $ do 
        let res = queryArc (compressArc (Arc 0.1 2) (fast "1 2" "3 4" :: Signal Time)) (Arc 1 2)
        property $ [] === res

      it "return empty if start or end are less than zero" $ do
        let res = queryArc (compressArc (Arc (-0.8) 0.1) (fast "1 2" "3 4" :: Signal Time)) (Arc 1 2)
        property $ [] === res
      
      it "otherwise compress difference between start and end values of Arc" $ do
        let p = fast "1 2" "3 4" :: Signal Time
        let res = queryArc (stripMetadata $ compressArc (Arc 0.2 0.8) p) (Arc 0 1)
        let expected = fmap toEvent [(((1%5, 1%2), (1%5, 1%2)), 3%1), (((1%2, 13%20), (1%2, 13%20)), 3%1), (((13%20, 4%5), (13%20, 4%5)), 4%1)]
        property $ expected === res
        

    -- pending "Sound.Tidal.Signal.eventL" $ do
    --  it "succeeds if the first event 'whole' is shorter" $ do
    --    property $ eventL (Event (Metadata []) (Just $ Arc 0,0),(Arc 0 1)),"x") (((0 0) (Arc 0 1.1)) "x")
    --  it "fails if the events are the same length" $ do
    --    property $ not $ eventL (Event (Metadata []) (Just $ Arc 0,0),(Arc 0 1)),"x") (((0 0) (Arc 0 1)) "x")
    --  it "fails if the second event is shorter" $ do
    --    property $ not $ eventL (Event (Metadata []) (Just $ Arc 0,0),(Arc 0 1)),"x") (((0 0) (Arc 0 0.5)) "x")
-}

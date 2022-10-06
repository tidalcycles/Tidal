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

        

    -- pending "Sound.Tidal.Signal.eventL" $ do
    --  it "succeeds if the first event 'whole' is shorter" $ do
    --    property $ eventL (Event (Metadata []) (Just $ Arc 0,0),(Arc 0 1)),"x") (((0 0) (Arc 0 1.1)) "x")
    --  it "fails if the events are the same length" $ do
    --    property $ not $ eventL (Event (Metadata []) (Just $ Arc 0,0),(Arc 0 1)),"x") (((0 0) (Arc 0 1)) "x")
    --  it "fails if the second event is shorter" $ do
    --    property $ not $ eventL (Event (Metadata []) (Just $ Arc 0,0),(Arc 0 1)),"x") (((0 0) (Arc 0 0.5)) "x")
-}

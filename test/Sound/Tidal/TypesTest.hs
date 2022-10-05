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

-}

{-
    describe "mapCycle" $ do
      it "Apply a function to the Arc values minus the start value rounded down (sam'), adding both results to sam' to obtain the new Arc value" $ do
        let res = mapCycle (*2) (Arc 3.3 5)
        property $ ((Arc 3.6 7.0) :: Arc) === res

    describe "toTime" $ do
      it "Convert a number of type Real to a Time value of type Rational, Int test" $ do
        let res = toTime (3 :: Int)
        property $ (3 % 1 :: Time) === res
      it "Convert a number of type Double to a Time value of type Rational" $ do
        let res = toTime (3.2 :: Double)
        property $ (3602879701896397 % 1125899906842624 :: Time) === res

    describe "cyclePos" $ do
      it "Subtract a Time value from its value rounded down (the start of the cycle)" $ do
        let res = cyclePos 2.6
        property $ (0.6 :: Time) === res
      it "If no difference between a given Time and the start of the cycle" $ do
        let res = cyclePos 2
        property $ (0.0 :: Time) === res

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

    describe "onsetIn" $ do
      it "If the beginning of an Event is within a given Arc, same rules as 'isIn'" $ do 
         let res = onsetIn (Arc 2.0 2.8) (Event (Metadata []) (Just $ Arc 2.2 2.7) (Arc 3.3 3.8) (5 :: Int))
         property $ True === res 
      it "Beginning of Event is equal to beggining of given Arc" $ do 
         let res = onsetIn (Arc 2.0 2.8) (Event (Metadata []) (Just $ Arc 2.0 2.7) (Arc 3.3 3.8) (5 :: Int))
         property $ True === res 
      it "Beginning of an Event is less than the start of the Arc" $ do 
         let res = onsetIn (Arc 2.0 2.8) (Event (Metadata []) (Just $ Arc 1.2 1.7) (Arc 3.3 3.8) (5 :: Int))
         property $ False === res
      it "Start of Event is greater than the start of the given Arc" $ do 
         let res = onsetIn (Arc 2.0 2.8) (Event (Metadata []) (Just $ Arc 3.1 3.5) (Arc 4.0 4.6) (5 :: Int))
         property $ False === res

    describe "subArc" $ do
      it "Checks if an Arc is within another, returns Just (max $ (fst a1) (fst a2), min $ (snd a1) (snd a2)) if so, otherwise Nothing" $ do       
        let res = subArc (Arc 2.1 2.4) (Arc 2.4 2.8)
        property $ Nothing === res
      it "if max (fst arc1) (fst arc2) <= min (snd arc1) (snd arc2) return Just (max (fst arc1) (fst arc2), min...)" $ do
        let res = subArc (Arc 2 2.8) (Arc 2.4 2.9)
        property $ Just (Arc 2.4 2.8) === res

    describe "timeToCycleArc" $ do
      it "given a Time value return the Arc in which it resides" $ do
        let res = timeToCycleArc 2.2 
        property $ (Arc 2.0 3.0) === res

    describe "cyclesInArc" $ do 
      it "Return a list of cycles in a given arc, if start is greater than end return empty list" $ do 
        let res = cyclesInArc (Arc 2.4 2.2)
        property $ ([] :: [Int]) === res
      it "If start value of Arc is equal to end value return list with start value rounded down" $ do
        let res = cyclesInArc (Arc 2.4 2.4)
        property $ ([2] :: [Int]) === res
      it "if start of Arc is less than end return list of start rounded down to end rounded up minus one" $ do
        let res = cyclesInArc (Arc 2.2 4.5)
        property $ ([2,3,4] :: [Int]) === res  

    describe "cycleArcsInArc" $ do
      it "generates a list of Arcs based on the cycles found within a given a Arc" $ do
       let res = cycleArcsInArc (Arc 2.2 4.5) 
       property $ [(Arc 2.0 3.0), (Arc 3.0 4.0), (Arc 4.0 5.0)] === res

    describe "isAdjacent" $ do
      it "if the given Events are adjacent actives of the same whole" $ do 
        let res = isAdjacent (Event (Metadata []) (Just $ Arc 1 2) (Arc 3 4) 5) (Event (Metadata []) (Just $ Arc 1 2) (Arc 4 3) (5 :: Int))
        property $ True === res 
      it "if first Arc of of first Event is not equal to first Arc of second Event" $ do
        let res = isAdjacent (Event (Metadata []) (Just $ Arc 1 2) (Arc 3 4) 5) (Event (Metadata []) (Just $ Arc 7 8) (Arc 4 3) (5 :: Int))
        property $ False === res  
      it "if the value of the first Event does not equal the value of the second Event" $ do 
        let res = isAdjacent (Event (Metadata []) (Just $ Arc 1 2) (Arc 3 4) 5) (Event (Metadata []) (Just $ Arc 1 2) (Arc 4 3) (6 :: Int))
        property $ False === res 
      it "second value of second Arc of first Event not equal to first value of second Arc in second Event..." $ do
        let res = isAdjacent (Event (Metadata []) (Just $ Arc 1 2) (Arc 3 4) 5) (Event (Metadata []) (Just $ Arc 1 2) (Arc 3 4) (5 :: Int))
        property $ False === res 

    describe "defragActives" $ do 
      it "if empty list with no events return empty list" $ do 
        let res = defragActives ([] :: [Event Int]) 
        property $ [] === res
      it "if list consists of only one Event return it as is" $ do 
        let res = defragActives [(Event (Metadata []) (Just $ Arc 1 2) (Arc 3 4) (5 :: Int))]
        property $ [Event (Metadata []) (Just $ Arc 1 2) (Arc 3 4) (5 :: Int)] === res 
      it "if list contains adjacent Events return list with Actives combined" $ do 
        let res = defragActives [(Event (Metadata []) (Just $ Arc 1 2) (Arc 3 4) (5 :: Int)), (Event (Metadata []) (Just $ Arc 1 2) (Arc 4 3) (5 :: Int))]
        property $ [(Event (Metadata []) (Just $ Arc 1 2) (Arc 3 4) 5)] === res
      it "if list contains more than one Event none of which are adjacent, return List as is" $ do 
        let res = defragActives [(Event (Metadata []) (Just $ Arc 1 2) (Arc 3 4) 5), (Event (Metadata []) (Just $ Arc 7 8) (Arc 4 3) (5 :: Int))]
        property $ [Event (Metadata []) (Just $ Arc 1 2) (Arc 3 4) 5, Event (Metadata []) (Just $ Arc 7 8) (Arc 4 3) (5 :: Int)] === res

    describe "sect" $ do 
      it "take two Arcs and return - Arc (max of two starts) (min of two ends)" $ do
        let res = sect (Arc 2.2 3) (Arc 2 2.9)
        property $ Arc 2.2 2.9 == res

    describe "hull" $ do 
      it "take two Arcs anre return - Arc (min of two starts) (max of two ends)" $ do
        let res = hull (Arc 2.2 3) (Arc 2 2.9) 
        property $ Arc 2 3 == res

    describe "withResultArc" $ do 
     it "apply given function to the Arcs" $ do
      let p = withResultArc (+5) (stripMetadata $ fast "1 2" "3 4" :: Signal Int) 
      let res = queryArc p (Arc 0 1)
      property $ res === fmap toEvent [(((5, 11%2), (5, 11%2)), 3), (((11%2, 23%4), (11%2, 23%4)), 3), (((23%4, 6), (23%4, 6)), 4)]

    describe "applyFIS" $ do 
      it "apply Float function when value of type VF" $ do 
        let res = applyFIS (+1) (+1) (++ "1") (VF 1)
        property $ (VF 2.0) === res
      it "apply Int function when value of type VI" $ do 
        let res = applyFIS (+1) (+1) (++ "1") (VI 1)
        property $ (VI 2) === res
      it "apply String function when value of type VS" $ do
        let res = applyFIS (+1) (+1) (++ "1") (VS "1")
        property $ (VS "11") === res 

    describe "fNum2" $ do
      it "apply Int function for two Int values" $ do 
        let res = fNum2 (+) (+) (VI 2) (VI 3)
        property $ (VI 5) === res 
      it "apply float function when given two float values" $ do 
        let res = fNum2 (+) (+) (VF 2) (VF 3)
        property $ (VF 5.0) === res 
      it "apply float function when one float and one int value given" $ do
        let res = fNum2 (+) (+) (VF 2) (VI 3) 
        property $ (VF 5.0) === res 

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

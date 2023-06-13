{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.EventTest where

import           Prelude                  hiding ((*>), (<*))
import           Sound.Tidal.Show         ()
import           Sound.Tidal.Signal.Event
import           Sound.Tidal.Types
import           Test.Microspec

-- import qualified Data.Map.Strict     as Map

run :: Microspec ()
run =
  describe "Sound.Tidal.Event" $ do
    describe "eventHasOnset" $ do
      it "return True when the start values of the two arcs in an event are equal" $ do
        let ev = (Event (Metadata []) (Just $ Arc 1 2) (Arc 1 3) (4 :: Int))
        property $ True === eventHasOnset ev
      it "return False when the start values of the two arcs in an event are not equal" $ do
        let ev = (Event (Metadata []) (Just $ Arc 1 2) (Arc 3 4) (5 :: Int))
        property $ False === eventHasOnset ev

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

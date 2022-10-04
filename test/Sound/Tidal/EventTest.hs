{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.EventTest where

import           Test.Microspec
import           TestUtils

import           Prelude             hiding ((*>), (<*))

import           Data.Ratio

import           Sound.Tidal.Types
import           Sound.Tidal.Signal.Event

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

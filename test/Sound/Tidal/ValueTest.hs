{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.ValueTest where

import           Test.Microspec
import           TestUtils

import           Prelude             hiding ((*>), (<*))

import           Data.Ratio

import           Sound.Tidal.Types
import           Sound.Tidal.Value

import qualified Data.Map.Strict     as Map

run :: Microspec ()
run =
  describe "Sound.Tidal.Signal.Value" $ do
    describe "applyFIRS" $ do 
      it "apply Float function when value of type VF" $ do 
        let res = applyFIRS (+1) (+1) (+1) (++ "1") (VF 1)
        property $ (VF 2.0) === res
      it "apply Int function when value of type VI" $ do 
        let res = applyFIRS (+1) (+1) (+1) (++ "1") (VI 1)
        property $ (VI 2) === res
      it "apply Int function when value of type VR" $ do 
        let res = applyFIRS (+1) (+1) (+1) (++ "1") (VR 1)
        property $ (VR 2) === res
      it "apply String function when value of type VS" $ do
        let res = applyFIRS (+1) (+1) (+1) (++ "1") (VS "1")
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

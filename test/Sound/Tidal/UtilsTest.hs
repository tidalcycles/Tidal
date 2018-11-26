{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.UtilsTest where

import Test.Microspec

import Prelude hiding ((<*), (*>))

import Sound.Tidal.Utils

run :: Microspec ()
run =
  describe "Sound.Tidal.Utils" $ do
    describe "delta" $ do
      it "subtracts the second element of a tuple from the first" $ do
        property $ delta (3,10) === (7 :: Int)

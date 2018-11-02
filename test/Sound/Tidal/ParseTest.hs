{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.ParseTest where

import TestUtils
import Test.Microspec

import Prelude hiding ((<*), (*>))

import Sound.Tidal.Core
import Sound.Tidal.Pattern

run :: Microspec ()
run =
  describe "Sound.Tidal.Parse" $ do
    describe "p" $ do
      it "can parse things" $ do
        compareP (0,5)
          ("a b c" :: Pattern String)
          (fastCat ["a", "b", "c"])

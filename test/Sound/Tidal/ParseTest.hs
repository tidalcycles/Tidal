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
    describe "parseBP_E" $ do
      it "can parse strings" $ do
        compareP (Arc 0 12)
          ("a b c" :: Pattern String)
          (fastCat ["a", "b", "c"])
      it "can parse ints" $ do
        compareP (Arc 0 2)
          ("0 1 2 3 4 5 6 7 8 0 10 20 30 40 50" :: Pattern Int)
          (fastCat $ map (pure . read) $ words "0 1 2 3 4 5 6 7 8 0 10 20 30 40 50")
      it "can alternate with <>" $ do
        compareP (Arc 0 2)
          ("a <b c>" :: Pattern String)
          (cat [fastCat ["a", "b"], fastCat ["a", "c"]])
      it "can slow with /" $ do
        compareP (Arc 0 2)
          ("a/2" :: Pattern String)
          (slow 2 $ "a")
      it "can speed up with *" $ do
        compareP (Arc 0 2)
          ("a*8" :: Pattern String)
          (fast 8 "a")
      it "can do polymeter with {}" $ do
        compareP (Arc 0 2)
          ("{a b, c d e}" :: Pattern String)
          (stack [fastcat [pure "a", pure "b"], slow 1.5 $ fastcat [pure "c", pure "d", pure "e"]])
      it "can parse a chord" $ do
        compareP (Arc 0 2)
          ("'major" :: Pattern Int)
          ("[0,4,7]")
      it "can parse two chords" $ do
        compareP (Arc 0 2)
          ("'major 'minor" :: Pattern Int)
          ("[0,4,7] [0,3,7]")
      it "can parse c chords" $ do
        compareP (Arc 0 2)
          ("'major 'minor 'dim7" :: Pattern Int)
          ("c'major c'minor c'dim7")
      it "can parse various chords" $ do
        compareP (Arc 0 2)
          ("c'major e'minor f'dim7" :: Pattern Int)
          ("c e f" + "'major 'minor 'dim7")


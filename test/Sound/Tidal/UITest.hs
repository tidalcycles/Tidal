{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.UITest where

import TestUtils
import Test.Microspec

import Prelude hiding ((<*), (*>))

import Sound.Tidal.Pattern
import Sound.Tidal.Control
import Sound.Tidal.Core
import Sound.Tidal.Params
import Sound.Tidal.UI

run :: Microspec ()
run =
  describe "Sound.Tidal.UI" $ do
    describe "_chop" $ do
      it "can chop in two bits" $ do
        compareP (0,1)
          (_chop 2 $ s (pure "a"))
          (begin (fastcat [pure 0, pure 0.5]) # end (fastcat [pure 0.5, pure 1]) # (s (pure "a")))
      it "can be slowed" $ do
        compareP (0,1)
          (slow 2 $ _chop 2 $ s (pure "a"))
          (begin (pure 0) # end (pure 0.5) # (s (pure "a")))
      it "can chop a chop" $
        property $ compareTol (0,1) (_chop 6 $ s $ pure "a") (_chop 2 $ _chop 3 $ s $ pure "a")

    describe "discretise" $ do
      it "can turn a single event into multiple events" $ do
        compareP (0,3)
          (discretise 4 "x")
          ("x*4" :: Pattern String)
      it "can turn a continuous into multiple discrete events" $ do
        compareP (0,3)
          (discretise 4 saw)
          ("0 0.25 0.5 0.75" :: Pattern Double)
      it "can hold a value over multiple cycles" $ do
        comparePD (0,8)
          (discretise 0.5 saw)
          (slow 2 "0" :: Pattern Double)
      it "holding values over multiple cycles works in combination" $ do
        comparePD (0,8)
          ("0*4" |+ (_discretise (1/8) $ saw))
          ("0*4" :: Pattern Double)

    describe "sometimesBy" $ do
      it "does nothing when set at 0% probability" $ do
        let
          overTimeSpan = (0, 1)
          testMe = sometimesBy 0 (rev) (ps "bd*2 hh sn")
          expectedResult = ps "bd*2 hh sn"
          in
            compareP overTimeSpan testMe expectedResult

      it "applies the 'rev' function sometimes when set at 50% probability" $ pending
      -- do
      --   let
      --     overTimeSpan = (0, 3)
      --     testMe = sometimesBy 0.5 (rev) (ps "bd bd hh sn")
      --     expectedResult = cat ["bd bd hh sd",  "sn hh bd bd", "bd bd hh sn"]
      --     in
      --       compareP overTimeSpan testMe expectedResult

      it "applies the 'rev' function when set at 100% probability" $ do
        let
          overTimeSpan = (0, 1)
          testMe = sometimesBy 1 (rev) (ps "bd*2 hh cp")
          expectedResult = ps "cp hh bd*2"
          in
            compareP overTimeSpan testMe expectedResult

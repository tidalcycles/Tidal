{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.ControlTest where

import TestUtils
import Test.Microspec

import Prelude hiding ((<*), (*>))

import Sound.Tidal.Control
import Sound.Tidal.Core
import Sound.Tidal.Params
import Sound.Tidal.Pattern

run :: Microspec ()
run =
  describe "Sound.Tidal.Control" $ do

    describe "echo" $ do
      it "should echo the event by the specified time and multiply the gain factor" $ do
        comparePD (Arc 0 1)
          (echo 3 0.2 0.5 $ s "bd" # gain "1")
          (stack [
            rotR 0 $ s "bd" # gain 1,
            rotR 0.2 $ s "bd" # gain 0.5,
            rotR 0.4 $ s "bd" # gain 0.25
          ])

    describe "echoWith" $ do
      it "should echo the event by the specified time and apply the specified function" $ do
        comparePD (Arc 0 1)
          (echoWith 3 0.25 (|* speed 2) $ s "bd" # speed "1")
          (stack [
            rotR 0 $ s "bd" # speed 1,
            rotR 0.25 $ s "bd" # speed 2,
            rotR 0.5 $ s "bd" # speed 4
          ])

    describe "stutWith" $ do
      it "can mimic stut" $ do
        comparePD (Arc 0 1)
          (filterOnsets $ stutWith 4 0.25 (# gain 1) $ sound "bd")
          (filterOnsets $ stut 4 1 0.25 $ sound "bd")

    describe "splice" $ do
      it "can beatslice" $ do
        comparePD (Arc 0 1)
          (splice "4 8" "0 1" $ sound "bev")
          (begin "0 0.125" # end "0.25 0.25" # speed "0.5 0.25" # sound "bev" # unit "c")

    describe "slice" $ do
      it "can slice samples" $ do
        compareP (Arc 0 1)
          (slice "8 4" "7 5 0 3 2 4 1 6" $ sound "sn bd")
          (begin "0.875 0.625 0.0 0.375 0.5 0.0 0.25 0.5" # end "1.0 0.75 0.125 0.5 0.75 0.25 0.5 0.75" # sound "sn bd")

      it "can slice by 1" $ do
        compareP (Arc 0 1)
          (slice "1 4" "1 [2 4 1 6]" $ sound "sn bd")
          (begin "0.0 [0.5 0.0 0.25 0.5]" # end "1.0 [0.75 0.25  0.5 0.75]" # sound "sn bd")


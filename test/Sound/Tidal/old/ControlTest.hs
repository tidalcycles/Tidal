{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.SignalControlTest where

import TestUtils
import Test.Microspec

import Prelude hiding ((<*), (*>))

import Sound.Tidal.Types
import Sound.Tidal.Pattern
import Sound.Tidal.Signal.Base
import Sound.Tidal.Signal.Control

run :: Microspec ()
run =
  describe "Sound.Tidal.SignalControl" $ do

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

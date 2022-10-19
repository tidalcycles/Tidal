{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.SignalControlTest where

import TestUtils
import Test.Microspec

import Prelude hiding ((<*), (*>))

import Sound.Tidal.Signal.Base
import Sound.Tidal.Signal.Control
import Sound.Tidal.Signal.Compose
import Sound.Tidal.Types
import Sound.Tidal.Pattern
import Sound.Tidal.Params

run :: Microspec ()
run =
  describe "Sound.Tidal.Control" $ do

    describe "echo" $ do
      it "should echo the event by the specified time and multiply the gain factor" $ do
        comparePD (Arc 0 1)
          (echo 3 0.2 0.5 $ s "bd" # gain "1")
          (stack [
            late 0 $ s "bd" # gain 1,
            late 0.2 $ s "bd" # gain 0.5,
            late 0.4 $ s "bd" # gain 0.25
          ])
    describe "echoWith" $ do
      it "should echo the event by the specified time and apply the specified function" $ do
        comparePD (Arc 0 1)
          (echoWith 3 0.25 (|* speed 2) $ s "bd" # speed "1")
          (stack [
            late 0 $ s "bd" # speed 1,
            late 0.25 $ s "bd" # speed 2,
            late 0.5 $ s "bd" # speed 4
          ])

    describe "splice" $ do
      it "can beatslice" $ do
        comparePD (Arc 0 1)
          (splice "4 8" "0 1" $ sound "bev")
          (begin "0 0.125" # end "0.25 0.25" # speed "0.5 0.25" # sound "bev" # unit "c")

    describe "_chop" $ do
      it "can chop in two bits" $ do
        compareP (Arc 0 1)
          (_chop 2 $ s (pure "a"))
          (begin (fastcat [pure 0, pure 0.5]) # end (fastcat [pure 0.5, pure 1]) # (s (pure "a")))
      it "can be slowed" $ do
        compareP (Arc 0 1)
          (slow 2 $ _chop 2 $ s (pure "a"))
          (begin (pure 0) # end (pure 0.5) # (s (pure "a")))
      it "can chop a chop" $
        property $ compareTol (Arc 0 1) (_chop 6 $ s $ pure "a") (_chop 2 $ _chop 3 $ s $ pure "a")


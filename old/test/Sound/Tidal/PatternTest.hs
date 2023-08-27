{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.PatternTest where

import           Test.Microspec
import           TestUtils

import           Prelude                     hiding ((*>), (<*))

import           Sound.Tidal.Params
import           Sound.Tidal.Pattern
import           Sound.Tidal.Signal.Base     (segment)
import           Sound.Tidal.Signal.Waveform (sine)
import           Sound.Tidal.Types

run :: Microspec ()
run =
  describe "Sound.Tidal.Pattern" $ do
    describe "off" $ do
      it "superimposes and shifts pattern" $ do
        compareP (Arc 0 1)
          (off "-e" id $ s "0")
          (superimpose ("e" <~) $ s "0")

    describe "quantise" $ do
      it "can quantise notes" $ do
        compareP (Arc 0 1)          (segment 2 $ quantise 1 $ sine :: Signal Note)
          ("1 0" :: Signal Note)

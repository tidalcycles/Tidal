{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.PatternTest where

import           Test.Microspec

import           Prelude                     hiding ((*>), (<*))

import           Sound.Tidal.Mininotation
import           Sound.Tidal.Pattern         (off, quantise, segment,
                                              superimpose, (<~))
import           Sound.Tidal.Signal.Waveform (sine)
import           Sound.Tidal.TestUtils
import           Sound.Tidal.Types

run :: Microspec ()
run =
  describe "Sound.Tidal.Pattern" $ do
    describe "off" $ do
      it "superimposes and shifts pattern" $ do
        compareP (Span 0 1)
          (("0" :: Signal Int))
          (("0" :: Signal Int))

    describe "quantise" $ do
      it "can quantise notes" $ do
        compareP (Span 0 1)
          (segment 2 $ quantise 1 $ sine :: Signal Note)
          ("1 0" :: Signal Note)

{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.TimeTest where

import           Prelude           hiding ((*>), (<*))
import           Sound.Tidal.Time
import           Sound.Tidal.Types
import           Test.Microspec

run :: Microspec ()
run =
  describe "Sound.Tidal.Time" $ do
    describe "sam" $ do
      it "start of a cycle, round down time value" $ do
        let res = sam (3.4 :: Time)
        property $ (3.0 :: Time) === res

    describe "nextSam" $ do
      it "the end point of the current cycle, and start of the next" $ do
        let res = nextSam (3.4 :: Time)
        property $ (4.0 :: Time) === res

    describe "cyclePos" $ do
      it "Subtract a Time value from its value rounded down (the start of the cycle)" $ do
        let res = cyclePos 2.6
        property $ (0.6 :: Time) === res
      it "If no difference between a given Time and the start of the cycle" $ do
        let res = cyclePos 2
        property $ (0.0 :: Time) === res


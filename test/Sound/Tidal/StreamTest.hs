{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.StreamTest where

import Test.Microspec
import Sound.Tidal.Stream
import Sound.Tidal.Pattern
import qualified Sound.OSC.FD as O

run :: Microspec ()
run =
  describe "Sound.Tidal.Stream" $ do
    describe "toDatum" $ do
      it "should convert VN to osc float" $ do
        toDatum (VN (Note 3.5)) `shouldBe` O.float (3.5 :: Double)
          

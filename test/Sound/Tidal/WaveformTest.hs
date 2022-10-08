{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.WaveformTest where

import           Test.Microspec
import           TestUtils

import           Prelude             hiding ((*>), (<*))

import           Data.Ratio

import           Sound.Tidal.Types
import           Sound.Tidal.Pattern
import           Sound.Tidal.Signal.Base
import           Sound.Tidal.Signal.Compose (struct)
import           Sound.Tidal.Signal.Waveform

import qualified Data.Map.Strict     as Map

run :: Microspec ()
run =
  describe "Sound.Tidal.Signal.Waveform" $ do
    describe "Elemental signals" $ do
      let sampleOf :: Signal Double -> Rational -> Double
          sampleOf pat t = (value . head) $ query pat (State (Arc t t) Map.empty)
      describe "are in range [0, 1]" $ do
        let inNormalRange pat t = (y >= 0) && (y <= 1)
              where y = sampleOf pat t
        it "sine" $ inNormalRange sine
        it "cosine" $ inNormalRange cosine
        it "saw" $ inNormalRange saw
        it "isaw" $ inNormalRange isaw
        it "tri" $ inNormalRange tri
        it "square" $ inNormalRange square
      describe "have correctly-scaled bipolar variants" $ do
        let areCorrectlyScaled pat pat2 t = (y * 2 - 1) ~== y2
              where y = sampleOf pat t
                    y2 = sampleOf pat2 t
        it "sine" $ areCorrectlyScaled sine sine2
        it "cosine" $ areCorrectlyScaled cosine cosine2
        it "saw" $ areCorrectlyScaled saw saw2
        it "isaw" $ areCorrectlyScaled isaw isaw2
        it "tri" $ areCorrectlyScaled tri tri2
        it "square" $ areCorrectlyScaled square square2

      describe "saw" $ do
        it "goes from 0 up to 1 every cycle" $ do
          it "0" $
            queryArc saw (Arc 0 0) `shouldBe` [Event (Metadata []) Nothing (Arc 0 0) 0 :: Event Double]
          it "0.25" $
            queryArc saw (Arc 0.25 0.25) `shouldBe` [Event (Metadata []) Nothing (Arc 0.25 0.25) 0.25 :: Event Double]
          it "0.5" $
            queryArc saw (Arc 0.5 0.5) `shouldBe` [Event (Metadata []) Nothing (Arc 0.5 0.5) 0.5 :: Event Double]
          it "0.75" $
            queryArc saw (Arc 0.75 0.75) `shouldBe` [Event (Metadata []) Nothing (Arc 0.75 0.75) 0.75 :: Event Double]
        it "can be added to" $
          map value (queryArc ((+ 1) <$> saw) (Arc 0.5 0.5)) `shouldBe` [1.5 :: Float]
        it "works on the left of <*>" $
          queryArc ((+) <$> saw <*> pure 3) (Arc 0 1)
            `shouldBe` [Event (Metadata []) Nothing (Arc 0 1) 3.5 :: Event Double]
        it "works on the right of <*>" $
          queryArc (fast 4 (pure (+ 3)) <*> saw) (Arc 0 1)
            `shouldBe` [ Event (Metadata []) Nothing (Arc 0 0.25) 3.5 :: Event Double,
                         Event (Metadata []) Nothing (Arc 0.25 0.5) 3.5,
                         Event (Metadata []) Nothing (Arc 0.5 0.75) 3.5,
                         Event (Metadata []) Nothing (Arc 0.75 1) 3.5
                       ]
        it "can be reversed" $ do
          it "works with whole cycles" $
            queryArc (rev saw) (Arc 0 1)
              `shouldBe` [Event (Metadata []) Nothing (Arc 0 1) 0.5 :: Event Double]
          it "works with half cycles" $
            queryArc (rev saw) (Arc 0 0.5)
              `shouldBe` [Event (Metadata []) Nothing (Arc 0 0.5) 0.75 :: Event Double]
          it "works with inset points" $
            queryArc (rev saw) (Arc 0.25 0.25)
              `shouldBe` [Event (Metadata []) Nothing (Arc 0.25 0.25) 0.75 :: Event Double]

      describe "tri" $ do
        it "goes from 0 up to 1 and back every cycle" $
          comparePD
            (Arc 0 1)
            (struct "t*8" (tri :: Signal Double))
            "0.125 0.375 0.625 0.875 0.875 0.625 0.375 0.125"
        it "can be added to" $
          comparePD
            (Arc 0 1)
            (struct "t*8" $ (tri :: Signal Double) + 1)
            "1.125 1.375 1.625 1.875 1.875 1.625 1.375 1.125"
  

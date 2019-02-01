{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.ChordsTest where

import TestUtils
import Test.Microspec

import Prelude hiding ((<*), (*>))

import Sound.Tidal.Chords
import Sound.Tidal.Pattern


run :: Microspec ()
run =
  describe "Sound.Tidal.Chords" $ do
    describe "chordList" $ do
        let expectedChordList = 
                "major maj minor min aug dim " ++
                "major7 maj7 dom7 minor7 min7 dim7 "++ 
                "one 1 five 5 plus "++
                "sharp5 msharp5 sus2 sus4 "++
                "six 6 m6 sevenSus2 7sus2 sevenSus4 7sus4 "++
                "sevenFlat5 7f5 m7flat5 m7f5 sevenSharp5 7s5 m7sharp5 m7s5 "++
                "nine m9 m7sharp9 m7s9 maj9 nineSus4 ninesus4 9sus4 "++
                "sixby9 6by9 m6by9 sevenFlat9 7f9 m7flat9 m7f9 "++
                "sevenFlat10 7f10 nineSharp5 9s5 m9sharp5 m9s5 "++
                "sevenSharp5flat9 7s5f9 m7sharp5flat9 "++
                "eleven 11 m11 maj11 elevenSharp 11s m11sharp m11s thirteen 13 m13"
        it "should list all scales" $ do
            (chordList) `shouldBe` expectedChordList

{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.ChordsTest where

import           Test.Microspec
import           TestUtils

import           Prelude           hiding ((*>), (<*))

import           Sound.Tidal.Types

run :: Microspec ()
run =
  describe "Sound.Tidal.Chords" $ do
    describe "chord" $ do
        describe "chord length adjustments" $ do
            it "can remove notes from the end of the list when length given is less than the standard chord length" $ do
                compareP (Span 0 1)
                    ("'major'1")
                    ("[0]" :: Signal Note)
            it "can do nothing when the length given is the same as the standard chord length" $ do
                compareP (Span 0 1)
                    ("'major'3")
                    ("[0, 4, 7]" :: Signal Note)
            it "can append chord notes at higher octaves to the list when length given is greater than the standard chord length" $ do
                compareP (Span 0 1)
                    ("'major'5")
                    ("[0, 4, 7, 12, 16]" :: Signal Note)
        describe "open voiced chords" $ do
            it "can subtract 12 from the first and third element of a list, and sort them in ascending numerical order" $ do
                compareP (Span 0 1)
                    ("'major'o")
                    ("[-12, -5, 4]" :: Signal Note)
            it "not crash if chord length is < 3" $ do
                compareP (Span 0 1)
                    ("'five'o")
                    ("[0, 7]" :: Signal Note)
        describe "chord inversions" $ do
            it "can add 12 to the first element of a list, and sort in ascending numeric order (1st inversion)" $ do
                compareP (Span 0 1)
                    ("'major'i")
                    ("[4, 7, 12]" :: Signal Note)
            it "can add 12 to the first two elements of a list, and sort in ascending numeric order (2nd inversion)" $ do
                compareP (Span 0 1)
                    ("'major'ii")
                    ("[7, 12, 16]" :: Signal Note)
            it "can add 12 to the first three elements of a list, and sort in ascending numeric order (3rd inversion)" $ do
                compareP (Span 0 1)
                    ("'major'iii")
                    ("[12, 16, 19]" :: Signal Note)
        describe "edge cases" $ do
            it "gracefully handle an inversion when there are more inversions than notes in the chord (4th inversion of a 3 note chord)" $ do
                compareP (Span 0 1)
                    ("'major'iiii")
                    ("[16, 19, 24]" :: Signal Note)

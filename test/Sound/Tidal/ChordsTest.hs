{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.ChordsTest where

import TestUtils
import Test.Microspec

import Prelude hiding ((<*), (*>))

import Sound.Tidal.Pattern

run :: Microspec ()
run =
  describe "Sound.Tidal.Chords" $ do
    describe "chord" $ do
        describe "open voiced chords" $ do
            it "can subtract 12 from the first and third element of a list, and sort them in ascending numerical order" $ do
                compareP (Arc 0 1)
                    ("'major'o")
                    ("[-12, -5, 4]" :: Pattern Note)
        describe "chord inversions" $ do
            it "can add 12 to the first element of a list, and sort in ascending numeric order (1st inversion)" $ do
                compareP (Arc 0 1)
                    ("'major'i")
                    ("[4, 7, 12]" :: Pattern Note)
            it "can add 12 to the first two elements of a list, and sort in ascending numeric order (2nd inversion)" $ do
                compareP (Arc 0 1)
                    ("'major'ii")
                    ("[7, 12, 16]" :: Pattern Note)
            it "can add 12 to the first three elements of a list, and sort in ascending numeric order (3rd inversion)" $ do
                compareP (Arc 0 1)
                    ("'major'iii")
                    ("[12, 16, 19]" :: Pattern Note)
        describe "edge cases" $ do
            it "gracefully handle an inversion when there are more inversions than notes in the chord (4th inversion of a 3 note chord)" $ do
                compareP (Arc 0 1)
                    ("'major'iiii")
                    ("[16, 19, 24]" :: Pattern Note)
        -- describe "edge cases" $ do
            -- -- fails currently, to fix
            -- it "can do nothing to a 2-note list because there are not enough elements" $ do
            --     compareP (Arc 0 1)
            --         (Sound.Tidal.ParseBP.parseChord "five'o")
            --         ("[0, 7]"::Pattern Rational)

-- run =
--   describe "Sound.Tidal.Scales" $ do
--     describe "scale" $ do
--         describe "5 note scales" $ do
--             let twoOctavesOf5NoteScale = "0 1 2 3 4 5 6 7 8 9"
--             it "can transform notes correctly over 2 octaves - minPent" $ do
--                 compareP (Arc 0 1)
--                     (Sound.Tidal.Scales.scale "minPent" twoOctavesOf5NoteScale)
--                     ("0 3 5 7 10 12 15 17 19 22"::Pattern Rational)
--         describe "6 note scales" $ do
--             let twoOctavesOf6NoteScale = "0 1 2 3 4 5 6 7 8 9 10 11"
--             it "can transform notes correctly over 2 octaves - whole" $ do
--                 compareP (Arc 0 1)
--                     (Sound.Tidal.Scales.scale "whole" twoOctavesOf6NoteScale)
--                     ("0 2 4 6 8 10 12 14 16 18 20 22"::Pattern Rational)
--         describe "modes of limited transposition" $ do
--             let twoOctavesOf6NoteScale = "0 1 2 3 4 5 6 7 8 9 10 11"
--             let twoOctavesOf8NoteScale = "0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15"
--             let twoOctavesOf9NoteScale = "0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17"
--             let twoOctavesOf10NoteScale = "0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19"
--             it "can transform notes correctly over 2 octaves - messiaen1" $ do
--                 compareP (Arc 0 1)
--                     (Sound.Tidal.Scales.scale "messiaen1" twoOctavesOf6NoteScale)
--                     (Sound.Tidal.Scales.scale "wholetone" twoOctavesOf6NoteScale::Pattern Rational)
--             it "can transform notes correctly over 2 octaves - messiaen2" $ do
--                 compareP (Arc 0 1)
--                     (Sound.Tidal.Scales.scale "messiaen2" twoOctavesOf8NoteScale)
--                     (Sound.Tidal.Scales.scale "diminished" twoOctavesOf8NoteScale::Pattern Rational)
--             it "can transform notes correctly over 2 octaves - messiaen3" $ do
--                 -- tone, semitone, semitone, tone, semitone, semitone, tone, semitone, semitone
--                 compareP (Arc 0 1)
--                     (Sound.Tidal.Scales.scale "messiaen3" twoOctavesOf9NoteScale)
--                     ("0 2 3 4 6 7 8 10 11 12 14 15 16 18 19 20 22 23"::Pattern Rational)
--         describe "12 note scales" $ do
--             let twoOctavesOf12NoteScale = "0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23"
--             it "can transform notes correctly over 2 octaves - chromatic" $ do
--                 compareP (Arc 0 1)
--                     (Sound.Tidal.Scales.scale "chromatic" twoOctavesOf12NoteScale)
--                     ("0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23"::Pattern Rational)
--         describe "edge cases" $ do
--             it "responds to unknown scales by mapping to octaves" $ do
--                 compareP (Arc 0 1)
--                     (Sound.Tidal.Scales.scale "ergaerv" "0 1 2 3 4")
--                     ("0 12 24 36 48"::Pattern Rational)
--             it "correctly maps negative numbers" $ do
--                 compareP (Arc 0 1)
--                     (Sound.Tidal.Scales.scale "major" "0 -1 -2 -3 -4 -5 -6 -7 -8 -9 -10 -11 -12 -13")
--                     ("0 -1 -3 -5 -7 -8 -10 -12 -13 -15 -17 -19 -20 -22 "::Pattern Rational)

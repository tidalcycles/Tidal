{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.ScalesTest where

import TestUtils
import Test.Microspec

import Prelude hiding ((<*), (*>))

import Sound.Tidal.Scales
import Sound.Tidal.Pattern

run :: Microspec ()
run =
  describe "Sound.Tidal.Scales" $ do
    describe "scale" $ do
        describe "5 note scales" $ do
            let twoOctavesOf5NoteScale = "0 1 2 3 4 5 6 7 8 9"
            it "can transform notes correctly over 2 octaves - minPent" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "minPent" twoOctavesOf5NoteScale)
                    ("0 3 5 7 10 12 15 17 19 22"::Pattern Rational)
            it "can transform notes correctly over 2 octaves - majPent" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "majPent" twoOctavesOf5NoteScale)
                    ("0 2 4 7 9 12 14 16 19 21"::Pattern Rational)
            it "can transform notes correctly over 2 octaves - ritusen" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "ritusen" twoOctavesOf5NoteScale)
                    ("0 2 5 7 9 12 14 17 19 21"::Pattern Rational)
            it "can transform notes correctly over 2 octaves - egyptian" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "egyptian" twoOctavesOf5NoteScale)
                    ("0 2 5 7 10 12 14 17 19 22"::Pattern Rational)
            it "can transform notes correctly over 2 octaves - kumai" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "kumai" twoOctavesOf5NoteScale)
                    ("0 2 3 7 9 12 14 15 19 21"::Pattern Rational)
            it "can transform notes correctly over 2 octaves - hirajoshi" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "hirajoshi" twoOctavesOf5NoteScale)
                    ("0 2 3 7 8 12 14 15 19 20"::Pattern Rational)
            it "can transform notes correctly over 2 octaves - iwato" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "iwato" twoOctavesOf5NoteScale)
                    ("0 1 5 6 10 12 13 17 18 22"::Pattern Rational)
            it "can transform notes correctly over 2 octaves - chinese" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "chinese" twoOctavesOf5NoteScale)
                    ("0 4 6 7 11 12 16 18 19 23"::Pattern Rational)
            it "can transform notes correctly over 2 octaves - indian" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "indian" twoOctavesOf5NoteScale)
                    ("0 4 5 7 10 12 16 17 19 22"::Pattern Rational)
            it "can transform notes correctly over 2 octaves - pelog" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "pelog" twoOctavesOf5NoteScale)
                    ("0 1 3 7 8 12 13 15 19 20"::Pattern Rational)
            it "can transform notes correctly over 2 octaves - prometheus" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "prometheus" twoOctavesOf5NoteScale)
                    ("0 2 4 6 11 12 14 16 18 23"::Pattern Rational)
            it "can transform notes correctly over 2 octaves - scriabin" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "scriabin" twoOctavesOf5NoteScale)
                    ("0 1 4 7 9 12 13 16 19 21"::Pattern Rational)
            it "can transform notes correctly over 2 octaves - gong" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "gong" twoOctavesOf5NoteScale)
                    ("0 2 4 7 9 12 14 16 19 21"::Pattern Rational)
            it "can transform notes correctly over 2 octaves - shang" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "shang" twoOctavesOf5NoteScale)
                    ("0 2 5 7 10 12 14 17 19 22"::Pattern Rational)
            it "can transform notes correctly over 2 octaves - jiao" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "jiao" twoOctavesOf5NoteScale)
                    ("0 3 5 8 10 12 15 17 20 22"::Pattern Rational)
            it "can transform notes correctly over 2 octaves - zhi" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "zhi" twoOctavesOf5NoteScale)
                    ("0 2 5 7 9 12 14 17 19 21"::Pattern Rational)
            it "can transform notes correctly over 2 octaves - yu" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "yu" twoOctavesOf5NoteScale)
                    ("0 3 5 7 10 12 15 17 19 22"::Pattern Rational)
        describe "6 note scales" $ do
            let twoOctavesOf6NoteScale = "0 1 2 3 4 5 6 7 8 9 10 11"
            it "can transform notes correctly over 2 octaves - whole" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "whole" twoOctavesOf6NoteScale)
                    ("0 2 4 6 8 10 12 14 16 18 20 22"::Pattern Rational)
            it "can transform notes correctly over 2 octaves - wholetone" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "wholetone" twoOctavesOf6NoteScale)
                    (Sound.Tidal.Scales.scale "whole" twoOctavesOf6NoteScale :: Pattern Rational)
            it "can transform notes correctly over 2 octaves - augmented" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "augmented" twoOctavesOf6NoteScale)
                    ("0 3 4 7 8 11 12 15 16 19 20 23"::Pattern Rational)
            it "can transform notes correctly over 2 octaves - augmented2" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "augmented2" twoOctavesOf6NoteScale)
                    ("0 1 4 5 8 9 12 13 16 17 20 21"::Pattern Rational)
            it "can transform notes correctly over 2 octaves - hexMajor7" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "hexMajor7" twoOctavesOf6NoteScale)
                    ("0 2 4 7 9 11 12 14 16 19 21 23"::Pattern Rational)
            it "can transform notes correctly over 2 octaves - hexPhrygian" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "hexPhrygian" twoOctavesOf6NoteScale)
                    ("0 1 3 5 8 10 12 13 15 17 20 22"::Pattern Rational)
            it "can transform notes correctly over 2 octaves - hexDorian" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "hexDorian" twoOctavesOf6NoteScale)
                    ("0 2 3 5 7 10 12 14 15 17 19 22"::Pattern Rational)
            it "can transform notes correctly over 2 octaves - hexSus" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "hexSus" twoOctavesOf6NoteScale)
                    ("0 2 5 7 9 10 12 14 17 19 21 22"::Pattern Rational)
            it "can transform notes correctly over 2 octaves - hexMajor6" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "hexMajor6" twoOctavesOf6NoteScale)
                    ("0 2 4 5 7 9 12 14 16 17 19 21"::Pattern Rational)
            it "can transform notes correctly over 2 octaves - hexAeolian" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "hexAeolian" twoOctavesOf6NoteScale)
                    ("0 3 5 7 8 10 12 15 17 19 20 22"::Pattern Rational)
        describe "7 note scales" $ do
            let twoOctavesOf7NoteScale = "0 1 2 3 4 5 6 7 8 9 10 11 12 13"
            it "can transform notes correctly over 2 octaves - major" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "major" twoOctavesOf7NoteScale)
                    ("0 2 4 5 7 9 11 12 14 16 17 19 21 23"::Pattern Rational)
            it "can transform notes correctly over 2 octaves - ionian" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "ionian" twoOctavesOf7NoteScale)
                    (Sound.Tidal.Scales.scale "major" twoOctavesOf7NoteScale :: Pattern Rational)
            it "can transform notes correctly over 2 octaves - dorian" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "dorian" twoOctavesOf7NoteScale)
                    ("0 2 3 5 7 9 10 12 14 15 17 19 21 22"::Pattern Rational)
            it "can transform notes correctly over 2 octaves - aeolian" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "aeolian" twoOctavesOf7NoteScale)
                    ("0 2 3 5 7 8 10 12 14 15 17 19 20 22"::Pattern Rational)
            it "can transform notes correctly over 2 octaves - aeolian" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "minor" twoOctavesOf7NoteScale)
                    (Sound.Tidal.Scales.scale "aeolian" twoOctavesOf7NoteScale::Pattern Rational)
            it "can transform notes correctly over 2 octaves - minor" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "minor" twoOctavesOf7NoteScale)
                    (Sound.Tidal.Scales.scale "aeolian" twoOctavesOf7NoteScale::Pattern Rational)
            it "can transform notes correctly over 2 octaves - locrian" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "locrian" twoOctavesOf7NoteScale)
                    ("0 1 3 5 6 8 10 12 13 15 17 18 20 22"::Pattern Rational)
            it "can transform notes correctly over 2 octaves - harmonicMinor" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "harmonicMinor" twoOctavesOf7NoteScale)
                    ("0 2 3 5 7 8 11 12 14 15 17 19 20 23"::Pattern Rational)
            it "can transform notes correctly over 2 octaves - harmonicMajor" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "harmonicMajor" twoOctavesOf7NoteScale)
                    ("0 2 4 5 7 8 11 12 14 16 17 19 20 23"::Pattern Rational)
            it "can transform notes correctly over 2 octaves - melodicMinor" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "melodicMinor" twoOctavesOf7NoteScale)
                    ("0 2 3 5 7 9 11 12 14 15 17 19 21 23"::Pattern Rational)
            it "can transform notes correctly over 2 octaves - melodicMinorDesc" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "melodicMinorDesc" twoOctavesOf7NoteScale)
                    (Sound.Tidal.Scales.scale "minor" twoOctavesOf7NoteScale::Pattern Rational)
            it "can transform notes correctly over 2 octaves - melodicMajor" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "melodicMajor" twoOctavesOf7NoteScale)
                    ("0 2 4 5 7 8 10 12 14 16 17 19 20 22"::Pattern Rational)
            it "can transform notes correctly over 2 octaves - bartok" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "bartok" twoOctavesOf7NoteScale)
                    (Sound.Tidal.Scales.scale "melodicMajor" twoOctavesOf7NoteScale::Pattern Rational)
            it "can transform notes correctly over 2 octaves - hindu" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "hindu" twoOctavesOf7NoteScale)
                    (Sound.Tidal.Scales.scale "melodicMajor" twoOctavesOf7NoteScale::Pattern Rational)
            it "can transform notes correctly over 2 octaves - todi" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "todi" twoOctavesOf7NoteScale)
                    ("0 1 3 6 7 8 11 12 13 15 18 19 20 23"::Pattern Rational)
            it "can transform notes correctly over 2 octaves - purvi" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "purvi" twoOctavesOf7NoteScale)
                    ("0 1 4 6 7 8 11 12 13 16 18 19 20 23"::Pattern Rational)
            it "can transform notes correctly over 2 octaves - marva" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "marva" twoOctavesOf7NoteScale)
                    ("0 1 4 6 7 9 11 12 13 16 18 19 21 23"::Pattern Rational)
            it "can transform notes correctly over 2 octaves - bhairav" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "bhairav" twoOctavesOf7NoteScale)
                    ("0 1 4 5 7 8 11 12 13 16 17 19 20 23"::Pattern Rational)
            it "can transform notes correctly over 2 octaves - ahirbhairav" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "ahirbhairav" twoOctavesOf7NoteScale)
                    ("0 1 4 5 7 9 10 12 13 16 17 19 21 22"::Pattern Rational)
            it "can transform notes correctly over 2 octaves - superLocrian" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "superLocrian" twoOctavesOf7NoteScale)
                    ("0 1 3 4 6 8 10 12 13 15 16 18 20 22"::Pattern Rational)
            it "can transform notes correctly over 2 octaves - romanianMinor" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "romanianMinor" twoOctavesOf7NoteScale)
                    ("0 2 3 6 7 9 10 12 14 15 18 19 21 22"::Pattern Rational)
            it "can transform notes correctly over 2 octaves - hungarianMinor" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "hungarianMinor" twoOctavesOf7NoteScale)
                    ("0 2 3 6 7 8 11 12 14 15 18 19 20 23"::Pattern Rational)
            it "can transform notes correctly over 2 octaves - neapolitanMinor" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "neapolitanMinor" twoOctavesOf7NoteScale)
                    ("0 1 3 5 7 8 11 12 13 15 17 19 20 23"::Pattern Rational)
            it "can transform notes correctly over 2 octaves - enigmatic" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "enigmatic" twoOctavesOf7NoteScale)
                    ("0 1 4 6 8 10 11 12 13 16 18 20 22 23"::Pattern Rational)
            it "can transform notes correctly over 2 octaves - spanish" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "spanish" twoOctavesOf7NoteScale)
                    ("0 1 4 5 7 8 10 12 13 16 17 19 20 22"::Pattern Rational)
            it "can transform notes correctly over 2 octaves - leadingWhole" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "leadingWhole" twoOctavesOf7NoteScale)
                    ("0 2 4 6 8 10 11 12 14 16 18 20 22 23"::Pattern Rational)
            it "can transform notes correctly over 2 octaves - lydianMinor" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "lydianMinor" twoOctavesOf7NoteScale)
                    ("0 2 4 6 7 8 10 12 14 16 18 19 20 22"::Pattern Rational)
            it "can transform notes correctly over 2 octaves - neapolitanMajor" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "neapolitanMajor" twoOctavesOf7NoteScale)
                    ("0 1 3 5 7 9 11 12 13 15 17 19 21 23"::Pattern Rational)
            it "can transform notes correctly over 2 octaves - locrianMajor" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "locrianMajor" twoOctavesOf7NoteScale)
                    ("0 2 4 5 6 8 10 12 14 16 17 18 20 22"::Pattern Rational)
        describe "8 note scales" $ do
            let twoOctavesOf8NoteScale = "0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15"
            it "can transform notes correctly over 2 octaves - diminished" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "diminished" twoOctavesOf8NoteScale)
                    ("0 1 3 4 6 7 9 10 12 13 15 16 18 19 21 22"::Pattern Rational)
            it "can transform notes correctly over 2 octaves - octatonic" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "octatonic" twoOctavesOf8NoteScale)
                    (Sound.Tidal.Scales.scale "diminished" twoOctavesOf8NoteScale::Pattern Rational)
            it "can transform notes correctly over 2 octaves - diminished2" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "diminished2" twoOctavesOf8NoteScale)
                    ("0 2 3 5 6 8 9 11 12 14 15 17 18 20 21 23"::Pattern Rational)
            it "can transform notes correctly over 2 octaves - octatonic2" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "octatonic2" twoOctavesOf8NoteScale)
                    (Sound.Tidal.Scales.scale "diminished2" twoOctavesOf8NoteScale::Pattern Rational)
        describe "modes of limited transposition" $ do
            let twoOctavesOf6NoteScale = "0 1 2 3 4 5 6 7 8 9 10 11"
            let twoOctavesOf8NoteScale = "0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15"
            let twoOctavesOf9NoteScale = "0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17"
            let twoOctavesOf10NoteScale = "0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19"
            it "can transform notes correctly over 2 octaves - messiaen1" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "messiaen1" twoOctavesOf6NoteScale)
                    (Sound.Tidal.Scales.scale "wholetone" twoOctavesOf6NoteScale::Pattern Rational)
            it "can transform notes correctly over 2 octaves - messiaen2" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "messiaen2" twoOctavesOf8NoteScale)
                    (Sound.Tidal.Scales.scale "diminished" twoOctavesOf8NoteScale::Pattern Rational)
            it "can transform notes correctly over 2 octaves - messiaen3" $ do
                -- tone, semitone, semitone, tone, semitone, semitone, tone, semitone, semitone
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "messiaen3" twoOctavesOf9NoteScale)
                    ("0 2 3 4 6 7 8 10 11 12 14 15 16 18 19 20 22 23"::Pattern Rational)
            it "can transform notes correctly over 2 octaves - messiaen4" $ do
                -- semitone, semitone, minor third, semitone, semitone, semitone, minor third, semitone
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "messiaen4" twoOctavesOf8NoteScale)
                    ("0 1 2 5 6 7 8 11 12 13 14 17 18 19 20 23"::Pattern Rational)
            it "can transform notes correctly over 2 octaves - messiaen5" $ do
                -- semitone, major third, semitone, semitone, major third, semitone
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "messiaen5" twoOctavesOf6NoteScale)
                    ("0 1 5 6 7 11 12 13 17 18 19 23"::Pattern Rational)
            it "can transform notes correctly over 2 octaves - messiaen6" $ do
                -- tone, tone, semitone, semitone, tone, tone, semitone, semitone
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "messiaen6" twoOctavesOf8NoteScale)
                    ("0 2 4 5 6 8 10 11 12 14 16 17 18 20 22 23"::Pattern Rational)
            it "can transform notes correctly over 2 octaves - messiaen7" $ do
                -- semitone, semitone, semitone, tone, semitone, semitone, semitone, semitone, tone, semitone
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "messiaen7" twoOctavesOf10NoteScale)
                    ("0 1 2 3 5 6 7 8 9 11 12 13 14 15 17 18 19 20 21 23"::Pattern Rational)
        describe "12 note scales" $ do
            let twoOctavesOf12NoteScale = "0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23"
            it "can transform notes correctly over 2 octaves - chromatic" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "chromatic" twoOctavesOf12NoteScale)
                    ("0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23"::Pattern Rational)
        describe "edge cases" $ do
            it "responds to unknown scales by mapping to octaves" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "ergaerv" "0 1 2 3 4")
                    ("0 12 24 36 48"::Pattern Rational)
            it "correctly maps negative numbers" $ do
                compareP (Arc 0 1)
                    (Sound.Tidal.Scales.scale "major" "0 -1 -2 -3 -4 -5 -6 -7 -8 -9 -10 -11 -12 -13")
                    ("0 -1 -3 -5 -7 -8 -10 -12 -13 -15 -17 -19 -20 -22 "::Pattern Rational)
           

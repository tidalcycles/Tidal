{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.ParseTest where

import Test.Microspec
import TestUtils
import Control.Exception

import Prelude hiding ((<*), (*>))

import Sound.Tidal.ExceptionsTest (shouldThrow, anyException)
import Sound.Tidal.Core
import Sound.Tidal.Pattern
import Sound.Tidal.UI (_degradeBy)

run :: Microspec ()
run =
  describe "Sound.Tidal.Parse" $ do
    describe "parseBP_E" $ do
      it "can parse strings" $ do
        compareP (Arc 0 12)
          ("a b c" :: Pattern String)
          (fastCat ["a", "b", "c"])
      it "can parse ints" $ do
        compareP (Arc 0 2)
          ("0 1 2 3 4 5 6 7 8 0 10 20 30 40 50" :: Pattern Int)
          (fastCat $ map (pure . read) $ words "0 1 2 3 4 5 6 7 8 0 10 20 30 40 50")
      it "can parse pattern groups" $ do
        compareP (Arc 0 1)
          ("[bd sd] hh" :: Pattern String)
          (fastCat ["bd sd", "hh"])
      it "can parse pattern groups shorthand " $ do
        compareP (Arc 0 1)
          ("bd sd . hh hh hh" :: Pattern String)
          ("[bd sd] [hh hh hh]")
      it "can alternate with <>" $ do
        compareP (Arc 0 2)
          ("a <b c>" :: Pattern String)
          (cat [fastCat ["a", "b"], fastCat ["a", "c"]])
      it "can slow with /" $ do
        compareP (Arc 0 2)
          ("a/2" :: Pattern String)
          (slow 2 $ "a")
      it "can speed up with *" $ do
        compareP (Arc 0 2)
          ("a*8" :: Pattern String)
          (fast 8 "a")
      it "can elongate with _" $ do
        compareP (Arc 0 2)
          ("a _ _ b _" :: Pattern String)
          (timeCat [(3,"a"), (2,"b")])
      it "can replicate with !" $ do
        compareP (Arc 0 2)
          ("a! b" :: Pattern String)
          (fastCat ["a", "a", "b"])
      it "can replicate with ! inside {}" $ do
        compareP (Arc 0 2)
          ("{a a}%2" :: Pattern String)
          ("{a !}%2" :: Pattern String)
      it "can replicate with ! and number" $ do
        compareP (Arc 0 2)
          ("a!3 b" :: Pattern String)
          (fastCat ["a", "a", "a", "b"])
      it "can degrade with ?" $ do
        compareP (Arc 0 1)
          ("a?" :: Pattern String)
          (degradeByDefault "a")
      it "can degrade with ? and number" $ do
        compareP (Arc 0 1)
          ("a?0.2" :: Pattern String)
          (_degradeBy 0.2 "a")
      it "can degrade with ? for double patterns" $ do
        compareP (Arc 0 1)
          ("0.4 0.5? 0.6" :: Pattern Double)
          (fastcat[0.4, degradeByDefault 0.5, 0.6])
      it "can handle ? on replicated value" $ do
        compareP (Arc 0 1)
          ("a!8?" :: Pattern String)
          ("[a!8]?" :: Pattern String)
      it "can handle ? on streched value" $ do
        compareP (Arc 0 1)
          ("a*4@0.25?" :: Pattern String)
          ("[a*4@0.25]?" :: Pattern String)
      it "can stretch with @" $ do
        comparePD (Arc 0 1)
          ("a@2 b" :: Pattern String)
          (timeCat [(2, "a"),(1,"b")])
      it "can do polymeter with {}" $ do
        compareP (Arc 0 2)
          ("{a b, c d e}" :: Pattern String)
          (stack [fastcat [pure "a", pure "b"], slow 1.5 $ fastcat [pure "c", pure "d", pure "e"]])
      it "can parse .. with ints" $ do
        compareP (Arc 0 2)
          ("0 .. 8" :: Pattern Int)
          ("0 1 2 3 4 5 6 7 8")
      it "can parse .. with rationals" $ do
        compareP (Arc 0 2)
          ("0 .. 8" :: Pattern Rational)
          ("0 1 2 3 4 5 6 7 8")
      it "can parse .. with doubles" $ do
        compareP (Arc 0 2)
          ("0.0 .. 8.0" :: Pattern Double)
          ("0 1 2 3 4 5 6 7 8")
      it "can parse .. with doubles, without spaces" $ do
        compareP (Arc 0 2)
          ("0.0..8.0" :: Pattern Double)
          ("0 1 2 3 4 5 6 7 8")
      it "can parse .. with notes" $ do
        compareP (Arc 0 2)
          ("0.0 .. 8.0" :: Pattern Note)
          ("0 1 2 3 4 5 6 7 8")
      it "can parse .. with notes, without spaces" $ do
        compareP (Arc 0 2)
          ("0..8" :: Pattern Note)
          ("0 1 2 3 4 5 6 7 8")
      it "can handle repeats (!) and durations (@) with <>" $ do
        compareP (Arc 0 31)
          ("<a!3 b ! c@5>" :: Pattern String)
          (slow 10 "[a a a b b] c")
      it "can handle repeats (!) and durations (@) with <> (with ints)" $ do
        compareP (Arc 0 31)
          ("<1!3 2 ! 3@5>" :: Pattern Int)
          (slow 10 "[1 1 1 2 2] 3")
      it "can handle fractional durations" $ do
        compareP (Arc 0 2)
          ("a@0.5 b@1%6 b@1%6 b@1%6" :: Pattern String)
          ("a b*3")
      it "can handle fractional durations (with rationals)" $ do
        compareP (Arc 0 2)
          ("1%3@0.5 3%4@1%6 3%4@1%6 3%4@1%6" :: Pattern Rational)
          ("1%3 0.75*3")
      it "can handle ratio shortands on a fraction" $ do
        compareP (Arc 0 1)
          ("1%3t" :: Pattern Rational)
          ("1%9" :: Pattern Rational)
      it "can handle ratio shortands on a floating point number" $ do
        compareP (Arc 0 1)
          ("3.33t" :: Pattern Double)
          ("1.11" :: Pattern Double)
      it "cannot handle fractional with floating point numerator or denominator" $ do
        evaluate ("1.2%5.3" :: Pattern Time)
          `shouldThrow` anyException
      it "can parse a chord" $ do
        compareP (Arc 0 2)
          ("'major" :: Pattern Int)
          ("[0,4,7]")
      it "can parse two chords" $ do
        compareP (Arc 0 2)
          ("'major 'minor" :: Pattern Int)
          ("[0,4,7] [0,3,7]")
      it "can parse c chords" $ do
        compareP (Arc 0 2)
          ("'major 'minor 'dim7" :: Pattern Int)
          ("c'major c'minor c'dim7")
      it "can parse various chords" $ do
        compareP (Arc 0 2)
          ("c'major e'minor f'dim7" :: Pattern Int)
          ("c e f" + "'major 'minor 'dim7")
      it "can parse note chords" $ do
        compareP (Arc 0 2)
          ("c'major c'minor" :: Pattern Note)
          ("'major 'minor")
      it "can invert chords" $ do
        compareP (Arc 0 2)
          ("c'major'i" :: Pattern Note)
          ("[4,7,12]")
      it "can invert chords using a number" $ do
        compareP (Arc 0 2)
          ("c'major'i2" :: Pattern Note)
          ("[7,12,16]")
      it "spread chords over a range" $ do
        compareP (Arc 0 2)
          ("c'major'5 e'min7'5" :: Pattern Note)
          ("[0,4,7,12,16] [4,7,11,14,16]")
      it "can open chords" $ do
        compareP (Arc 0 2)
          ("c'major'o" :: Pattern Note)
          ("[-12,-5,4]")
      it "can drop notes in a chord" $ do
        compareP (Arc 0 2)
          ("c'major'd1" :: Pattern Note)
          ("[-5,0,4]")
      it "can apply multiple modifiers" $ do
        compareP (Arc 0 2)
          ("c'major'i'5" :: Pattern Note)
          ("[4,7,12,16,19]")
      it "can pattern modifiers" $ do
        compareP (Arc 0 2)
          ("c'major'<i 5>" :: Pattern Note)
          ("<[4,7,12] [0,4,7,12,16]>")
      it "can pattern chord names" $ do
        compareP (Arc 0 2)
          ("c'<major minor>'i" :: Pattern Note)
          ("<[4,7,12] [3,7,12]>")
      it "can pattern chord notes" $ do
        compareP (Arc 0 2)
          ("<c e>'<major minor>'i" :: Pattern Note)
          ("<[4,7,12] [7,11,16]>")
      it "handle trailing and leading whitespaces" $ do
        compareP (Arc 0 1)
          ("  bd  " :: Pattern String)
          ("bd" :: Pattern String)
      it "can parse negative ratio shorthands" $ do
        compareP (Arc 0 1)
          ("h -h" :: Pattern Double)
          ("0.5 -0.5" :: Pattern Double)
      it "can parse multiplied ratio shorthands" $ do
        compareP (Arc 0 1)
          ("3h -2q 1.5q" :: Pattern Double)
          ("1.5 -0.5 0.375" :: Pattern Double)
      it "can parse exponential notation value for pattern double" $ do
        compareP (Arc 0 1)
          ("1e3" :: Pattern Double)
          ("1000" :: Pattern Double)
      it "can parse negative exponential notation value for pattern double" $ do
        compareP (Arc 0 1)
          ("400e-3" :: Pattern Double)
          ("0.4" :: Pattern Double)
      it "can parse ratio shortand on exponential notation value" $ do
        compareP (Arc 0 1)
          ("4e2q" :: Pattern Double)
          ("100" :: Pattern Double)
      it "can parse euclid pattern" $ do
        compareP (Arc 0 1)
          ("bd(3,8,1)" :: Pattern String)
          ("~ ~ bd ~ ~ bd ~ bd")
      it "can parse euclid bool pattern" $ do
        compareP (Arc 0 1)
          ("t(3,8,1)" :: Pattern Bool)
          ("f f t f f t f t")
      it "doesn't crash on zeroes (1)" $ do
        compareP (Arc 0 2)
          ("cp/0" :: Pattern String)
          (silence)
      it "doesn't crash on zeroes (2)" $ do
        compareP (Arc 0 2)
          ("cp(5,0)" :: Pattern String)
          (silence)
      it "doesn't crash on zeroes (3)" $ do
        compareP (Arc 0 2)
          ("cp(5,c)" :: Pattern String)
          (silence)
      it "can't parse a floating point number as int" $ do
        evaluate ("1.5" :: Pattern Int)
          `shouldThrow` anyException
      it "can correctly parse multiplied boolean patterns 1" $ do
        compareP (Arc 0 1)
          ("t*2 t*3" :: Pattern Bool)
          ("1*2 1*3" :: Pattern Bool)
      it "can correctly parse multiplied boolean patterns 2" $ do
        compareP (Arc 0 1)
          ("t*2t t" :: Pattern Bool)
          ("1*2%3 1" :: Pattern Bool)
      it "does the same for '-' and '~' in simple patterns" $ do
        compareP (Arc 0 1)
          ("- 2" :: Pattern String)
          ("~ 2" :: Pattern String)
      it "does the same for '-' and '~' in complex patterns" $ do
        compareP (Arc 0 1)
          ("[-- 2 <-- 2@7 3> 1*4%2 3? 4 9|8 -- [-- <2 9q> -]] 2!4" :: Pattern String)
          ("[~~ 2 <~~ 2@7 3> 1*4%2 3? 4 9|8 ~~ [~~ <2 9q> ~]] 2!4" :: Pattern String)
      it "does the same for '-' and '~' using rational numbers" $ do
        compareP (Arc 0 1)
          ("- 2q -3.999-9" :: Pattern String)
          ("~ 2q -3.999-9" :: Pattern String)
      it "does the same for '-' and '~' in list patterns" $ do
        compareP (Arc 0 1)
          ("[-- 2 -- -]" :: Pattern String)
          ("[~~ 2 ~~ ~]" :: Pattern String)
      it "does the same for '-' and '~' alternating patterns" $ do
        compareP (Arc 0 1)
          ("<-- 2 -- - 8>" :: Pattern String)
          ("<~~ 2 ~~ ~ 8>" :: Pattern String)
    where degradeByDefault = _degradeBy 0.5

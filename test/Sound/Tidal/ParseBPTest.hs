{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.ParseBPTest where

import           Control.Exception
import           Test.Microspec
import           TestUtils

import           Prelude                    hiding ((*>), (<*))

import           Sound.Tidal.ExceptionsTest (anyException, shouldThrow)
import           Sound.Tidal.ParseBP
import           Sound.Tidal.Pattern        (cat, fast, fastCat, fastcat,
                                             silence, slow, stack, timeCat)
import           Sound.Tidal.Signal.Random  (_degradeBy)
import           Sound.Tidal.Types

run :: Microspec ()
run =
  describe "Sound.Tidal.ParseBP" $ do
    describe "parseBP_E" $ do
      it "can parse strings" $ do
        compareP (Arc 0 12)
          ("a b c" :: Signal String)
          (fastCat ["a", "b", "c"])
      it "can parse ints" $ do
        compareP (Arc 0 2)
          ("0 1 2 3 4 5 6 7 8 0 10 20 30 40 50" :: Signal Int)
          (fastCat $ map (pure . read) $ words "0 1 2 3 4 5 6 7 8 0 10 20 30 40 50")
      it "can parse pattern groups" $ do
        compareP (Arc 0 1)
          ("[bd sd] hh" :: Signal String)
          (fastCat ["bd sd", "hh"])
      it "can parse pattern groups shorthand " $ do
        compareP (Arc 0 1)
          ("bd sd . hh hh hh" :: Signal String)
          ("[bd sd] [hh hh hh]")
      it "can alternate with <>" $ do
        compareP (Arc 0 2)
          ("a <b c>" :: Signal String)
          (cat [fastCat ["a", "b"], fastCat ["a", "c"]])
      it "can slow with /" $ do
        compareP (Arc 0 2)
          ("a/2" :: Signal String)
          (slow 2 $ "a")
      it "can speed up with *" $ do
        compareP (Arc 0 2)
          ("a*8" :: Signal String)
          (fast 8 "a")
      it "can elongate with _" $ do
        compareP (Arc 0 2)
          ("a _ _ b _" :: Signal String)
          (timeCat [(3,"a"), (2,"b")])
      it "can replicate with !" $ do
        compareP (Arc 0 2)
          ("a! b" :: Signal String)
          (fastCat ["a", "a", "b"])
      it "can replicate with ! inside {}" $ do
        compareP (Arc 0 2)
          ("{a a}%2" :: Signal String)
          ("{a!}%2" :: Signal String)
      it "can replicate with ! and number" $ do
        compareP (Arc 0 2)
          ("a!3 b" :: Signal String)
          (fastCat ["a", "a", "a", "b"])
      it "can degrade with ?" $ do
        compareP (Arc 0 1)
          ("a?" :: Signal String)
          (degradeByDefault "a")
      it "can degrade with ? and number" $ do
        compareP (Arc 0 1)
          ("a?0.2" :: Signal String)
          (_degradeBy 0.2 "a")
      it "can degrade with ? for double patterns" $ do
        compareP (Arc 0 1)
          ("0.4 0.5? 0.6" :: Signal Double)
          (fastcat[0.4, degradeByDefault 0.5, 0.6])
      it "can handle ? on replicated value" $ do
        compareP (Arc 0 1)
          ("a!8?" :: Signal String)
          ("[a!8]?" :: Signal String)
      it "can handle ? on streched value" $ do
        compareP (Arc 0 1)
          ("a*4@0.25?" :: Signal String)
          ("[a*4@0.25]?" :: Signal String)
      it "can stretch with @" $ do
        comparePD (Arc 0 1)
          ("a@2 b" :: Signal String)
          (timeCat [(2, "a"),(1,"b")])
      it "can do polymeter with {}" $ do
        compareP (Arc 0 2)
          ("{a b, c d e}" :: Signal String)
          (stack [fastcat [pure "a", pure "b"], slow 1.5 $ fastcat [pure "c", pure "d", pure "e"]])
      it "can parse .. with ints" $ do
        compareP (Arc 0 2)
          ("0 .. 8" :: Signal Int)
          ("0 1 2 3 4 5 6 7 8")
      it "can parse .. with rationals" $ do
        compareP (Arc 0 2)
          ("0 .. 8" :: Signal Rational)
          ("0 1 2 3 4 5 6 7 8")
      it "can parse .. with doubles" $ do
        compareP (Arc 0 2)
          ("0.0 .. 8.0" :: Signal Double)
          ("0 1 2 3 4 5 6 7 8")
      it "can parse .. with doubles, without spaces" $ do
        compareP (Arc 0 2)
          ("0.0..8.0" :: Signal Double)
          ("0 1 2 3 4 5 6 7 8")
      it "can parse .. with notes" $ do
        compareP (Arc 0 2)
          ("0.0 .. 8.0" :: Signal Note)
          ("0 1 2 3 4 5 6 7 8")
      it "can parse .. with notes, without spaces" $ do
        compareP (Arc 0 2)
          ("0..8" :: Signal Note)
          ("0 1 2 3 4 5 6 7 8")
      it "can handle repeats (!) and durations (@) with <>" $ do
        compareP (Arc 0 31)
          ("<a!3 b! c@5>" :: Signal String)
          (slow 10 "[a a a b b] c")
      it "can handle repeats (!) and durations (@) with <> (with ints)" $ do
        compareP (Arc 0 31)
          ("<1!3 2! 3@5>" :: Signal Int)
          (slow 10 "[1 1 1 2 2] 3")
      it "can handle fractional durations" $ do
        compareP (Arc 0 2)
          ("a@0.5 b@1%6 b@1%6 b@1%6" :: Signal String)
          ("a b*3")
      it "can handle fractional durations (with rationals)" $ do
        compareP (Arc 0 2)
          ("1%3@0.5 3%4@1%6 3%4@1%6 3%4@1%6" :: Signal Rational)
          ("1%3 0.75*3")
      it "can handle ratio shortands on a fraction" $ do
        compareP (Arc 0 1)
          ("1%3t" :: Signal Rational)
          ("1%9" :: Signal Rational)
      it "can handle ratio shortands on a floating point number" $ do
        compareP (Arc 0 1)
          ("3.33t" :: Signal Double)
          ("1.11" :: Signal Double)
      it "cannot handle fractional with floating point numerator or denominator" $ do
        evaluate ("1.2%5.3" :: Signal Time)
          `shouldThrow` anyException
      it "can parse a chord" $ do
        compareP (Arc 0 2)
          ("'major" :: Signal Int)
          ("[0,4,7]")
      it "can parse two chords" $ do
        compareP (Arc 0 2)
          ("'major 'minor" :: Signal Int)
          ("[0,4,7] [0,3,7]")
      it "can parse c chords" $ do
        compareP (Arc 0 2)
          ("'major 'minor 'dim7" :: Signal Int)
          ("c'major c'minor c'dim7")
      it "can parse various chords" $ do
        compareP (Arc 0 2)
          ("c'major e'minor f'dim7" :: Signal Int)
          ("c e f" + "'major 'minor 'dim7")
      it "can parse note chords" $ do
        compareP (Arc 0 2)
          ("c'major c'minor" :: Signal Note)
          ("'major 'minor")
      it "can invert chords" $ do
        compareP (Arc 0 2)
          ("c'major'i" :: Signal Note)
          ("[4,7,12]")
      it "can invert chords using a number" $ do
        compareP (Arc 0 2)
          ("c'major'i2" :: Signal Note)
          ("[7,12,16]")
      it "spread chords over a range" $ do
        compareP (Arc 0 2)
          ("c'major'5 e'min7'5" :: Signal Note)
          ("[0,4,7,12,16] [4,7,11,14,16]")
      it "can open chords" $ do
        compareP (Arc 0 2)
          ("c'major'o" :: Signal Note)
          ("[-12,-5,4]")
      it "can drop notes in a chord" $ do
        compareP (Arc 0 2)
          ("c'major'd1" :: Signal Note)
          ("[-5,0,4]")
      it "can apply multiple modifiers" $ do
        compareP (Arc 0 2)
          ("c'major'i'5" :: Signal Note)
          ("[4,7,12,16,19]")
      it "can pattern modifiers" $ do
        compareP (Arc 0 2)
          ("c'major'<i 5>" :: Signal Note)
          ("<[4,7,12] [0,4,7,12,16]>")
      it "can pattern chord names" $ do
        compareP (Arc 0 2)
          ("c'<major minor>'i" :: Signal Note)
          ("<[4,7,12] [3,7,12]>")
      it "can pattern chord notes" $ do
        compareP (Arc 0 2)
          ("<c e>'<major minor>'i" :: Signal Note)
          ("<[4,7,12] [7,11,16]>")
      it "handle trailing and leading whitespaces" $ do
        compareP (Arc 0 1)
          ("  bd  " :: Signal String)
          ("bd" :: Signal String)
      it "can parse negative ratio shorthands" $ do
        compareP (Arc 0 1)
          ("h -h" :: Signal Double)
          ("0.5 -0.5" :: Signal Double)
      it "can parse multiplied ratio shorthands" $ do
        compareP (Arc 0 1)
          ("3h -2q 1.5q" :: Signal Double)
          ("1.5 -0.5 0.375" :: Signal Double)
      it "can parse exponential notation value for pattern double" $ do
        compareP (Arc 0 1)
          ("1e3" :: Signal Double)
          ("1000" :: Signal Double)
      it "can parse negative exponential notation value for pattern double" $ do
        compareP (Arc 0 1)
          ("400e-3" :: Signal Double)
          ("0.4" :: Signal Double)
      it "can parse ratio shortand on exponential notation value" $ do
        compareP (Arc 0 1)
          ("4e2q" :: Signal Double)
          ("100" :: Signal Double)
      it "can parse euclid pattern" $ do
        compareP (Arc 0 1)
          ("bd(3,8,1)" :: Signal String)
          ("~ ~ bd ~ ~ bd ~ bd")
      it "can parse euclid bool pattern" $ do
        compareP (Arc 0 1)
          ("t(3,8,1)" :: Signal Bool)
          ("f f t f f t f t")
      it "doesn't crash on zeroes (1)" $ do
        compareP (Arc 0 2)
          ("cp/0" :: Signal String)
          (silence)
      it "doesn't crash on zeroes (2)" $ do
        compareP (Arc 0 2)
          ("cp(5,0)" :: Signal String)
          (silence)
      it "doesn't crash on zeroes (3)" $ do
        compareP (Arc 0 2)
          ("cp(5,c)" :: Signal String)
          (silence)
      it "can't parse a floating point number as int" $ do
        evaluate ("1.5" :: Signal Int)
          `shouldThrow` anyException
      it "can correctly parse multiplied boolean patterns 1" $ do
        compareP (Arc 0 1)
          ("t*2 t*3" :: Signal Bool)
          ("1*2 1*3" :: Signal Bool)
      it "can correctly parse multiplied boolean patterns 2" $ do
        compareP (Arc 0 1)
          ("t*2t t" :: Signal Bool)
          ("1*2%3 1" :: Signal Bool)
    where degradeByDefault = _degradeBy 0.5

{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.UITest where

import qualified Data.Map.Strict as Map
-- import Sound.Tidal.Pattern
import Sound.Tidal.Control ( _chop )
import Sound.Tidal.Core
    ( sig, sine, saw, (|+), (>|), (#), run, cat, fastcat, (<~) )
import Sound.Tidal.Params ( sound, begin, crush, end, n, speed, s )
import Sound.Tidal.ParseBP ( parseBP_E )
import Sound.Tidal.Pattern
import Sound.Tidal.UI
import Test.Hspec ( describe, it, shouldBe, Spec )
import TestUtils ( compareP, comparePD, compareTol, ps )
import Prelude hiding ((*>), (<*))

run :: Spec
run =
  describe "Sound.Tidal.UI" $ do
    describe "_chop" $ do
      it "can chop in two bits" $ do
        compareP
          (Arc 0 1)
          (_chop 2 $ s (pure "a"))
          (begin (fastcat [pure 0, pure 0.5]) # end (fastcat [pure 0.5, pure 1]) # (s (pure "a")))
      it "can be slowed" $ do
        compareP
          (Arc 0 1)
          (slow 2 $ _chop 2 $ s (pure "a"))
          (begin (pure 0) # end (pure 0.5) # s (pure "a"))
      it "can chop a chop" $
        compareTol (Arc 0 1) (_chop 6 $ s $ pure "a") (_chop 2 $ _chop 3 $ s $ pure "a")

    describe "segment" $ do
      it "can turn a single event into multiple events" $ do
        compareP
          (Arc 0 3)
          (segment 4 "x")
          ("x*4" :: Pattern String)
      it "can turn a continuous pattern into multiple discrete events" $ do
        compareP
          (Arc 0 3)
          (segment 4 saw)
          ("0 0.25 0.5 0.75" :: Pattern Double)
      it "can hold a value over multiple cycles" $ do
        comparePD
          (Arc 0 8)
          (segment 0.5 saw)
          (slow 2 "0" :: Pattern Double)
    {-
    -- not sure what this is supposed to do!
    it "holding values over multiple cycles works in combination" $ do
      comparePD (Arc 0 8)
        ("0*4" |+ (_segment (1/8) $ saw))
        ("0*4" :: Pattern Double)
    -}

    describe "rolledBy" $ do
      it "shifts each start of events in a list correctly" $ do
        let overTimeSpan = (Arc 0 1)
            testMe = rolledBy "0.5" $ n ("[0,1,2,3]")
            expectedResult = n "[0, ~ 1@7, ~@2 2@6, ~@3 3@5]"
         in compareP overTimeSpan testMe expectedResult
      it "shifts each start of events in a list correctly in reverse order" $ do
        let overTimeSpan = (Arc 0 1)
            testMe = rolledBy "-0.5" $ n ("[0,1,2,3]")
            expectedResult = n "[3, ~ 2@7, ~@2 1@6, ~@3 0@5]"
         in compareP overTimeSpan testMe expectedResult
      it "trims the result pattern if it becomes larger than the original pattern" $ do
        let overTimeSpan = (Arc 0 1)
            testMe = rolledBy "1.5" $ n ("[0,1,2]")
            expectedResult = n "[0, ~ 1]"
         in compareP overTimeSpan testMe expectedResult
      it "does nothing for continous functions" $ do
        let overTimeSpan = (Arc 0 1)
            testMe = n (rolledBy "0.25" (irand 0) |+ "[0,12]")
            expectedResult = n (irand 0) |+ n "[0, 12]"
         in compareP overTimeSpan testMe expectedResult
      it "does nothing when passing zero as time value" $ do
        let overTimeSpan = (Arc 0 1)
            testMe = n (rolledBy "0" "[0,1,2,3]")
            expectedResult = n "[0,1,2,3]"
         in compareP overTimeSpan testMe expectedResult

    describe "sometimesBy" $ do
      it "does nothing when set at 0% probability" $ do
        let overTimeSpan = (Arc 0 1)
            testMe = sometimesBy 0 (rev) (ps "bd*2 hh sn")
            expectedResult = ps "bd*2 hh sn"
         in compareP overTimeSpan testMe expectedResult

      it "applies the 'rev' function when set at 100% probability" $ do
        let overTimeSpan = (Arc 0 1)
            testMe = sometimesBy 1 (rev) (ps "bd*2 hh cp")
            expectedResult = ps "cp hh bd*2"
         in compareP overTimeSpan testMe expectedResult

    describe "sometimesBy'" $ do
      it "does nothing when set at 0% probability -- using const" $ do
        let overTimeSpan = (Arc 0 2)
            testMe = sometimesBy' 0 (const $ s "cp") (s "bd*8")
            expectedResult = s "bd*8"
         in compareP overTimeSpan testMe expectedResult

    describe "rand" $ do
      it "it generates a (pseudo-)random number between 0 and 1 at the start of a cycle" $
        (queryArc rand (Arc 0 0)) `shouldBe` [Event (Context []) Nothing (Arc 0 0) (0 :: Float)]
      it "it generates a (pseudo-)random number between 0 and 1 at 1/4 of a cycle" $
        (queryArc rand (Arc 0.25 0.25))
          `shouldBe` [Event (Context []) Nothing (Arc 0.25 0.25) (0.6295689214020967 :: Float)]
      it "it generates a (pseudo-)random number between 0 and 1 at 3/4 of a cycle" $
        (queryArc rand (Arc 0.75 0.75))
            `shouldBe` [Event (Context []) Nothing (Arc 0.75 0.75) (0.20052618719637394 :: Float)]

    describe "irand" $ do
      -- it "generates a (pseudo-random) integer between zero & i" $ do
      it "at the start of a cycle" $
        (queryArc (irand 10) (Arc 0 0)) `shouldBe` [Event (Context []) Nothing (Arc 0 0) (0 :: Int)]
      it "at 1/4 of a cycle" $
        (queryArc (irand 10) (Arc 0.25 0.25)) `shouldBe` [Event (Context []) Nothing (Arc 0.25 0.25) (6 :: Int)]
      it "is patternable" $
        (queryArc (irand "10 2") (Arc 0 1))
          `shouldBe` [ Event (Context [((1, 1), (3, 1))]) Nothing (Arc 0 0.5) (0 :: Int),
                       Event (Context [((4, 1), (5, 1))]) Nothing (Arc 0.5 1) (0 :: Int)
                     ]

    describe "normal" $ do
      it "produces values within [0,1] in a bell curve at different parts of a cycle" $ do
        queryArc normal (Arc 0 0.1)
          `shouldBe` [Event (Context []) Nothing (Arc 0 0.1) (0.26202741871417984 :: Double)]
        queryArc normal (Arc 0.25 0.25)
          `shouldBe` [Event (Context []) Nothing (Arc 0.25 0.25) (0.5 :: Double)]
        queryArc normal (Arc 0.75 0.75)
          `shouldBe` [Event (Context []) Nothing (Arc 0.75 0.75) (0.5 :: Double)]

    describe "range" $ do
      describe "scales a pattern to the supplied range" $ do
        describe "from 3 to 4" $ do
          it "at the start of a cycle" $
            (queryArc (Sound.Tidal.UI.range 3 4 saw) (Arc 0 0))
              `shouldBe` [Event (Context []) Nothing (Arc 0 0) (3 :: Float)]
          it "at 1/4 of a cycle" $
            (queryArc (Sound.Tidal.UI.range 3 4 saw) (Arc 0.25 0.25))
              `shouldBe` [Event (Context []) Nothing (Arc 0.25 0.25) (3.25 :: Float)]
          it "at 3/4 of a cycle" $
            (queryArc (Sound.Tidal.UI.range 3 4 saw) (Arc 0.75 0.75))
              `shouldBe` [Event (Context []) Nothing (Arc 0.75 0.75) (3.75 :: Float)]

        describe "from -1 to 1" $ do
          it "at 1/2 of a cycle" $
            (queryArc (Sound.Tidal.UI.range (-1) 1 saw) (Arc 0.5 0.5))
              `shouldBe` [Event (Context []) Nothing (Arc 0.5 0.5) (0 :: Float)]

        describe "from 4 to 2" $ do
          it "at the start of a cycle" $
            (queryArc (Sound.Tidal.UI.range 4 2 saw) (Arc 0 0))
              `shouldBe` [Event (Context []) Nothing (Arc 0 0) (4 :: Float)]
          it "at 1/4 of a cycle" $
            (queryArc (Sound.Tidal.UI.range 4 2 saw) (Arc 0.25 0.25))
              `shouldBe` [Event (Context []) Nothing (Arc 0.25 0.25) (3.5 :: Float)]
          it "at 3/4 of a cycle" $
            (queryArc (Sound.Tidal.UI.range 4 2 saw) (Arc 0.75 0.75))
              `shouldBe` [Event (Context []) Nothing (Arc 0.75 0.75) (2.5 :: Float)]

        describe "from 10 to 10" $ do
          it "at 1/2 of a cycle" $
            (queryArc (Sound.Tidal.UI.range 10 10 saw) (Arc 0.5 0.5))
              `shouldBe` [Event (Context []) Nothing (Arc 0.5 0.5) (10 :: Float)]

    describe "rot" $ do
      it "rotates values in a pattern irrespective of structure" $
          comparePD
            (Arc 0 2)
            (rot 1 "a ~ b c" :: Pattern String)
            ("b ~ c a" :: Pattern String)
      it "works with negative values" $
          comparePD
            (Arc 0 2)
            (rot (-1) "a ~ b c" :: Pattern String)
            ("c ~ a b" :: Pattern String)
      it "works with complex patterns" $
          comparePD
            (Arc 0 2)
            (rot (1) "a ~ [b [c ~ d]] [e <f g>]" :: Pattern String)
            ("b ~ [c [d ~ e]] [<f g> a]" :: Pattern String)

    describe "ply" $ do
      it "can ply chords" $ do
        compareP
          (Arc 0 1)
          (ply 3 "[0,1] [3,4,5] 6")
          ("[0,1]*3 [3,4,5]*3 6*3" :: Pattern Int)
      it "can pattern the ply factor" $ do
        compareP
          (Arc 0 1)
          (ply "3 4 5" "[0,1] [3,4,5] 6")
          ("[0,1]*3 [3,4,5]*4 6*5" :: Pattern Int)
    describe "press" $ do
      it "can syncopate a pattern" $ do
        compareP
          (Arc 0 1)
          (press "a b [c d] e")
          ("[~ a] [~ b] [[~ c] [~ d]] [~ e]" :: Pattern String)
    describe "pressBy" $ do
      it "can syncopate a pattern by a given amount" $ do
        compareP
          (Arc 0 1)
          (pressBy (1 / 3) "a b [~ c]")
          ("[~ a@2] [~ b@2] [~ [~ c@2]]" :: Pattern String)
    describe "fix" $ do
      it "can apply functions conditionally" $ do
        compareP
          (Arc 0 1)
          (fix (|+ n 1) (s "sn") (s "bd sn cp" # n 1))
          (s "bd sn cp" # n "1 2 1")
      it "works with complex matches" $ do
        compareP
          (Arc 0 1)
          (fix (|+ n 2) (s "sn" # n 2) (s "bd sn*4 cp" # n "1 2"))
          (s "bd sn*4 cp" # n "1 [1 4] 2")
      it "leaves unmatched controls in place" $ do
        compareP
          (Arc 0 1)
          (fix (|+ n 2) (s "sn" # n 2) (s "bd sn*4 cp" # n "1 2" # speed (sine + 1)))
          (s "bd sn*4 cp" # n "1 [1 4] 2" # speed (sine + 1))
      it "ignores silence" $ do
        compareP
          (Arc 0 1)
          (fix (|+ n 2) (silence) $ s "bd sn*4 cp" # n "1 2" # speed (sine + 1))
          (s "bd sn*4 cp" # n "1 2" # speed (sine + 1))
      it "treats polyphony as 'or'" $ do
        compareP
          (Arc 0 1)
          (fix (# crush 2) (n "[1,2]") $ s "bd sn" # n "1 2")
          (s "bd sn" # n "1 2" # crush 2)

    describe "unfix" $ do
      it "does the opposite of fix" $ do
        compareP
          (Arc 0 1)
          (unfix (|+ n 2) (s "sn" # n 2) (s "bd sn*4 cp" # n "1 2" # speed (sine + 1)))
          (s "bd sn*4 cp" # n "3 [3 2] 4" # speed (sine + 1))

    describe "contrast" $ do
      it "does both fix and unfix" $ do
        compareP
          (Arc 0 1)
          (contrast (|+ n 2) (|+ n 10) (s "sn" # n 2) (s "bd sn*4 cp" # n "1 2" # speed (sine + 1)))
          (s "bd sn*4 cp" # n "11 [11 4] 12" # speed (sine + 1))

    describe "contrastRange" $ do
      it "matches using a pattern of ranges" $ do
        compareP
          (Arc 0 1)
          (contrastRange (# crush 3) (# crush 0) (pure $ Map.singleton "n" $ (VN 0, VN 3)) $ s "bd" >| n "1 4")
          (s "bd" >| n "1 4" >| crush "3 0")

    describe "euclidFull" $ do
      it "can match against silence" $ do
        compareP
          (Arc 0 1)
          (euclidFull 3 8 "bd" silence)
          ("bd(3,8)" :: Pattern String)

    describe "snowball" $ do
      let testPattern = ("1 2 3 4" :: Pattern Int)
      it "acummulates a transform version of a pattern and appends the result - addition" $ do
        compareP
          (Arc 0 1)
          (snowball 3 (+) (slow 2) (testPattern))
          (cat [testPattern, (testPattern + (slow 2 testPattern)), ((testPattern + (slow 2 testPattern)) + slow 2 (testPattern + (slow 2 testPattern)))])

    describe "soak" $ do
      it "applies a transform and then appends the result -- addition" $ do
        compareP
          (Arc 0 3)
          (soak 3 (+ 1) "4 ~ 0 1")
          (cat ["4 ~ 0 1" :: Pattern Int, "5 ~ 1 2" :: Pattern Int, "6 ~ 2 3" :: Pattern Int])
      it "applies a transform and then appends the result -- slow" $ do
        compareP
          (Arc 0 7)
          (soak 3 (slow 2) "4 ~ 0 1")
          (cat ["4 ~ 0 1" :: Pattern Int, slow 2 "4 ~ 0 1" :: Pattern Int, slow 4 "4 ~  0 1" :: Pattern Int])
      it "applies a transform and then appends the result -- addition patterns" $ do
        compareP
          (Arc 0 3)
          (soak 3 (+ "1 2 3") "1 1")
          (cat ["1 1" :: Pattern Int, "2 [3 3] 4" :: Pattern Int, "3 [5 5] 7" :: Pattern Int])

    describe "euclid" $ do
      describe "matches examples in Toussaint's paper" $ do
        mapM_ (\(a, b) -> it b $ compareP (Arc 0 1) a (parseBP_E b))
          ( [ (euclid 1 2 "x", "x ~"),
              (euclid 1 3 "x", "x ~ ~"),
              (euclid 1 4 "x", "x ~ ~ ~"),
              (euclid 4 12 "x", "x ~ ~ x ~ ~ x ~ ~ x ~ ~"),
              (euclid 2 5 "x", "x ~ x ~ ~"),
              -- (euclid 3 4 "x", "x ~ x x"), -- Toussaint is wrong..
              (euclid 3 4 "x", "x x x ~"), -- correction
              (euclid 3 5 "x", "x ~ x ~ x"),
              (euclid 3 7 "x", "x ~ x ~ x ~ ~"),
              (euclid 3 8 "x", "x ~ ~ x ~ ~ x ~"),
              (euclid 4 7 "x", "x ~ x ~ x ~ x"),
              (euclid 4 9 "x", "x ~ x ~ x ~ x ~ ~"),
              (euclid 4 11 "x", "x ~ ~ x ~ ~ x ~ ~ x ~"),
              -- (euclid 5 6 "x", "x ~ x x x x"), -- Toussaint is wrong..
              (euclid 5 6 "x", "x x x x x ~"), -- correction
              (euclid 5 7 "x", "x ~ x x ~ x x"),
              (euclid 5 8 "x", "x ~ x x ~ x x ~"),
              (euclid 5 9 "x", "x ~ x ~ x ~ x ~ x"),
              (euclid 5 11 "x", "x ~ x ~ x ~ x ~ x ~ ~"),
              (euclid 5 12 "x", "x ~ ~ x ~ x ~ ~ x ~ x ~"),
              -- (euclid 5 16 "x", "x ~ ~ x ~ ~ x ~ ~ x ~ ~ x ~ ~ ~ ~"),  -- Toussaint is wrong..
              (euclid 5 16 "x", "x ~ ~ x ~ ~ x ~ ~ x ~ ~ x ~ ~ ~"), -- correction
              -- (euclid 7 8 "x", "x ~ x x x x x x"), -- Toussaint is wrong..
              (euclid 7 8 "x", "x x x x x x x ~"), -- Correction
              (euclid 7 12 "x", "x ~ x x ~ x ~ x x ~ x ~"),
              (euclid 7 16 "x", "x ~ ~ x ~ x ~ x ~ ~ x ~ x ~ x ~"),
              (euclid 9 16 "x", "x ~ x x ~ x ~ x ~ x x ~ x ~ x ~"),
              (euclid 11 24 "x", "x ~ ~ x ~ x ~ x ~ x ~ x ~ ~ x ~ x ~ x ~ x ~ x ~"),
              (euclid 13 24 "x", "x ~ x x ~ x ~ x ~ x ~ x ~ x x ~ x ~ x ~ x ~ x ~")
            ] ::
              [(Pattern String, String)]
          )
      it "can be called with a negative first value to give the inverse" $ do
        compareP
          (Arc 0 1)
          (euclid (-3) 8 ("bd" :: Pattern String))
          (euclidInv 3 8 ("bd" :: Pattern String))
      it "can be called with a negative first value to give the inverse (patternable)" $ do
        compareP
          (Arc 0 1)
          (euclid (-3) 8 ("bd" :: Pattern String))
          ("bd(-3,8)" :: Pattern String)

    describe "wedge" $ do
      it "should not freeze tidal amount is 1" $ do
        compareP
          (Arc 0 1)
          (wedge (1) (s "ho ho:2 ho:3 hc") (rev $ s "ho ho:2 ho:3 hc"))
          (s "ho ho:2 ho:3 hc")
      it "should not freeze tidal amount is 0" $ do
        compareP
          (Arc 0 1)
          (wedge (0) (s "ho ho:2 ho:3 hc") (rev $ s "ho ho:2 ho:3 hc"))
          (rev $ s "ho ho:2 ho:3 hc")

    describe "bite" $ do
      it "can slice a pattern into bits" $ do
        compareP
          (Arc 0 4)
          (bite 4 "0 2*2" (Sound.Tidal.Core.run 8))
          ("[0 1] [4 5]*2" :: Pattern Int)
      it "can slice a pattern into patternable bits number" $ do
        compareP
          (Arc 0 4)
          (bite "8 4" "0 2*2" (Sound.Tidal.Core.run 8))
          ("[0] [4 5]*2" :: Pattern Int)

    describe "chooseBy" $ do
      it "chooses from elements based on closest scaled double value" $ do
        compareP
          (Arc 0 4)
          (("0" :: Pattern Int) |+ chooseBy ((/ 4) $ (sig fromRational)) [0, 1, 2, 3])
          ("<0 1 2 3>" :: Pattern Int)
      it "never gets an index out of bounds" $ do
        compareP
          (Arc 0 4)
          ("0" |+ chooseBy (sig fromRational) [0, 1, 2, 3])
          ("0" :: Pattern Int)

    describe "arpeggiate" $ do
      it "can arpeggiate" $ do
        compareP
          (Arc 0 1)
          (arpeggiate ("[bd, sn] [hh:1, cp]" :: Pattern String))
          ("bd sn hh:1 cp" :: Pattern String)
      it "can arpeggiate" $ do
        compareP
          (Arc 0 4)
          (arpeggiate $ "[0,0] [0,0]")
          ("0 0 0 0" :: Pattern Int)
      it "can arpeggiate a 'sped up' pattern" $ do
        compareP
          (Arc 0 4)
          (arpeggiate $ "[0,0]*2")
          ("0 0 0 0" :: Pattern Int)

    describe "chunk" $ do
      it "can chunk a rev pattern" $ do
        compareP
          (Arc 0 4)
          (chunk 2 (rev) $ ("a b c d" :: Pattern String))
          (slow 2 $ "d c c d a b b a" :: Pattern String)
      it "can chunk a fast pattern" $ do
        compareP
          (Arc 0 4)
          (chunk 2 (fast 2) $ "a b" :: Pattern String)
          (slow 2 $ "a b b _ a _ a b" :: Pattern String)
      it "should chunk backward with a negative number" $ do
        compareP
          (Arc 0 4)
          (chunk (-2) (rev) $ ("a b c d" :: Pattern String))
          (slow 2 $ "a b b a d c c d" :: Pattern String)

    describe "binary" $ do
      it "converts a number to a pattern of boolean" $ do
        compareP
          (Arc 0 1)
          (binary "128")
          ("t f f f f f f f" :: Pattern Bool)

    describe "binaryN" $ do
      it "converts a number to a pattern of boolean of specified length" $ do
        compareP
          (Arc 0 1)
          (binaryN 4 "8")
          ("t f f f" :: Pattern Bool)
      it "converts a number to a pattern of boolean of specified patternable length" $ do
        compareP
          (Arc 0 2)
          (binaryN "<4 8>" "8")
          (cat ["t f f f", "f f f f t f f f"] :: Pattern Bool)

    describe "off" $ do
      it "superimposes and shifts pattern" $ do
        compareP
          (Arc 0 1)
          (off "-e" id $ s "0")
          (superimpose ("e" <~) $ s "0")

    describe "loopFirst" $ do
      it "plays the first cycle" $ do
        compareP
          (Arc 0 1)
          (loopFirst $ rotL 3 $ slow 8 $ "0 .. 7" :: Pattern Int)
          ("3")

    describe "loopCycles" $ do
      it "plays the first n cycles" $ do
        compareP
          (Arc 0 1)
          (loopFirst $ rotL 3 $ slow 8 $ "0 .. 7" :: Pattern Int)
          ("3")

    describe "timeLoop" $ do
      it "can loop time" $ do
        compareP
          (Arc 0 1)
          ((3 <~) $ (timeLoop 3 $ sound "<a b c d>"))
          (sound "a")

    describe "timeLoop" $ do
      it "can pattern time" $ do
        compareP
          (Arc 0 1)
          ((1 <~) $ timeLoop "<2 1>" $ sound "b")
          (sound "b")

    describe "necklace" $ do
      it "can specify rhythm by IOI" $ do
        compareP
          (Arc 0 1)
          (necklace 12 [4, 2])
          ("t f f f t f t f f f t f")

    describe "quantise" $ do
      it "can quantise notes" $ do
        compareP
          (Arc 0 1)
          (segment 2 $ quantise 1 $ sine :: Pattern Note)
          ("0 1" :: Pattern Note)

{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.CoreTest where

import Data.List (sort)
import qualified Data.Map as Map
import Data.Ratio
import Sound.Tidal.Context
import Test.Microspec
import TestUtils
import Prelude hiding ((*>), (<*))

run :: Microspec ()
run =
  describe "Sound.Tidal.Core" $ do
    describe "Elemental patterns" $ do
      let sampleOf :: Pattern Double -> Rational -> Double
          sampleOf pat t = (value . head) $ query pat (State (Arc t t) Map.empty)
      describe "are in range [0, 1]" $ do
        let inNormalRange pat t = (y >= 0) && (y <= 1)
              where
                y = sampleOf pat t
        it "sine" $ inNormalRange sine
        it "cosine" $ inNormalRange cosine
        it "saw" $ inNormalRange saw
        it "isaw" $ inNormalRange isaw
        it "tri" $ inNormalRange tri
        it "square" $ inNormalRange square
      describe "have correctly-scaled bipolar variants" $ do
        let areCorrectlyScaled pat pat2 t = (y * 2 - 1) ~== y2
              where
                y = sampleOf pat t
                y2 = sampleOf pat2 t
        it "sine" $ areCorrectlyScaled sine sine2
        it "cosine" $ areCorrectlyScaled cosine cosine2
        it "saw" $ areCorrectlyScaled saw saw2
        it "isaw" $ areCorrectlyScaled isaw isaw2
        it "tri" $ areCorrectlyScaled tri tri2
        it "square" $ areCorrectlyScaled square square2

    describe "append" $
      it "can switch between the cycles from two pures" $ do
        queryArc (append (pure "a") (pure "b")) (Arc 0 5)
          `shouldBe` fmap
            toEvent
            [ (((0, 1), (0, 1)), "a" :: String),
              (((1, 2), (1, 2)), "b"),
              (((2, 3), (2, 3)), "a"),
              (((3, 4), (3, 4)), "b"),
              (((4, 5), (4, 5)), "a")
            ]

    describe "cat" $ do
      it "can switch between the cycles from three pures" $ do
        queryArc (cat [pure "a", pure "b", pure "c"]) (Arc 0 5)
          `shouldBe` fmap
            toEvent
            [ (((0, 1), (0, 1)), "a" :: String),
              (((1, 2), (1, 2)), "b"),
              (((2, 3), (2, 3)), "c"),
              (((3, 4), (3, 4)), "a"),
              (((4, 5), (4, 5)), "b")
            ]
      it "can extract nested revs" $
        let a = "1 2 3" :: Pattern Int
            b = "4 5 6" :: Pattern Int
            c = "7 8 9" :: Pattern Int
         in comparePD
              (Arc 0 10)
              (rev $ cat [a, b, c])
              (cat [rev a, rev b, rev c])

    describe "fastCat" $ do
      it "can switch between the cycles from three pures inside one cycle" $ do
        it "1" $
          queryArc (fastCat [pure "a", pure "b", pure "c"]) (Arc 0 1)
            `shouldBe` fmap
              toEvent
              [ (((0, 1 / 3), (0, 1 / 3)), "a" :: String),
                (((1 / 3, 2 / 3), (1 / 3, 2 / 3)), "b"),
                (((2 / 3, 1), (2 / 3, 1)), "c")
              ]
        it "5/3" $
          queryArc (fastCat [pure "a", pure "b", pure "c"]) (Arc 0 (5 / 3))
            `shouldBe` fmap
              toEvent
              [ (((0, 1 / 3), (0, 1 / 3)), "a" :: String),
                (((1 / 3, 2 / 3), (1 / 3, 2 / 3)), "b"),
                (((2 / 3, 1), (2 / 3, 1)), "c"),
                (((1, 4 / 3), (1, 4 / 3)), "a"),
                (((4 / 3, 5 / 3), (4 / 3, 5 / 3)), "b")
              ]
      it "works with zero-length queries" $ do
        it "0" $
          queryArc (fastCat [pure "a", pure "b"]) (Arc 0 0)
            `shouldBe` fmap toEvent [(((0, 0.5), (0, 0)), "a" :: String)]
        it "1/3" $
          queryArc (fastCat [pure "a", pure "b"]) (Arc (1 % 3) (1 % 3))
            `shouldBe` fmap toEvent [(((0, 0.5), (1 % 3, 1 % 3)), "a" :: String)]

    describe "rev" $ do
      it "mirrors events" $ do
        let forward = fastCat [fastCat [pure 7, pure 8], pure 9] :: Pattern Int
            backward = fastCat [pure 9, fastCat [pure 8, pure 7]]
        -- sort the events into time order to compare them
        sort (queryArc (rev forward) (Arc 0 1)) `shouldBe` sort (queryArc backward (Arc 0 1))

      it "returns the original if you reverse it twice" $ do
        let x = fastCat [fastCat [pure 7, pure 8], pure 9] :: Pattern Int
        queryArc (rev $ rev x) (Arc 0 5) `shouldBe` queryArc x (Arc 0 5)

    describe "|>|" $ do
      let a = "[1, 1] [2,2] 3" :: Pattern Int
          b = "4 [5, 5] 6 7" :: Pattern Int
          c = "7 8 9 10" :: Pattern Int
          d = "7 [8, 9] 10 11" :: Pattern Int
      it "creates silence when" $ do
        it "first argument silent" $
          comparePD
            (Arc 0 1)
            (silence |>| a)
            silence
        it "second argument silent" $
          comparePD
            (Arc 0 1)
            (a |>| silence)
            silence
      it "creates the same pattern when left argument has the same structure" $
        comparePD
          (Arc 0 1)
          (b |>| a)
          (d |>| a)
      it "can extract rev from first argument" $
        comparePD
          (Arc 0 1)
          (rev a |>| b)
          (rev (a |>| rev b))
      it "is assiociative" $
        comparePD
          (Arc 0 1)
          ((a |>| b) |>| c)
          (a |>| (b |>| c))
      it "is commutative in all arguments except the rightmost" $
        comparePD
          (Arc 0 1)
          (a |>| b |>| c)
          (b |>| a |>| c)

    describe "stack" $ do
      let a = "1 2 3" :: Pattern Int
          b = "4 5 6" :: Pattern Int
          c = "7 8 9" :: Pattern Int
      it "is neutral with silence" $
        comparePD
          (Arc 0 1)
          (stack [a, silence])
          a
      it "can create silence" $
        comparePD
          (Arc 0 1)
          (stack [] :: Pattern Int)
          silence
      it "follows commutative laws" $
        comparePD
          (Arc 0 1)
          (stack [a, b])
          (stack [b, a])
      it "follows assiociative laws" $
        comparePD
          (Arc 0 1)
          (stack [a, stack [b, c]])
          (stack [stack [a, b], c])
      it "can extract nested revs" $
        comparePD
          (Arc 0 1)
          (rev $ stack [a, b, c])
          (stack [rev a, rev b, rev c])

    describe "fast" $ do
      let x = "1 2 3" :: Pattern Time
          y = "4 5 6" :: Pattern Time
      it "is neutral with speedup 1" $
        comparePD
          (Arc 0 1)
          (fast 1 x)
          x
      it "mutes, when there is" $ do
        it "silence in first argument" $
          comparePD
            (Arc 0 1)
            (fast silence x)
            silence
        it "silence in second argument" $
          comparePD
            (Arc 0 1)
            (fast x silence :: Pattern Time)
            silence
        it "speedup by 0" $
          comparePD
            (Arc 0 1)
            (fast 0 x)
            silence
      it "is reciprocal to slow" $
        comparePD
          (Arc 0 1)
          (fast 2 x)
          (slow (fromRational $ 1 % 2) x)
      it "can be reversed by reciprocal speedup" $
        comparePD
          (Arc 0 1)
          (fast 2 $ fast (fromRational $ 1 % 2) x)
          x
      it "preserves structure" $
        comparePD
          (Arc 0 1)
          (fast x (stack [y, y]))
          (fast (stack [x, x]) y)

    describe "slow" $ do
      let x = "1 2 3" :: Pattern Time
          y = "4 5 6" :: Pattern Time
      it "is neutral with slowdown 1" $
        comparePD
          (Arc 0 10)
          (slow 1 x)
          x
      it "mutes, when there is" $ do
        it "silence in first argument" $
          comparePD
            (Arc 0 10)
            (slow silence x)
            silence
        it "silence in second argument" $
          comparePD
            (Arc 0 10)
            (slow x silence :: Pattern Time)
            silence
        it "speedup by 0" $
          comparePD
            (Arc 0 10)
            (slow 0 x)
            silence
      it "is reciprocal to fast" $
        comparePD
          (Arc 0 10)
          (slow 2 x)
          (fast (fromRational $ 1 % 2) x)
      it "can be reversed by reciprocal slowdown" $
        comparePD
          (Arc 0 10)
          (slow 2 $ slow (fromRational $ 1 % 2) x)
          x
      it "preserves structure" $
        comparePD
          (Arc 0 1)
          (slow x (stack [y, y]))
          (slow (stack [x, x]) y)

    describe "compress" $ do
      it "squashes cycles to the start of a cycle" $ do
        let p = compress (0, 0.5) $ fastCat [pure 7, pure 8] :: Pattern Int
        queryArc p (Arc 0 1)
          `shouldBe` fmap
            toEvent
            [ (((0, 0.25), (0, 0.25)), 7),
              (((0.25, 0.5), (0.25, 0.5)), 8)
            ]
      it "squashes cycles to the end of a cycle" $ do
        let p = compress (0.5, 1) $ fastCat [pure 7, pure 8] :: Pattern Int
        queryArc p (Arc 0 1)
          `shouldBe` fmap
            toEvent
            [ (((0.5, 0.75), (0.5, 0.75)), 7 :: Int),
              (((0.75, 1), (0.75, 1)), 8)
            ]
      it "squashes cycles to the middle of a cycle" $ do
        let p = compress (0.25, 0.75) $ fastCat [pure 7, pure 8]
        queryArc p (Arc 0 1)
          `shouldBe` fmap
            toEvent
            [ (((0.25, 0.5), (0.25, 0.5)), 7 :: Int),
              (((0.5, 0.75), (0.5, 0.75)), 8)
            ]

    describe "saw" $ do
      it "goes from 0 up to 1 every cycle" $ do
        it "0" $
          queryArc saw (Arc 0 0) `shouldBe` [Event (Context []) Nothing (Arc 0 0) 0 :: Event Double]
        it "0.25" $
          queryArc saw (Arc 0.25 0.25) `shouldBe` [Event (Context []) Nothing (Arc 0.25 0.25) 0.25 :: Event Double]
        it "0.5" $
          queryArc saw (Arc 0.5 0.5) `shouldBe` [Event (Context []) Nothing (Arc 0.5 0.5) 0.5 :: Event Double]
        it "0.75" $
          queryArc saw (Arc 0.75 0.75) `shouldBe` [Event (Context []) Nothing (Arc 0.75 0.75) 0.75 :: Event Double]
      it "can be added to" $
        map value (queryArc ((+ 1) <$> saw) (Arc 0.5 0.5)) `shouldBe` [1.5 :: Float]
      it "works on the left of <*>" $
        queryArc ((+) <$> saw <*> pure 3) (Arc 0 1)
          `shouldBe` [Event (Context []) Nothing (Arc 0 1) 3.5 :: Event Double]
      it "works on the right of <*>" $
        queryArc (fast 4 (pure (+ 3)) <*> saw) (Arc 0 1)
          `shouldBe` [ Event (Context []) Nothing (Arc 0 0.25) 3.5 :: Event Double,
                       Event (Context []) Nothing (Arc 0.25 0.5) 3.5,
                       Event (Context []) Nothing (Arc 0.5 0.75) 3.5,
                       Event (Context []) Nothing (Arc 0.75 1) 3.5
                     ]
      it "can be reversed" $ do
        it "works with whole cycles" $
          queryArc (rev saw) (Arc 0 1)
            `shouldBe` [Event (Context []) Nothing (Arc 0 1) 0.5 :: Event Double]
        it "works with half cycles" $
          queryArc (rev saw) (Arc 0 0.5)
            `shouldBe` [Event (Context []) Nothing (Arc 0 0.5) 0.75 :: Event Double]
        it "works with inset points" $
          queryArc (rev saw) (Arc 0.25 0.25)
            `shouldBe` [Event (Context []) Nothing (Arc 0.25 0.25) 0.75 :: Event Double]

    describe "tri" $ do
      it "goes from 0 up to 1 and back every cycle" $
        comparePD
          (Arc 0 1)
          (struct "t*8" (tri :: Pattern Double))
          "0.125 0.375 0.625 0.875 0.875 0.625 0.375 0.125"
      it "can be added to" $
        comparePD
          (Arc 0 1)
          (struct "t*8" $ (tri :: Pattern Double) + 1)
          "1.125 1.375 1.625 1.875 1.875 1.625 1.375 1.125"
    describe "every" $
      it "`every n id` doesn't change the pattern's structure" $ do
        comparePD
          (Arc 0 4)
          (every 2 id "x/2" :: Pattern String)
          "x/2"

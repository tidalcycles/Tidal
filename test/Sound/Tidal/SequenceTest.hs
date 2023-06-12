{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.SequenceTest where

import           Test.Microspec
import           TestUtils

import           Prelude                 hiding ((*>), (<*))

import           Sound.Tidal.Pattern
import           Sound.Tidal.Sequence
import           Sound.Tidal.Signal.Base (queryArc)
import           Sound.Tidal.Types

a :: Time -> Time -> Time -> Maybe a -> Sequence a
a = Atom (Metadata [])

metaless a b = (stripMetadata a) `shouldBe` (stripMetadata b)

run :: Microspec ()
run =
  describe "Sound.Tidal.Sequence" $ do
    describe "pairAligned" $ do
      it "Aligns pairs of events" $ do
        (pairAligned In ("10 20", "1 2") :: Sequence (Int, Int))
          `metaless`
          Cat [step 1 (10,1), step 1 (20,2)]
    describe "pairAlign" $ do
      it "Can centre two sequences, paired into structure of the first one." $ do
        (pairAlign Centre In "10" "1 2")
          `metaless`
          (Cat [a 0.5 0 0 Nothing,
                a 0.5 0 0.5 $ Just (10,1),
                a 0.5 0.5 0 $ Just (10,2),
                a 0.5 0 0 Nothing
               ] :: Sequence (Int,Int))
    describe "alignF" $ do
      it "Can align and combine two sequences by expansion and addition" $ do
        ((alignF Expand In (+) "0 1 2" "10 20") :: Sequence Int)
          `metaless`
          (Cat [a 1 0 0 $ Just 10, a 0.5 0 0.5 $ Just 11, a 0.5 0.5 0 $ Just 21,
                a 1 0 0 $ Just 22])
      it "Can align and combine subsequences by expansion and addition with subsequence" $ do
        ((alignF Expand In (+) "0 [1 2] 3" "10 20") :: Sequence Int)
          `metaless`
          (Cat [a 1 0 0 $ Just 10, a 0.5 0 0 $ Just 11, a 0.5 0 0 $ Just 22,
                a 1 0 0 $ Just 23])
      it "Can align and combine subsequences by Expansion and addition with subsequences on both sides" $ do
        ((alignF Expand In (+) "0 [1 2] 3" "10 [20 30]") :: Sequence Int)
          `metaless`
          (Cat [a 1 0 0 $ Just 10, a 0.5 0 0 $ Just 11, a 0.5 0 0 $ Just 22,
                a 0.25 0 0.75 $ Just 23, a 0.75 0.25 0 $ Just 33])
    describe "beatMode" $ do
      it "Can turn a sequence into a signal" $ do
        (queryArc (stripMetadata (seqToSignal' ( alignF Centre Out (+) ("10 20 30") ("1 2")) :: Signal Int)) (Arc 0 1))
          `shouldBe`
          [Event (Metadata []) (Just $ Arc (1/6) (1/2)) (Arc (1/6) (1/3)) 11,
           Event (Metadata []) (Just $ Arc (1/6) (1/2)) (Arc (1/3) (1/2)) 21,
           Event (Metadata []) (Just $ Arc (1/2) (5/6)) (Arc (1/2) (2/3)) 22,
           Event (Metadata []) (Just $ Arc (1/2) (5/6)) (Arc (2/3) (5/6)) 32
          ]
      it "Can convert half an event" $ do
        compareP (Arc 0 1)
          (beatMode 0.5 $ Atom (Metadata []) 0.5 0 0.5 (Just 'a'))
          (_slow 2 $ pure 'a')


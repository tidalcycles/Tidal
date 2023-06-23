{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.SequenceTest where

import           Test.Microspec          (MTestable (describe), Microspec,
                                          Property, it, shouldBe)
import           TestUtils               (stripSequenceMetadata)

import           Prelude                 hiding ((*>), (<*))

import           Sound.Tidal.Sequence
import           Sound.Tidal.Signal.Base (queryArc)
import           Sound.Tidal.Types       (ArcF (Arc), Direction (In, Out),
                                          Event (Event), Metadata (Metadata),
                                          Sequence (Atom, Cat), Signal,
                                          Strategy (Centre, Expand), Time)

shouldMatch :: (Eq a, Show a) => Sequence a -> Sequence a -> Property
shouldMatch seq1 seq2 = shouldBe (stripSequenceMetadata seq1) (stripSequenceMetadata seq2)

tAtom :: Time -> Time -> Time -> Maybe a -> Sequence a
tAtom = Atom mempty

run :: Microspec ()
run =
  describe "Sound.Tidal.Sequence" $ do
    describe "pairAligned" $ do
      it "Aligns pairs of events" $ do
        (pairAligned In ("10 20", "1 2") :: Sequence (Int, Int))
          `shouldMatch`
          Cat [step 1 (10,1), step 1 (20,2)]
    describe "pairAlign" $ do
      it "Can centre two sequences, paired into structure of the first one." $ do
        (pairAlign Centre In "10" "1 2")
          `shouldMatch`
          (Cat [tAtom 0.5 0 0 Nothing,
                tAtom 0.5 0 0.5 $ Just (10,1),
                tAtom 0.5 0.5 0 $ Just (10,2),
                tAtom 0.5 0 0 Nothing
               ] :: Sequence (Int,Int))
    describe "alignF" $ do
      it "Can align and combine two sequences by Expansion and addition" $ do
        ((alignF Expand In (+) "0 1 2" "10 20") :: Sequence Int)
          `shouldMatch`
          (Cat [tAtom 1 0 0 $ Just 10, tAtom 0.5 0 0.5 $ Just 11, tAtom 0.5 0.5 0 $ Just 21,
                tAtom 1 0 0 $ Just 22])
      it "Can align and combine subsequences by Expansion and addition" $ do
        ((alignF Expand In (+) "0 [1 2] 3" "10 20") :: Sequence Int)
          `shouldMatch`
          (Cat [tAtom 1 0 0 $ Just 10, tAtom 0.5 0 0 $ Just 11, tAtom 0.5 0 0 $ Just 22,
                tAtom 1 0 0 $ Just 23])
      it "Can align and combine subsequences by Expansion and addition" $ do
        ((alignF Expand In (+) "0 [1 2] 3" "10 [20 30]") :: Sequence Int)
          `shouldMatch`
          (Cat [tAtom 1 0 0 $ Just 10, tAtom 0.5 0 0 $ Just 11, tAtom 0.5 0 0 $ Just 22,
                tAtom 0.25 0 0.75 $ Just 23, tAtom 0.75 0.25 0 $ Just 33])
    describe "beatMode" $ do
      it "Can turn a sequence into a signal" $ do
        (queryArc ((seqToSignal' (stripSequenceMetadata $ alignF Centre Out (+) ("10 20 30") ("1 2")) :: Signal Int)) (Arc 0 1))
          `shouldBe`
          [Event (Metadata []) (Just $ Arc (1/6) (1/2)) (Arc (1/6) (1/3)) 11,
           Event (Metadata []) (Just $ Arc (1/6) (1/2)) (Arc (1/3) (1/2)) 21,
           Event (Metadata []) (Just $ Arc (1/2) (5/6)) (Arc (1/2) (2/3)) 22,
           Event (Metadata []) (Just $ Arc (1/2) (5/6)) (Arc (2/3) (5/6)) 32
          ]

{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.SequenceTest where

import           Sound.Tidal.TestUtils (compareP, stripSequenceMetadata)
import           Test.Microspec        (MTestable (describe), Microspec,
                                        Property, it, shouldBe)

import           Prelude               hiding ((*>), (<*))

import           Sound.Tidal.Pattern   (_slow, stripMetadata)
import           Sound.Tidal.Sequence
import           Sound.Tidal.Signal    (querySpan)
import           Sound.Tidal.Types     (Alignment (Centre, Expand),
                                        Event (Event), Metadata (Metadata),
                                        Pattern, Sequence (Atom, Cat),
                                        SequenceBind (SeqIn, SeqOut), Signal,
                                        Span (Span), Time)

shouldMatch :: (Eq a, Show a) => Sequence a -> Sequence a -> Property
shouldMatch seq1 seq2 = shouldBe (stripSequenceMetadata seq1) (stripSequenceMetadata seq2)

tAtom :: Time -> Time -> Time -> Maybe a -> Sequence a
tAtom = Atom mempty

a :: Time -> Time -> Time -> Maybe a -> Sequence a
a = Atom mempty

metaless :: (Eq (p a), Show (p a), Pattern p) => p a -> p a -> Property
metaless x y = (stripMetadata x) `shouldBe` (stripMetadata y)

run :: Microspec ()
run =
  describe "Sound.Tidal.Sequence" $ do
    describe "pairAligned" $ do
      it "Aligns pairs of events" $ do
        (pairAligned SeqIn ("10 20", "1 2") :: Sequence (Int, Int))
          `metaless`
          Cat [step 1 (10,1), step 1 (20,2)]
    describe "pairAlign" $ do
      it "Can centre two sequences, paired into structure of the first one." $ do
        (pairAlign Centre SeqIn "10" "1 2")
          `metaless`
          (Cat [a 0.5 0 0 Nothing,
                a 0.5 0 0.5 $ Just (10,1),
                a 0.5 0.5 0 $ Just (10,2),
                a 0.5 0 0 Nothing
               ] :: Sequence (Int,Int))
    describe "alignF" $ do
      it "Can align and combine two sequences by expansion and addition" $ do
        ((alignF Expand SeqIn (+) "0 1 2" "10 20") :: Sequence Int)
          `metaless`
          (Cat [a 1 0 0 $ Just 10, a 0.5 0 0.5 $ Just 11, a 0.5 0.5 0 $ Just 21,
                a 1 0 0 $ Just 22])
      it "Can align and combine subsequences by expansion and addition with subsequence" $ do
        ((alignF Expand SeqIn (+) "0 [1 2] 3" "10 20") :: Sequence Int)
          `metaless`
          (Cat [a 1 0 0 $ Just 10, a 0.5 0 0 $ Just 11, a 0.5 0 0 $ Just 22,
                a 1 0 0 $ Just 23])
      it "Can align and combine subsequences by Expansion and addition with subsequences on both sides" $ do
        ((alignF Expand SeqIn (+) "0 [1 2] 3" "10 [20 30]") :: Sequence Int)
          `metaless`
          (Cat [a 1 0 0 $ Just 10, a 0.5 0 0 $ Just 11, a 0.5 0 0 $ Just 22,
                a 0.25 0 0.75 $ Just 23, a 0.75 0.25 0 $ Just 33])
    describe "beatMode" $ do
      it "Can turn a sequence into a signal" $ do
        (querySpan (stripMetadata (seqToSignal' ( alignF Centre SeqOut (+) ("10 20 30") ("1 2")) :: Signal Int)) (Span 0 1))
          `shouldBe`
          [Event mempty (Just $ Span (1/6) (1/2)) (Span (1/6) (1/3)) 11,
           Event mempty (Just $ Span (1/6) (1/2)) (Span (1/3) (1/2)) 21,
           Event mempty (Just $ Span (1/2) (5/6)) (Span (1/2) (2/3)) 22,
           Event mempty (Just $ Span (1/2) (5/6)) (Span (2/3) (5/6)) 32
          ]
      it "Can convert half an event" $ do
        compareP (Span 0 1)
          (beatMode 0.5 $ Atom mempty 0.5 0 0.5 (Just 'a'))
          (_slow 2 $ pure 'a')


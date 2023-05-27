{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.SignalControlTest where

import           Test.Microspec
import           TestUtils

import           Prelude                     hiding ((*>), (<*))

import           Sound.Tidal.Compose
import           Sound.Tidal.Params
import           Sound.Tidal.Pattern
import           Sound.Tidal.Signal.Base
import           Sound.Tidal.Signal.Control
import           Sound.Tidal.Signal.Random   (irand)
import           Sound.Tidal.Signal.Waveform (sine)
import           Sound.Tidal.Types

import qualified Data.Map.Strict             as Map

run :: Microspec ()
run =
  describe "Sound.Tidal.Control" $ do

    describe "echo" $ do
      it "should echo the event by the specified time and multiply the gain factor" $ do
        comparePD (Arc 0 1)
          (echo 3 0.2 0.5 $ s "bd" # gain "1")
          (stack [
            late 0 $ s "bd" # gain 1,
            late 0.2 $ s "bd" # gain 0.5,
            late 0.4 $ s "bd" # gain 0.25
          ])
    describe "echoWith" $ do
      it "should echo the event by the specified time and apply the specified function" $ do
        comparePD (Arc 0 1)
          (echoWith 3 0.25 (|* speed 2) $ s "bd" # speed "1")
          (stack [
            late 0 $ s "bd" # speed 1,
            late 0.25 $ s "bd" # speed 2,
            late 0.5 $ s "bd" # speed 4
          ])

    describe "splice" $ do
      it "can beatslice" $ do
        comparePD (Arc 0 1)
          (splice "4 8" "0 1" $ sound "bev")
          (begin "0 0.125" # end "0.25 0.25" # speed "0.5 0.25" # sound "bev" # unit "c")

    describe "_chop" $ do
      it "can chop in two bits" $ do
        compareP (Arc 0 1)
          (_chop 2 $ s (pure "a"))
          (begin (fastcat [pure 0, pure 0.5]) # end (fastcat [pure 0.5, pure 1]) # (s (pure "a")))
      it "can be slowed" $ do
        compareP (Arc 0 1)
          (slow 2 $ _chop 2 $ s (pure "a"))
          (begin (pure 0) # end (pure 0.5) # (s (pure "a")))
      it "can chop a chop" $
        property $ compareTol (Arc 0 1) (_chop 6 $ s $ pure "a") (_chop 2 $ _chop 3 $ s $ pure "a")



    describe "fix" $ do
      it "can apply functions conditionally" $ do
        compareP (Arc 0 1)
          (fix (|+ n 1) (s "sn") (s "bd sn cp" # n 1))
          (s "bd sn cp" # n "1 2 1")
      it "works with complex matches" $ do
        compareP (Arc 0 1)
          (fix (|+ n 2) (s "sn" # n 2) (s "bd sn*4 cp" # n "1 2"))
          (s "bd sn*4 cp" # n "1 [1 4] 2")
      it "leaves unmatched controls in place" $ do
        compareP (Arc 0 1)
          (fix (|+ n 2) (s "sn" # n 2) (s "bd sn*4 cp" # n "1 2" # speed (sine + 1)))
          (s "bd sn*4 cp" # n "1 [1 4] 2" # speed (sine + 1))
      it "ignores silence" $ do
        compareP (Arc 0 1)
          (fix (|+ n 2) (silence) $ s "bd sn*4 cp" # n "1 2" # speed (sine + 1))
          (s "bd sn*4 cp" # n "1 2" # speed (sine + 1))
      it "treats polyphony as 'or'" $ do
        compareP (Arc 0 1)
          (fix (# crush 2) (n "[1,2]") $ s "bd sn" # n "1 2")
          (s "bd sn" # n "1 2" # crush 2)

    describe "unfix" $ do
      it "does the opposite of fix" $ do
        compareP (Arc 0 1)
          (unfix (|+ n 2) (s "sn" # n 2) (s "bd sn*4 cp" # n "1 2" # speed (sine + 1)))
          (s "bd sn*4 cp" # n "3 [3 2] 4" # speed (sine + 1))

    describe "contrast" $ do
      it "does both fix and unfix" $ do
        compareP (Arc 0 1)
          (contrast (|+ n 2) (|+ n 10) (s "sn" # n 2) (s "bd sn*4 cp" # n "1 2" # speed (sine + 1)))
          (s "bd sn*4 cp" # n "11 [11 4] 12" # speed (sine + 1))

    describe "contrastRange" $ do
      it "matches using a pattern of ranges" $ do
        compareP (Arc 0 1)
          (contrastRange (# crush 3) (# crush 0) (pure $ Map.singleton "n" $ (VN 0, VN 3)) $ s "bd" =| n "1 4")
          (s "bd" =| n "1 4" =| crush "3 0")

    describe "chew" $ do
      it "chews up patterns, with relative playback speeds" $
        compareP (Arc 0 1)
          (stripMetadata $ chew 4 "0 [1 ~] [2 3] ~" (s "a b c d"))
          (stripMetadata $ s "a [b ~] [c d] ~" # speed "1 2 2 2")

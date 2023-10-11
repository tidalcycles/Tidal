{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.BjorklundTest where

import           Sound.Tidal
import           Sound.Tidal.Bjorklund
import           Sound.Tidal.Mininotation
import           Sound.Tidal.Signal
import           Sound.Tidal.TestUtils    (compareP, comparePD, toEvent)
import           Sound.Tidal.Types
import           Test.Microspec           (MTestable (describe), Microspec,
                                           Testable (property), it, shouldBe,
                                           (===))


run :: Microspec ()
run = describe "Sound.Tidal.Bjorklund" $ do
  describe "euclid" $ do
    it "matches examples in Toussaint's paper" $ do
      sequence_ $ map (\(a,b) -> it b $ compareP (Span 0 1) a (parseBP_E b))
        ([(euclid 1 2, "t f")
          -- (euclid 1 3, "t f f"),
          -- (euclid 1 4, "t f f f"),
          -- (euclid 4 12, "t f f t f f t f f t f f"),
          -- (euclid 2 5, "t f t f f"),
          -- -- (euclid 3 4, "t f t t"), -- Toussaint is wrong..
          -- (euclid 3 4, "t t t f"), -- correction
          -- (euclid 3 5, "t f t f t"),
          -- (euclid 3 7, "t f t f t f f"),
          -- (euclid 3 8, "t f f t f f t f"),
          -- (euclid 4 7, "t f t f t f t"),
          -- (euclid 4 9, "t f t f t f t f f"),
          -- (euclid 4 11, "t f f t f f t f f t f"),
          -- -- (euclid 5 6, "t f t t t t"), -- Toussaint is wrong..
          -- (euclid 5 6, "t t t t t f"),  -- correction
          -- (euclid 5 7, "t f t t f t t"),
          -- (euclid 5 8, "t f t t f t t f"),
          -- (euclid 5 9, "t f t f t f t f t"),
          -- (euclid 5 11, "t f t f t f t f t f f"),
          -- (euclid 5 12, "t f f t f t f f t f t f"),
          -- -- (euclid 5 16, "t f f t f f t f f t f f t f f f f"),  -- Toussaint is wrong..
          -- (euclid 5 16, "t f f t f f t f f t f f t f f f"), -- correction
          -- -- (euclid 7 8, "t f t t t t t t"), -- Toussaint is wrong..
          -- (euclid 7 8, "t t t t t t t f"), -- Correction
          -- (euclid 7 12, "t f t t f t f t t f t f"),
          -- (euclid 7 16, "t f f t f t f t f f t f t f t f"),
          -- (euclid 9 16, "t f t t f t f t f t t f t f t f"),
          -- (euclid 11 24, "t f f t f t f t f t f t f f t f t f t f t f t f"),
          -- (euclid 13 24, "t f t t f t f t f t f t f t t f t f t f t f t f")
         ] :: [(Signal Bool, String)])
    -- it "can be called with a negative first value to give the inverse" $ do
    --   compareP (Span 0 1)
    --     (euclid (-3) 8 :: Signal Bool)
    --     (euclidInv 3 8)
    -- it "can be called with a negative first value to give the inverse (patternable)" $ do
    --   compareP (Span 0 1)
    --     (euclid (-3) 8)
    --     ("t(-3,8)")
  -- describe "euclidFull" $ do
  --   it "can match against silence" $ do
  --     compareP (Span 0 1)
  --       (euclidFull 3 8 "bd" silence)
  --       ("bd(3,8)" :: Signal String)

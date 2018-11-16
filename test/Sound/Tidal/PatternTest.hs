{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.PatternTest where

import TestUtils
import Test.Microspec

import Prelude hiding ((<*), (*>))

import Data.Ratio

import Sound.Tidal.Core
import Sound.Tidal.Pattern
import Sound.Tidal.Control

import qualified Data.Map.Strict as Map

run :: Microspec ()
run =
  describe "Sound.Tidal.Pattern" $ do
    describe "eventWhole" $ do
      it "returns the first element of a tuple inside the first element of a tuple" $ do
        property $ (1,2) === eventWhole (((1,2),(3,4)), 5 :: Int)

    describe "eventPart" $ do
      it "returns the second element of a tuple inside the first element of a tuple" $ do
        property $ (3,4) === eventPart (((1,2),(3,4)), 5 :: Int)

    describe "pure" $ do
      it "fills a whole cycle" $ do
        property $ queryArc (pure 0) (0,1) === [(((0,1),(0,1)),0 :: Int)]
      it "returns the part of an pure that you ask for, preserving the whole" $ do
        property $ queryArc (pure 0) (0.25,0.75) === [(((0,1),(0.25,0.75)),0 :: Int)]
      it "gives correct fragments when you go over cycle boundaries" $ do
        property $ queryArc (pure 0) (0.25,1.25) === [(((0,1),(0.25,1)),0 :: Int),
                                                      (((1,2),(1,1.25)),0)
                                                     ]
      it "works with zero-length queries" $ do
        it "0" $
          queryArc (pure "a") (0,0)
            `shouldBe` [(((0,1), (0,0)), "a")]
        it "1/3" $
          queryArc (pure "a") (1%3,1%3)
            `shouldBe` [(((0,1), (1%3,1%3)), "a")]

    describe "_fastGap" $ do
      it "copes with cross-cycle queries" $ do
        (queryArc(_fastGap 2 $ fastCat [pure "a", pure "b"]) (0.5,1.5))
          `shouldBe`
          [(((1 % 1,5 % 4),(1 % 1,5 % 4)),"a"),
           (((5 % 4,3 % 2),(5 % 4,3 % 2)),"b")
          ]
      it "does not return events outside of the query" $ do
        (queryArc(_fastGap 2 $ fastCat [pure "a", pure "b"]) (0.5,0.9))
          `shouldBe` []

    describe "<*>" $ do
      it "can apply a pattern of values to a pattern of values" $ do
        queryArc ((pure (+1)) <*> (pure 3)) (0,1) `shouldBe` [(((0,1), (0,1)), 4  :: Int)]
      it "can take structure from the left" $ do
        queryArc ((fastCat [pure (+1), pure (+2)]) <*> (pure 3)) (0,1) `shouldBe` [(((0,0.5), (0,0.5)), 4 :: Int),
                                                                                (((0.5,1), (0.5,1)), 5)
                                                                               ]
      it "can take structure from the right" $ do
        queryArc (pure (+1) <*> (fastCat [pure 7, pure 8])) (0,1) `shouldBe` [(((0,0.5), (0,0.5)), 8 :: Int),
                                                                           (((0.5,1), (0.5,1)), 9)
                                                                          ]
      it "can take structure from the both sides" $ do
        it "one" $
          queryArc ((fastCat [pure (+1), pure (+2)]) <*> (fastCat [pure 7, pure 8])) (0,1)
          `shouldBe` [(((0,0.5), (0,0.5)), 8 :: Int),
                      (((0.5,1), (0.5,1)), 10)
                     ]
        it "two" $
          queryArc ((fastCat [pure (+1), pure (+2), pure (+3)]) <*> (fastCat [pure 7, pure 8])) (0,1)
          `shouldBe` [(((0 % 1,1 % 3),(0 % 1,1 % 3)),8 :: Int),
                      (((1 % 3,1 % 2),(1 % 3,1 % 2)),9),
                      (((1 % 2,2 % 3),(1 % 2,2 % 3)),10),
                      (((2 % 3,1 % 1),(2 % 3,1 % 1)),11)
                     ]
      it "obeys pure id <*> v = v" $ do
        let v = (fastCat [fastCat [pure 7, pure 8], pure 9]) :: Pattern Int
        queryArc ((pure id <*> v)) (0,5) `shouldBe` queryArc v (0,5)

      it "obeys pure f <*> pure x = pure (f x)" $ do
        let f = (+3)
            x = 7 :: Int
        queryArc (pure f <*> pure x) (0,5) `shouldBe` queryArc (pure (f x)) (0,5)

      it "obeys u <*> pure y = pure ($ y) <*> u" $ do
        let u = fastCat [pure (+7), pure (+8)]
            y = 6 :: Int
        queryArc (u <*> pure y) (0,5) `shouldBe` queryArc (pure ($ y) <*> u) (0,5)

      it "obeys pure (.) <*> u <*> v <*> w = u <*> (v <*> w)" $ do
        let u = (fastCat [pure (+7), pure (+8)]) :: Pattern (Int -> Int)
            v = fastCat [pure (+3), pure (+4), pure (+5)]
            w = fastCat [pure 1, pure 2]
        queryArc (pure (.) <*> u <*> v <*> w) (0,5) `shouldBe` queryArc (u <*> (v <*> w)) (0,5)

    describe "<*" $ do
      it "can apply a pattern of values to a pattern of functions" $ do
        queryArc ((pure (+1)) <* (pure 3)) (0,1) `shouldBe` [(((0,1), (0,1)), 4  :: Int)]
      it "doesn't take structure from the right" $ do
        queryArc (pure (+1) <* (fastCat [pure 7, pure 8])) (0,1)
          `shouldBe` [(((0,1), (0,1)), 8 :: Int)]

    describe "*>" $ do
      it "can apply a pattern of values to a pattern of functions" $ do
        it "works within cycles" $ queryArc ((pure (+1)) *> (pure 3)) (0,1) `shouldBe` [(((0,1), (0,1)), 4  :: Int)]
        it "works across cycles" $ queryArc ((pure (+1)) *> (slow 2 $ pure 3)) (0,1) `shouldBe` [(((0,2), (0,1)), 4  :: Int)]
      it "doesn't take structure from the left" $ do
        queryArc (pure (+1) *> (fastCat [pure 7, pure 8])) (0,1)
          `shouldBe` [(((0,0.5), (0,0.5)), 8 :: Int),
                      (((0.5,1), (0.5,1)), 9 :: Int)
                     ]

    describe "arcCycles" $ do
     it "leaves a unit cycle intact" $ do
       it "(0,1)" $ arcCycles (0,1) `shouldBe` [(0,1)]
       it "(3,4)" $ arcCycles (3,4) `shouldBe` [(3,4)]
     it "splits a cycle at cycle boundaries" $ do
       it "(0,1.1)" $ arcCycles (0,1.1) `shouldBe` [(0,1),(1,1.1)]
       it "(1,2,1)" $ arcCycles (1,2.1) `shouldBe` [(1,2),(2,2.1)]
       it "(3 + (1%3),5.1)" $
          arcCycles (3 + (1%3),5.1) `shouldBe` [(3+(1%3),4),(4,5),(5,5.1)]

    describe "unwrap" $ do
      it "preserves inner structure" $ do
        it "one" $
          (queryArc (unwrap $ pure (fastCat [pure "a", pure "b"])) (0,1))
          `shouldBe` (queryArc (fastCat [pure "a", pure "b"]) (0,1))
        it "two" $
          (queryArc (unwrap $ pure (fastCat [pure "a", pure "b", fastCat [pure "c", pure "d"]])) (0,1))
          `shouldBe` (queryArc (fastCat [pure "a", pure "b", fastCat [pure "c", pure "d"]]) (0,1))
      it "preserves outer structure" $ do
        it "one" $
          (queryArc (unwrap $ fastCat [pure $ pure "a", pure $ pure "b"]) (0,1))
          `shouldBe` (queryArc (fastCat [pure "a", pure "b"]) (0,1))
        it "two" $
          (queryArc (unwrap $ fastCat [pure $ pure "a", pure $ pure "b", fastCat [pure $ pure "c", pure $ pure "d"]]) (0,1))
          `shouldBe` (queryArc (fastCat [pure "a", pure "b", fastCat [pure "c", pure "d"]]) (0,1))
      it "gives events whole/part timespans that are an intersection of that of inner and outer events" $ do
        let a = fastCat [pure "a", pure "b"]
            b = fastCat [pure "c", pure "d", pure "e"]
            pp = fastCat [pure a, pure b]
        queryArc (unwrap pp) (0,1)
          `shouldBe` [(((0 % 1,1 % 2),(0 % 1,1 % 2)),"a"),
                      (((1 % 2,2 % 3),(1 % 2,2 % 3)),"d"),
                      (((2 % 3,1 % 1),(2 % 3,1 % 1)),"e")
                     ]

    describe "unwrapSqueeze" $ do
      it "compresses cycles to fit outer 'whole' timearc of event" $ do
        let a = fastCat [pure "a", pure "b"]
            b = fastCat [pure "c", pure "d", pure "e"]
            pp = fastCat [pure a, pure b]
        queryArc (unwrapSqueeze pp) (0,1)
          `shouldBe` [(((0 % 1,1 % 4),(0 % 1,1 % 4)),"a"),
                      (((1 % 4,1 % 2),(1 % 4,1 % 2)),"b"),
                      (((1 % 2,2 % 3),(1 % 2,2 % 3)),"c"),
                      (((2 % 3,5 % 6),(2 % 3,5 % 6)),"d"),
                      (((5 % 6,1 % 1),(5 % 6,1 % 1)),"e")
                     ]

    describe ">>=" $ do
      it "can apply functions to patterns" $ do
       let p = fastCat [pure 7, pure 8] :: Pattern Int
           p' = do x <- p
                   return $ x + 1
       (queryArc p' (0,1)) `shouldBe` (queryArc ((+1) <$> p) (0,1))

      it "can add two patterns together" $ do
       let p1 = fastCat [pure 7, pure 8, pure 9] :: Pattern Int
           p2 = fastCat [pure 4, fastCat [pure 5, pure 6]]
           p' = do x <- p1
                   y <- p2
                   return $ x + y
       compareP (0,1) p' ((+) <$> p1 <*> p2)

      it "conforms to (return v) >>= f = f v" $ do
       let f x = pure $ x + 10
           v = 5 :: Int
       compareP (0,5) ((return v) >>= f) (f v)
      it "conforms to m >>= return ≡ m" $ do
       let m = fastCat [pure "a", fastCat [pure "b", pure "c"]]
       compareP (0,1) (m >>= return) m
     --    it "conforms to (m >>= f) >>= g ≡ m >>= ( \x -> (f x >>= g) )" $ do
     --      let m = fastCat [pure "a", fastCat [pure "b", pure "c"]]

    describe "rotR" $ do
      it "works over two cycles" $
       property $ comparePD (0,2) (0.25 ~> pure "a") (0.25 `rotR` pure "a")
      it "works over one cycle" $
       property $ compareP (0,1) (0.25 ~> pure "a") (0.25 `rotR` pure "a")
      it "works with zero width queries" $
       property $ compareP (0,0) (0.25 ~> pure "a") (0.25 `rotR` pure "a")

    describe "comparePD" $ do
      it "allows split events to be compared" $
       property $ comparePD (0,2)
         (splitQueries $ _slow 2 $ pure "a")
         (_slow 2 $ pure "a")

    describe "controlI" $ do
      it "can retrieve values from state" $
       (query (pure 3 + cF_ "hello") $ State (0,1) (Map.singleton "hello" (VF 0.5)))
       `shouldBe` [(((0 % 1,1 % 1),(0 % 1,1 % 1)),3.5)]

    -- pending "Sound.Tidal.Pattern.eventL" $ do
    --  it "succeeds if the first event 'whole' is shorter" $ do
    --    property $ eventL (((0,0),(0,1)),"x") (((0,0),(0,1.1)),"x")
    --  it "fails if the events are the same length" $ do
    --    property $ not $ eventL (((0,0),(0,1)),"x") (((0,0),(0,1)),"x")
    --  it "fails if the second event is shorter" $ do
    --    property $ not $ eventL (((0,0),(0,1)),"x") (((0,0),(0,0.5)),"x")

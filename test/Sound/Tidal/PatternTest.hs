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
        property $ (Arc 1 2) === eventWhole (Event (Arc 1 2) (Arc 3 4) (5 :: Int))

    describe "eventPart" $ do
      it "returns the second element of a tuple inside the first element of a tuple" $ do
        property $ (Arc 3 4) === eventPart (Event (Arc 1 2) (Arc 3 4) ( 5 :: Int))

    describe "pure" $ do
      it "fills a whole cycle" $ do
        property $ queryArc (pure 0) (Arc 0 1) === [(Event (Arc 0 1) (Arc 0 1) (0 :: Int))]
      it "returns the part of an pure that you ask for, preserving the whole" $ do
        property $ queryArc (pure 0) (Arc 0.25 0.75) === [(Event (Arc 0 1) (Arc 0.25 0.75) (0 :: Int))]
      it "gives correct fragments when you go over cycle boundaries" $ do
        property $ queryArc (pure 0) (Arc 0.25 1.25) ===
          [ (Event (Arc 0 1) (Arc 0.25 1) (0 :: Int)),
            (Event (Arc 1 2) (Arc 1 1.25) 0)
          ]
      it "works with zero-length queries" $ do
        it "0" $
          queryArc (pure "a") (Arc 0 0)
            `shouldBe` fmap toEvent [(((0,1), (0,0)), "a" :: String)]
        it "1/3" $
          queryArc (pure "a") (Arc (1%3) (1%3))
            `shouldBe` fmap toEvent [(((0,1), (1%3,1%3)), "a" :: String)]

    describe "_fastGap" $ do
      it "copes with cross-cycle queries" $ do
        (queryArc(_fastGap 2 $ fastCat [pure "a", pure "b"]) (Arc 0.5 1.5))
          `shouldBe`
          [(Event (Arc (1 % 1) (5 % 4)) (Arc (1 % 1) (5 % 4)) ("a" :: String)),
           (Event (Arc (5 % 4) (3 % 2)) (Arc (5 % 4) (3 % 2)) "b")
          ]
      it "does not return events outside of the query" $ do
        (queryArc(_fastGap 2 $ fastCat [pure "a", pure ("b" :: String)]) (Arc 0.5 0.9))
          `shouldBe` []

    describe "<*>" $ do
      it "can apply a pattern of values to a pattern of values" $ do
        queryArc ((pure (+1)) <*> (pure 3)) (Arc 0 1) `shouldBe` fmap toEvent [(((0,1), (0,1)), 4  :: Int)]
      it "can take structure from the left" $ do
        queryArc ((fastCat [pure (+1), pure (+2)]) <*> (pure 3)) (Arc 0 1) `shouldBe` fmap toEvent
          [(((0,0.5), (0,0.5)), 4 :: Int),
            (((0.5,1), (0.5,1)), 5)
          ]
      it "can take structure from the right" $ do
        queryArc (pure (+1) <*> (fastCat [pure 7, pure 8])) (Arc 0 1) `shouldBe` fmap toEvent
          [(((0,0.5), (0,0.5)), 8 :: Int),
            (((0.5,1), (0.5,1)), 9)
          ]
      it "can take structure from the both sides" $ do
        it "one" $
          queryArc ((fastCat [pure (+1), pure (+2)]) <*> (fastCat [pure 7, pure 8])) (Arc 0 1)
          `shouldBe` fmap toEvent
          [(((0,0.5), (0,0.5)), 8 :: Int),
            (((0.5,1), (0.5,1)), 10)
          ]
        it "two" $
          queryArc ((fastCat [pure (+1), pure (+2), pure (+3)]) <*> (fastCat [pure 7, pure 8])) (Arc 0 1)
          `shouldBe` fmap toEvent
          [ (((0%1, 1%3), (0%1, 1%3)), 8 :: Int),
            (((1%3, 1%2), (1%3, 1%2)), 9),
            (((1%2, 2%3), (1%2, 2%3)), 10),
            (((2%3, 1%1), (2%3, 1%1)), 11)
          ]
      it "obeys pure id <*> v = v" $ do
        let v = (fastCat [fastCat [pure 7, pure 8], pure 9]) :: Pattern Int
        queryArc ((pure id <*> v)) (Arc 0 5) `shouldBe` queryArc v (Arc 0 5)

      it "obeys pure f <*> pure x = pure (f x)" $ do
        let f = (+3)
            x = 7 :: Int
        queryArc (pure f <*> pure x) (Arc 0 5) `shouldBe` queryArc (pure (f x)) (Arc 0 5)

      it "obeys u <*> pure y = pure ($ y) <*> u" $ do
        let u = fastCat [pure (+7), pure (+8)]
            y = 6 :: Int
        queryArc (u <*> pure y) (Arc 0 5) `shouldBe` queryArc (pure ($ y) <*> u) (Arc 0 5)

      it "obeys pure (.) <*> u <*> v <*> w = u <*> (v <*> w)" $ do
        let u = (fastCat [pure (+7), pure (+8)]) :: Pattern (Int -> Int)
            v = fastCat [pure (+3), pure (+4), pure (+5)]
            w = fastCat [pure 1, pure 2]
        queryArc (pure (.) <*> u <*> v <*> w) (Arc 0 5) `shouldBe` queryArc (u <*> (v <*> w)) (Arc 0 5)

    describe "<*" $ do
      it "can apply a pattern of values to a pattern of functions" $ do
        queryArc ((pure (+1)) <* (pure 3)) (Arc 0 1) `shouldBe` fmap toEvent
          [(((0,1), (0,1)), 4  :: Int)]
      it "doesn't take structure from the right" $ do
        queryArc (pure (+1) <* (fastCat [pure 7, pure 8])) (Arc 0 1)
          `shouldBe` fmap toEvent [(((0,1), (0,1)), 8 :: Int)]

    describe "*>" $ do
      it "can apply a pattern of values to a pattern of functions" $ do
        it "works within cycles" $ queryArc ((pure (+1)) *> (pure 3)) (Arc 0 1) `shouldBe` fmap toEvent [(((0,1), (0,1)), 4  :: Int)]
        it "works across cycles" $ queryArc ((pure (+1)) *> (slow 2 $ pure 3)) (Arc 0 1) `shouldBe` fmap toEvent [(((0,2), (0,1)), 4  :: Int)]
      it "doesn't take structure from the left" $ do
        queryArc (pure (+1) *> (fastCat [pure 7, pure 8])) (Arc 0 1)
          `shouldBe` fmap toEvent
          [(((0,0.5), (0,0.5)), 8 :: Int),
            (((0.5,1), (0.5,1)), 9 :: Int)
          ]

    describe "arcCycles" $ do
     it "leaves a unit cycle intact" $ do
       it "(0,1)" $ arcCycles (Arc 0 1) `shouldBe` [(Arc 0 1)]
       it "(3,4)" $ arcCycles (Arc 3 4) `shouldBe` [(Arc 3 4)]
     it "splits a cycle at cycle boundaries" $ do
       it "(0,1.1)" $ arcCycles (Arc 0 1.1) `shouldBe` [(Arc 0 1),(Arc 1 1.1)]
       it "(1,2,1)" $ arcCycles (Arc 1 2.1) `shouldBe` [(Arc 1 2),(Arc 2 2.1)]
       it "(3 + (1%3),5.1)" $
          arcCycles (Arc (3 + (1%3)) 5.1) `shouldBe` [(Arc (3+(1%3)) 4),(Arc 4 5),(Arc 5 5.1)]

    describe "unwrap" $ do
      it "preserves inner structure" $ do
        it "one" $
          (queryArc (unwrap $ pure (fastCat [pure "a", pure ("b" :: String)])) (Arc 0 1))
          `shouldBe` (queryArc (fastCat [pure "a", pure "b"]) (Arc 0 1))
        it "two" $
          (queryArc (unwrap $ pure (fastCat [pure "a", pure "b", fastCat [pure "c", pure ("d" :: String)]])) (Arc 0 1))
          `shouldBe` (queryArc (fastCat [pure "a", pure "b", fastCat [pure "c", pure "d"]]) (Arc 0 1))
      it "preserves outer structure" $ do
        it "one" $
          (queryArc (unwrap $ fastCat [pure $ pure "a", pure $ pure ("b" :: String)]) (Arc 0 1))
          `shouldBe` (queryArc (fastCat [pure "a", pure "b"]) (Arc 0 1))
        it "two" $
          (queryArc (unwrap $ fastCat [pure $ pure "a", pure $ pure "b", fastCat [pure $ pure "c", pure $ pure ("d" :: String)]]) (Arc 0 1))
          `shouldBe` (queryArc (fastCat [pure "a", pure "b", fastCat [pure "c", pure "d"]]) (Arc 0 1))
      it "gives events whole/part timespans that are an intersection of that of inner and outer events" $ do
        let a = fastCat [pure "a", pure "b"]
            b = fastCat [pure "c", pure "d", pure "e"]
            pp = fastCat [pure a, pure b]
        queryArc (unwrap pp) (Arc 0 1)
          `shouldBe` [(Event (Arc (0 % 1) (1 % 2)) (Arc (0 % 1) (1 % 2)) ("a" :: String)),
                      (Event (Arc (1 % 2) (2 % 3)) (Arc (1 % 2) (2 % 3)) "d"),
                      (Event (Arc (2 % 3) (1 % 1)) (Arc (2 % 3) (1 % 1)) "e")
                     ]

    describe "unwrapSqueeze" $ do
      it "compresses cycles to fit outer 'whole' timearc of event" $ do
        let a = fastCat [pure "a", pure "b"]
            b = fastCat [pure "c", pure "d", pure "e"]
            pp = fastCat [pure a, pure b]
        queryArc (unwrapSqueeze pp) (Arc 0 1)
          `shouldBe` [(Event (Arc (0 % 1) (1 % 4)) (Arc (0 % 1) (1 % 4)) ("a" :: String)),
                      (Event (Arc (1 % 4) (1 % 2)) (Arc (1 % 4) (1 % 2)) "b"),
                      (Event (Arc (1 % 2) (2 % 3)) (Arc (1 % 2) (2 % 3)) "c"),
                      (Event (Arc (2 % 3) (5 % 6)) (Arc (2 % 3) (5 % 6)) "d"),
                      (Event (Arc (5 % 6) (1 % 1)) (Arc (5 % 6) (1 % 1)) "e")
                     ]

    describe ">>=" $ do
      it "can apply functions to patterns" $ do
       let p = fastCat [pure 7, pure 8] :: Pattern Int
           p' = do x <- p
                   return $ x + 1
       (queryArc p' (Arc 0 1)) `shouldBe` (queryArc ((+1) <$> p) (Arc 0 1))

      it "can add two patterns together" $ do
       let p1 = fastCat [pure 7, pure 8, pure 9] :: Pattern Int
           p2 = fastCat [pure 4, fastCat [pure 5, pure 6]]
           p' = do x <- p1
                   y <- p2
                   return $ x + y
       compareP (Arc 0 1) p' ((+) <$> p1 <*> p2)

      it "conforms to (return v) >>= f = f v" $ do
       let f x = pure $ x + 10
           v = 5 :: Int
       compareP (Arc 0 5) ((return v) >>= f) (f v)
      it "conforms to m >>= return ≡ m" $ do
       let m = fastCat [pure "a", fastCat [pure "b", pure ("c" :: String)]]
       compareP (Arc 0 1) (m >>= return) m
     --    it "conforms to (m >>= f) >>= g ≡ m >>= ( \x -> (f x >>= g) )" $ do
     --      let m = fastCat [pure "a", fastCat [pure "b", pure "c"]]

    describe "rotR" $ do
      it "works over two cycles" $
       property $ comparePD (Arc 0 2) (0.25 ~> pure "a") (0.25 `rotR` pure ("a" :: String))
      it "works over one cycle" $
       property $ compareP (Arc 0 1) (0.25 ~> pure "a") (0.25 `rotR` pure ("a" :: String))
      it "works with zero width queries" $
       property $ compareP (Arc 0 0) (0.25 ~> pure "a") (0.25 `rotR` pure ("a" :: String))

    describe "comparePD" $ do
      it "allows split events to be compared" $
       property $ comparePD (Arc 0 2)
         (splitQueries $ _slow 2 $ pure ("a" :: String))
         (_slow 2 $ pure "a")

    describe "controlI" $ do
      it "can retrieve values from state" $
       (query (pure 3 + cF_ "hello") $ State (Arc 0 1) (Map.singleton "hello" (VF 0.5)))
       `shouldBe` [(Event (Arc (0 % 1) (1 % 1)) (Arc (0 % 1) (1 % 1)) 3.5)]

    describe "eventWholeOnset" $ do 
      it "retrieve first element of a tuple, inside first element of a tuple, inside the first of another" $ do 
        property $ 1 === eventWholeOnset (Event (Arc 1 2) (Arc 3 4) (5 :: Int))

    describe "eventValue" $ do
      it "retrieve the second value from a tuple" $ do 
        property $ 5 === eventValue (Event (Arc 1 2) (Arc 3 4) (5 :: Int))

    describe "eventHasOnset" $ do 
      it "return True when the start values of the two arcs in an event are equal" $ do 
        let ev = (Event (Arc 1 2) (Arc 1 3) (4 :: Int)) 
        property $ True === eventHasOnset ev 
      it "return False when the start values of the two arcs in an event are not equal" $ do 
        let ev = (Event (Arc 1 2) (Arc 3 4) (5 :: Int)) 
        property $ False === eventHasOnset ev

    describe "sam" $ do 
      it "start of a cycle, round down time value" $ do
        let res = sam (3.4 :: Time)
        property $ (3.0 :: Time) === res

    describe "nextSam" $ do 
      it "the end point of the current cycle, and start of the next" $ do 
        let res = nextSam (3.4 :: Time) 
        property $ (4.0 :: Time) === res 

    describe "arcCycles" $ do 
      it "if start time is greater than end time return empty list" $ do 
        let res = arcCycles (Arc 2.3 2.1)
        property $ [] === res 
      it "if start time is equal to end time return empty list" $ do 
        let res = arcCycles (Arc 3 3)
        property $ [] === res
      it "if start and end time round down to same value return list of (start, end)" $ do
        let res = arcCycles (Arc 2.1 2.3) 
        property $ [(Arc 2.1 2.3)] === res
      it "if start time is less than end time and start time does not round down to same value as end time" $ do
        let res = arcCycles (Arc 2.1 3.3)
        property $ [(Arc 2.1 3.0), (Arc 3.0 3.3)] === res
    
    -- pending "Sound.Tidal.Pattern.eventL" $ do
    --  it "succeeds if the first event 'whole' is shorter" $ do
    --    property $ eventL (Event (Arc 0,0),(0,1)),"x") (((0 0) (Arc 0 1.1)) "x")
    --  it "fails if the events are the same length" $ do
    --    property $ not $ eventL (Event (Arc 0,0),(0,1)),"x") (((0 0) (Arc 0 1)) "x")
    --  it "fails if the second event is shorter" $ do
    --    property $ not $ eventL (Event (Arc 0,0),(0,1)),"x") (((0 0) (Arc 0 0.5)) "x")

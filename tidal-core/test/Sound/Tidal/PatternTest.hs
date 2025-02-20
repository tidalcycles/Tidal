{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.PatternTest where

import qualified Data.Map.Strict as Map
import Data.Ratio
import Sound.Tidal.Core
import Sound.Tidal.Pattern
import Sound.Tidal.UI
import Test.Hspec
import TestUtils
import Prelude hiding ((*>), (<*))

run :: Spec
run =
  describe "Sound.Tidal.Pattern" $ do
    describe "Arc" $ do
      it "Arc is a Functor: Apply a given function to the start and end values of an Arc" $ do
        let res = fmap (+ 1) (Arc 3 5)
        res `shouldBe` ((Arc 4 6) :: Arc)

    describe "whole" $ do
      it "returns the whole Arc in an Event" $ do
        (Just $ Arc 1 2) `shouldBe` whole (Event (Context []) (Just $ Arc 1 2) (Arc 3 4) 5 :: Event Int)

    describe "part" $ do
      it "returns the part Arc in an Event" $ do
        (Arc 3 4) `shouldBe` part (Event (Context []) (Just $ Arc 1 2) (Arc 3 4) 5 :: Event Int)

    describe "value" $ do
      it "returns the event value in an Event" $ do
        5 `shouldBe` value (Event (Context []) (Just $ Arc (1 :: Rational) 2) (Arc 3 4) (5 :: Int))

    describe "wholeStart" $ do
      it "retrieve the onset of an event: the start of the whole Arc" $ do
        1 `shouldBe` wholeStart (Event (Context []) (Just $ Arc 1 2) (Arc 3 4) (5 :: Int))

    describe "eventHasOnset" $ do
      it "return True when the start values of the two arcs in an event are equal" $ do
        let ev = (Event (Context []) (Just $ Arc 1 2) (Arc 1 3) (4 :: Int))
        True `shouldBe` eventHasOnset ev
      it "return False when the start values of the two arcs in an event are not equal" $ do
        let ev = (Event (Context []) (Just $ Arc 1 2) (Arc 3 4) (5 :: Int))
        False `shouldBe` eventHasOnset ev

    describe "pure" $ do
      it "fills a whole cycle" $ do
        queryArc (pure 0) (Arc 0 1) `shouldBe` [(Event (Context []) (Just $ Arc 0 1) (Arc 0 1) (0 :: Int))]
      it "returns the part of an pure that you ask for, preserving the whole" $ do
        queryArc (pure 0) (Arc 0.25 0.75) `shouldBe` [(Event (Context []) (Just $ Arc 0 1) (Arc 0.25 0.75) (0 :: Int))]
      it "gives correct fragments when you go over cycle boundaries" $ do
        queryArc (pure 0) (Arc 0.25 1.25)
          `shouldBe` [ (Event (Context []) (Just $ Arc 0 1) (Arc 0.25 1) (0 :: Int)),
            (Event (Context []) (Just $ Arc 1 2) (Arc 1 1.25) 0)]
      it "works with zero-length queries" $ do
        queryArc (pure "a") (Arc 0 0)
          `shouldBe` fmap toEvent [(((0, 1), (0, 0)), "a" :: String)]
        queryArc (pure "a") (Arc (1 % 3) (1 % 3))
          `shouldBe` fmap toEvent [(((0, 1), (1 % 3, 1 % 3)), "a" :: String)]

    describe "_fastGap" $ do
      it "copes with cross-cycle queries" $ do
        (queryArc (_fastGap 2 $ fastCat [pure "a", pure "b"]) (Arc 0.5 1.5))
          `shouldBe` [ (Event (Context []) (Just $ Arc (1 % 1) (5 % 4)) (Arc (1 % 1) (5 % 4)) ("a" :: String)),
                       (Event (Context []) (Just $ Arc (5 % 4) (3 % 2)) (Arc (5 % 4) (3 % 2)) "b")
                     ]
      it "does not return events outside of the query" $ do
        (queryArc (_fastGap 2 $ fastCat [pure "a", pure ("b" :: String)]) (Arc 0.5 0.9))
          `shouldBe` []

    describe "<*>" $ do
      it "can apply a pattern of values to a pattern of values" $ do
        queryArc ((pure (+ 1)) <*> (pure 3)) (Arc 0 1) `shouldBe` fmap toEvent [(((0, 1), (0, 1)), 4 :: Int)]
      it "can take structure from the left" $ do
        queryArc ((fastCat [pure (+ 1), pure (+ 2)]) <*> (pure 3)) (Arc 0 1)
          `shouldBe` fmap
            toEvent
            [ (((0, 0.5), (0, 0.5)), 4 :: Int),
              (((0.5, 1), (0.5, 1)), 5)
            ]
      it "can take structure from the right" $ do
        queryArc (pure (+ 1) <*> (fastCat [pure 7, pure 8])) (Arc 0 1)
          `shouldBe` fmap
            toEvent
            [ (((0, 0.5), (0, 0.5)), 8 :: Int),
              (((0.5, 1), (0.5, 1)), 9)
            ]
      it "can take structure from the both sides" $ do
        queryArc ((fastCat [pure (+ 1), pure (+ 2)]) <*> (fastCat [pure 7, pure 8])) (Arc 0 1)
          `shouldBe` fmap
            toEvent
            [ (((0, 0.5), (0, 0.5)), 8 :: Int),
              (((0.5, 1), (0.5, 1)), 10)
            ]
        queryArc ((fastCat [pure (+ 1), pure (+ 2), pure (+ 3)]) <*> (fastCat [pure 7, pure 8])) (Arc 0 1)
          `shouldBe` fmap
            toEvent
            [ (((0 % 1, 1 % 3), (0 % 1, 1 % 3)), 8 :: Int),
              (((1 % 3, 1 % 2), (1 % 3, 1 % 2)), 9),
              (((1 % 2, 2 % 3), (1 % 2, 2 % 3)), 10),
              (((2 % 3, 1 % 1), (2 % 3, 1 % 1)), 11)
            ]
      it "obeys pure id <*> v = v" $ do
        let v = (fastCat [fastCat [pure 7, pure 8], pure 9]) :: Pattern Int
        queryArc ((pure id <*> v)) (Arc 0 5) `shouldBe` queryArc v (Arc 0 5)

      it "obeys pure f <*> pure x = pure (f x)" $ do
        let f = (+ 3)
            x = 7 :: Int
        queryArc (pure f <*> pure x) (Arc 0 5) `shouldBe` queryArc (pure (f x)) (Arc 0 5)

      it "obeys u <*> pure y = pure ($ y) <*> u" $ do
        let u = fastCat [pure (+ 7), pure (+ 8)]
            y = 6 :: Int
        queryArc (u <*> pure y) (Arc 0 5) `shouldBe` queryArc (pure ($ y) <*> u) (Arc 0 5)

      it "obeys pure (.) <*> u <*> v <*> w = u <*> (v <*> w)" $ do
        let u = (fastCat [pure (+ 7), pure (+ 8)]) :: Pattern (Int -> Int)
            v = fastCat [pure (+ 3), pure (+ 4), pure (+ 5)]
            w = fastCat [pure 1, pure 2]
        queryArc (pure (.) <*> u <*> v <*> w) (Arc 0 5) `shouldBe` queryArc (u <*> (v <*> w)) (Arc 0 5)

    describe "<*" $ do
      it "can apply a pattern of values to a pattern of functions" $ do
        queryArc ((pure (+ 1)) <* (pure 3)) (Arc 0 1)
          `shouldBe` fmap
            toEvent
            [(((0, 1), (0, 1)), 4 :: Int)]
      it "doesn't take structure from the right" $ do
        queryArc (pure (+ 1) <* (fastCat [pure 7, pure 8])) (Arc 0 1)
          `shouldBe` fmap
            toEvent
            [ (((0, 1), (0, 0.5)), 8 :: Int),
              (((0, 1), (0.5, 1)), 9 :: Int)
            ]

    describe "*> can apply a pattern of values to a pattern of functions" $ do
      it "works within cycles" $ queryArc ((pure (+ 1)) *> (pure 3)) (Arc 0 1) `shouldBe` fmap toEvent [(((0, 1), (0, 1)), 4 :: Int)]
      it "works across cycles" $ queryArc ((pure (+ 1)) *> (slow 2 $ pure 3)) (Arc 0 1) `shouldBe` fmap toEvent [(((0, 2), (0, 1)), 4 :: Int)]
      it "doesn't take structure from the left" $ do
        queryArc (pure (+ 1) *> (fastCat [pure 7, pure 8])) (Arc 0 1)
          `shouldBe` fmap
            toEvent
            [ (((0, 0.5), (0, 0.5)), 8 :: Int),
              (((0.5, 1), (0.5, 1)), 9 :: Int)
            ]

    describe "arcCycles" $ do
      it "leaves a unit cycle intact" $ do
        arcCycles (Arc 0 1) `shouldBe` [(Arc 0 1)]
        arcCycles (Arc 3 4) `shouldBe` [(Arc 3 4)]
      it "splits a cycle at cycle boundaries" $ do
        arcCycles (Arc 0 1.1) `shouldBe` [(Arc 0 1), (Arc 1 1.1)]
        arcCycles (Arc 1 2.1) `shouldBe` [(Arc 1 2), (Arc 2 2.1)]
        arcCycles (Arc (3 + (1 % 3)) 5.1) `shouldBe` [(Arc (3 + (1 % 3)) 4), (Arc 4 5), (Arc 5 5.1)]

    describe "unwrap" $ do
      it "preserves inner structure" $ do
        (queryArc (unwrap $ pure (fastCat [pure "a", pure ("b" :: String)])) (Arc 0 1))
          `shouldBe` (queryArc (fastCat [pure "a", pure "b"]) (Arc 0 1))
        (queryArc (unwrap $ pure (fastCat [pure "a", pure "b", fastCat [pure "c", pure ("d" :: String)]])) (Arc 0 1))
          `shouldBe` (queryArc (fastCat [pure "a", pure "b", fastCat [pure "c", pure "d"]]) (Arc 0 1))
      it "preserves outer structure" $ do
        (queryArc (unwrap $ fastCat [pure $ pure "a", pure $ pure ("b" :: String)]) (Arc 0 1))
          `shouldBe` (queryArc (fastCat [pure "a", pure "b"]) (Arc 0 1))
        (queryArc (unwrap $ fastCat [pure $ pure "a", pure $ pure "b", fastCat [pure $ pure "c", pure $ pure ("d" :: String)]]) (Arc 0 1))
          `shouldBe` (queryArc (fastCat [pure "a", pure "b", fastCat [pure "c", pure "d"]]) (Arc 0 1))
      it "gives events whole/part timespans that are an intersection of that of inner and outer events" $ do
        let a = fastCat [pure "a", pure "b"]
            b = fastCat [pure "c", pure "d", pure "e"]
            pp = fastCat [pure a, pure b]
        queryArc (unwrap pp) (Arc 0 1)
          `shouldBe` [ (Event (Context []) (Just $ Arc (0 % 1) (1 % 2)) (Arc (0 % 1) (1 % 2)) ("a" :: String)),
                       (Event (Context []) (Just $ Arc (1 % 2) (2 % 3)) (Arc (1 % 2) (2 % 3)) "d"),
                       (Event (Context []) (Just $ Arc (2 % 3) (1 % 1)) (Arc (2 % 3) (1 % 1)) "e")
                     ]

    describe "squeezeJoin" $ do
      it "compresses cycles to fit outer 'whole' timearc of event" $ do
        let a = fastCat [pure "a", pure "b"]
            b = fastCat [pure "c", pure "d", pure "e"]
            pp = fastCat [pure a, pure b]
        queryArc (squeezeJoin pp) (Arc 0 1)
          `shouldBe` [ (Event (Context []) (Just $ Arc (0 % 1) (1 % 4)) (Arc (0 % 1) (1 % 4)) ("a" :: String)),
                       (Event (Context []) (Just $ Arc (1 % 4) (1 % 2)) (Arc (1 % 4) (1 % 2)) "b"),
                       (Event (Context []) (Just $ Arc (1 % 2) (2 % 3)) (Arc (1 % 2) (2 % 3)) "c"),
                       (Event (Context []) (Just $ Arc (2 % 3) (5 % 6)) (Arc (2 % 3) (5 % 6)) "d"),
                       (Event (Context []) (Just $ Arc (5 % 6) (1 % 1)) (Arc (5 % 6) (1 % 1)) "e")
                     ]
      it "preserves cycle number of inner patterns" $ do
        (map value $ queryArc (squeezeJoin (pure $ struct "1" $ (sig $ id))) (Arc 3 4))
          `shouldBe` [3]

    describe ">>=" $ do
      it "can apply functions to patterns" $ do
        let p = fastCat [pure 7, pure 8] :: Pattern Int
            p' = do
              x <- p
              return $ x + 1
        (queryArc p' (Arc 0 1)) `shouldBe` (queryArc ((+ 1) <$> p) (Arc 0 1))

      it "can add two patterns together" $ do
        let p1 = fastCat [pure 7, pure 8, pure 9] :: Pattern Int
            p2 = fastCat [pure 4, fastCat [pure 5, pure 6]]
            p' = do
              x <- p1
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
        comparePD (Arc 0 2) (0.25 ~> pure "a") (0.25 `rotR` pure ("a" :: String))
      it "works over one cycle" $
        compareP (Arc 0 1) (0.25 ~> pure "a") (0.25 `rotR` pure ("a" :: String))
      it "works with zero width queries" $
        compareP (Arc 0 0) (0.25 ~> pure "a") (0.25 `rotR` pure ("a" :: String))

    describe "comparePD" $ do
      it "allows split events to be compared" $
        comparePD
          (Arc 0 2)
          (splitQueries $ _slow 2 $ pure ("a" :: String))
          (_slow 2 $ pure "a")

    describe "controlI" $ do
      it "can retrieve values from state" $
        (query (pure 3 + cF_ "hello") $ State (Arc 0 1) (Map.singleton "hello" (VF 0.5)))
          `shouldBe` [(Event (Context []) (Just $ Arc (0 % 1) (1 % 1)) (Arc (0 % 1) (1 % 1)) 3.5)]

    describe "wholeStart" $ do
      it "retrieve first element of a tuple, inside first element of a tuple, inside the first of another" $ do
         1 `shouldBe` wholeStart (Event (Context []) (Just $ Arc 1 2) (Arc 3 4) (5 :: Int))

    describe "wholeStop" $ do
      it "retrieve the end time from the first Arc in an Event" $ do
         2 `shouldBe` wholeStop (Event (Context []) (Just $ Arc 1 2) (Arc 3 4) (5 :: Int))

    describe "eventPartStart" $ do
      it "retrieve the start time of the second Arc in an Event" $ do
         3 `shouldBe` eventPartStart (Event (Context []) (Just $ Arc 1 2) (Arc 3 4) (5 :: Int))

    describe "eventPartStop" $ do
      it "retrieve the end time of the second Arc in an Event" $ do
         4 `shouldBe` eventPartStop (Event (Context []) (Just $ Arc 1 2) (Arc 3 4) (5 :: Int))

    describe "eventPart" $ do
      it "retrieve the second Arc in an Event" $ do
         Arc 3 4 `shouldBe` eventPart (Event (Context []) (Just $ Arc 1 2) (Arc 3 4) (5 :: Int))

    describe "eventValue" $ do
      it "retrieve the second value from a tuple" $ do
         5 `shouldBe` eventValue (Event (Context []) (Just $ Arc 1 2) (Arc 3 4) (5 :: Int))

    describe "eventHasOnset" $ do
      it "return True when the start values of the two arcs in an event are equal" $ do
        let ev = (Event (Context []) (Just $ Arc 1 2) (Arc 1 3) (4 :: Int))
        True `shouldBe` eventHasOnset ev
      it "return False when the start values of the two arcs in an event are not equal" $ do
        let ev = (Event (Context []) (Just $ Arc 1 2) (Arc 3 4) (5 :: Int))
        False `shouldBe` eventHasOnset ev

    describe "sam" $ do
      it "start of a cycle, round down time value" $ do
        let res = sam (3.4 :: Time)
        (3.0 :: Time) `shouldBe` res

    describe "nextSam" $ do
      it "the end point of the current cycle, and start of the next" $ do
        let res = nextSam (3.4 :: Time)
        (4.0 :: Time) `shouldBe` res

    describe "arcCycles" $ do
      it "if start time is greater than end time return empty list" $ do
        let res = arcCycles (Arc 2.3 2.1)
        [] `shouldBe` res
      it "if start time is equal to end time return empty list" $ do
        let res = arcCycles (Arc 3 3)
        [] `shouldBe` res
      it "if start and end time round down to same value return list of (start, end)" $ do
        let res = arcCycles (Arc 2.1 2.3)
        [(Arc 2.1 2.3)] `shouldBe` res
      it "if start time is less than end time and start time does not round down to same value as end time" $ do
        let res = arcCycles (Arc 2.1 3.3)
        [(Arc 2.1 3.0), (Arc 3.0 3.3)] `shouldBe` res

    describe "arcCyclesZW" $ do
      it "if start and end time are equal return list of (start, end)" $ do
        let res = arcCyclesZW (Arc 2.5 2.5)
        [(Arc 2.5 2.5)] `shouldBe` res
      it "if start and end time are not equal call arcCycles (start, end) with same rules as above" $ do
        let res = arcCyclesZW (Arc 2.3 2.1)
        [] `shouldBe` res
      it "if start time is less than end time" $ do
        let res = arcCyclesZW (Arc 2.1 2.3)
        [(Arc 2.1 2.3)] `shouldBe` res
      it "if start time is greater than end time" $ do
        let res = arcCyclesZW (Arc 2.1 3.3)
        [(Arc 2.1 3.0), (Arc 3.0 3.3)] `shouldBe` res

    describe "mapCycle" $ do
      it "Apply a function to the Arc values minus the start value rounded down (sam'), adding both results to sam' to obtain the new Arc value" $ do
        let res = mapCycle (* 2) (Arc 3.3 5)
        ((Arc 3.6 7.0) :: Arc) `shouldBe` res

    describe "toTime" $ do
      it "Convert a number of type Real to a Time value of type Rational, Int test" $ do
        let res = toTime (3 :: Int)
        (3 % 1 :: Time) `shouldBe` res
      it "Convert a number of type Double to a Time value of type Rational" $ do
        let res = toTime (3.2 :: Double)
        (3602879701896397 % 1125899906842624 :: Time) `shouldBe` res

    describe "cyclePos" $ do
      it "Subtract a Time value from its value rounded down (the start of the cycle)" $ do
        let res = cyclePos 2.6
        (0.6 :: Time) `shouldBe` res
      it "If no difference between a given Time and the start of the cycle" $ do
        let res = cyclePos 2
        (0.0 :: Time) `shouldBe` res

    describe "isIn" $ do
      it "Check given Time is inside a given Arc value, Time is greater than start and less than end Arc values" $ do
        let res = isIn (Arc 2.0 2.8) 2.5
        True `shouldBe` res
      it "Given Time is equal to the Arc start value" $ do
        let res = isIn (Arc 2.0 2.8) 2.0
        True `shouldBe` res
      it "Given Time is less than the Arc start value" $ do
        let res = isIn (Arc 2.0 2.8) 1.4
        False `shouldBe` res
      it "Given Time is greater than the Arc end value" $ do
        let res = isIn (Arc 2.0 2.8) 3.2
        False `shouldBe` res

    describe "onsetIn" $ do
      it "If the beginning of an Event is within a given Arc, same rules as 'isIn'" $ do
        let res = onsetIn (Arc 2.0 2.8) (Event (Context []) (Just $ Arc 2.2 2.7) (Arc 3.3 3.8) (5 :: Int))
        True `shouldBe` res
      it "Beginning of Event is equal to beggining of given Arc" $ do
        let res = onsetIn (Arc 2.0 2.8) (Event (Context []) (Just $ Arc 2.0 2.7) (Arc 3.3 3.8) (5 :: Int))
        True `shouldBe` res
      it "Beginning of an Event is less than the start of the Arc" $ do
        let res = onsetIn (Arc 2.0 2.8) (Event (Context []) (Just $ Arc 1.2 1.7) (Arc 3.3 3.8) (5 :: Int))
        False `shouldBe` res
      it "Start of Event is greater than the start of the given Arc" $ do
        let res = onsetIn (Arc 2.0 2.8) (Event (Context []) (Just $ Arc 3.1 3.5) (Arc 4.0 4.6) (5 :: Int))
        False `shouldBe` res

    describe "subArc" $ do
      it "Checks if an Arc is within another, returns Just (max $ (fst a1) (fst a2), min $ (snd a1) (snd a2)) if so, otherwise Nothing" $ do
        let res = subArc (Arc 2.1 2.4) (Arc 2.4 2.8)
        Nothing `shouldBe` res
      it "if max (fst arc1) (fst arc2) <= min (snd arc1) (snd arc2) return Just (max (fst arc1) (fst arc2), min...)" $ do
        let res = subArc (Arc 2 2.8) (Arc 2.4 2.9)
        Just (Arc 2.4 2.8) `shouldBe` res

    describe "timeToCycleArc" $ do
      it "given a Time value return the Arc in which it resides" $ do
        let res = timeToCycleArc 2.2
        (Arc 2.0 3.0) `shouldBe` res

    describe "cyclesInArc" $ do
      it "Return a list of cycles in a given arc, if start is greater than end return empty list" $ do
        let res = cyclesInArc (Arc 2.4 2.2)
        ([] :: [Int]) `shouldBe` res
      it "If start value of Arc is equal to end value return list with start value rounded down" $ do
        let res = cyclesInArc (Arc 2.4 2.4)
        ([2] :: [Int]) `shouldBe` res
      it "if start of Arc is less than end return list of start rounded down to end rounded up minus one" $ do
        let res = cyclesInArc (Arc 2.2 4.5)
        ([2, 3, 4] :: [Int]) `shouldBe` res

    describe "cycleArcsInArc" $ do
      it "generates a list of Arcs based on the cycles found within a given a Arc" $ do
        let res = cycleArcsInArc (Arc 2.2 4.5)
        [(Arc 2.0 3.0), (Arc 3.0 4.0), (Arc 4.0 5.0)] `shouldBe` res

    describe "isAdjacent" $ do
      it "if the given Events are adjacent parts of the same whole" $ do
        let res = isAdjacent (Event (Context []) (Just $ Arc 1 2) (Arc 3 4) 5) (Event (Context []) (Just $ Arc 1 2) (Arc 4 3) (5 :: Int))
        True `shouldBe` res
      it "if first Arc of of first Event is not equal to first Arc of second Event" $ do
        let res = isAdjacent (Event (Context []) (Just $ Arc 1 2) (Arc 3 4) 5) (Event (Context []) (Just $ Arc 7 8) (Arc 4 3) (5 :: Int))
        False `shouldBe` res
      it "if the value of the first Event does not equal the value of the second Event" $ do
        let res = isAdjacent (Event (Context []) (Just $ Arc 1 2) (Arc 3 4) 5) (Event (Context []) (Just $ Arc 1 2) (Arc 4 3) (6 :: Int))
        False `shouldBe` res
      it "second value of second Arc of first Event not equal to first value of second Arc in second Event..." $ do
        let res = isAdjacent (Event (Context []) (Just $ Arc 1 2) (Arc 3 4) 5) (Event (Context []) (Just $ Arc 1 2) (Arc 3 4) (5 :: Int))
        False `shouldBe` res

    describe "defragParts" $ do
      it "if empty list with no events return empty list" $ do
        let res = defragParts ([] :: [Event Int])
        [] `shouldBe` res
      it "if list consists of only one Event return it as is" $ do
        let res = defragParts [(Event (Context []) (Just $ Arc 1 2) (Arc 3 4) (5 :: Int))]
        [Event (Context []) (Just $ Arc 1 2) (Arc 3 4) (5 :: Int)] `shouldBe` res
      it "if list contains adjacent Events return list with Parts combined" $ do
        let res = defragParts [(Event (Context []) (Just $ Arc 1 2) (Arc 3 4) (5 :: Int)), (Event (Context []) (Just $ Arc 1 2) (Arc 4 3) (5 :: Int))]
        [(Event (Context []) (Just $ Arc 1 2) (Arc 3 4) 5)] `shouldBe` res
      it "if list contains more than one Event none of which are adjacent, return List as is" $ do
        let res = defragParts [(Event (Context []) (Just $ Arc 1 2) (Arc 3 4) 5), (Event (Context []) (Just $ Arc 7 8) (Arc 4 3) (5 :: Int))]
        [Event (Context []) (Just $ Arc 1 2) (Arc 3 4) 5, Event (Context []) (Just $ Arc 7 8) (Arc 4 3) (5 :: Int)] `shouldBe` res

    describe "sect" $ do
      it "take two Arcs and return - Arc (max of two starts) (min of two ends)" $ do
        let res = sect (Arc 2.2 3) (Arc 2 2.9)
        Arc 2.2 2.9 == res

    describe "hull" $ do
      it "take two Arcs anre return - Arc (min of two starts) (max of two ends)" $ do
        let res = hull (Arc 2.2 3) (Arc 2 2.9)
        Arc 2 3 == res

    describe "withResultArc" $ do
      it "apply given function to the Arcs" $ do
        let p = withResultArc (+ 5) (stripContext $ fast "1 2" "3 4" :: Pattern Int)
        let res = queryArc p (Arc 0 1)
        res `shouldBe` fmap toEvent [(((5, 11 % 2), (5, 11 % 2)), 3), (((11 % 2, 23 % 4), (11 % 2, 23 % 4)), 3), (((23 % 4, 6), (23 % 4, 6)), 4)]

    describe "applyFIS" $ do
      it "apply Float function when value of type VF" $ do
        let res = applyFIS (+ 1) (+ 1) (++ "1") (VF 1)
        (VF 2.0) `shouldBe` res
      it "apply Int function when value of type VI" $ do
        let res = applyFIS (+ 1) (+ 1) (++ "1") (VI 1)
        (VI 2) `shouldBe` res
      it "apply String function when value of type VS" $ do
        let res = applyFIS (+ 1) (+ 1) (++ "1") (VS "1")
        (VS "11") `shouldBe` res

    describe "fNum2" $ do
      it "apply Int function for two Int values" $ do
        let res = fNum2 (+) (+) (VI 2) (VI 3)
        (VI 5) `shouldBe` res
      it "apply float function when given two float values" $ do
        let res = fNum2 (+) (+) (VF 2) (VF 3)
        (VF 5.0) `shouldBe` res
      it "apply float function when one float and one int value given" $ do
        let res = fNum2 (+) (+) (VF 2) (VI 3)
        (VF 5.0) `shouldBe` res

    describe "getI" $ do
      it "get Just value when Int value is supplied" $ do
        let res = getI (VI 3)
        (Just 3) `shouldBe` res
      it "get floored value when float value is supplied" $ do
        let res = getI (VF 3.5)
        (Just 3) `shouldBe` res
      it "get if String value is supplied" $ do
        let res = getI (VS "3")
        Nothing `shouldBe` res

    describe "getF" $ do
      it "get Just value when Float value is supplied" $ do
        let res = getF (VF 3)
        (Just 3.0) `shouldBe` res
      it "get converted value if Int value is supplied" $ do
        let res = getF (VI 3)
        (Just 3.0) `shouldBe` res

    describe "getS" $ do
      it "get Just value when String value is supplied" $ do
        let res = getS (VS "Tidal")
        (Just "Tidal") `shouldBe` res
      it "get Nothing if Int value is not supplied" $ do
        let res = getS (VI 3)
        Nothing `shouldBe` res

    describe "filterValues" $ do
      it "remove Events above given threshold" $ do
        let fil = filterValues (< 2) $ fastCat [pure 1, pure 2, pure 3] :: Pattern Time
        let res = queryArc fil (Arc 0.5 1.5)
        fmap toEvent [(((1, 4 % 3), (1, 4 % 3)), 1 % 1)] `shouldBe` res

      it "remove Events below given threshold" $ do
        let fil = filterValues (> 2) $ fastCat [pure 1, pure 2, pure 3] :: Pattern Time
        let res = queryArc fil (Arc 0.5 1.5)
        fmap toEvent [(((2 % 3, 1), (2 % 3, 1)), 3 % 1)] `shouldBe` res

    describe "filterWhen" $ do
      it "filter below given threshold" $ do
        let fil = filterWhen (< 0.5) $ struct "t*4" $ (tri :: Pattern Double) + 1
        let res = queryArc fil (Arc 0.5 1.5)
        [] `shouldBe` res

      it "filter above given threshold" $ do
        let fil = stripContext $ filterWhen (> 0.5) $ struct "t*4" $ (tri :: Pattern Double) + 1
        let res = queryArc fil (Arc 0.5 1.5)
        fmap toEvent [(((3 % 4, 1), (3 % 4, 1)), 1.5), (((1, 5 % 4), (1, 5 % 4)), 1), (((5 % 4, 3 % 2), (5 % 4, 3 % 2)), 1.5)] `shouldBe` res

    describe "compressArc" $ do
      it "return empty if start time is greater than end time" $ do
        let res = queryArc (compressArc (Arc 0.8 0.1) (fast "1 2" "3 4" :: Pattern Time)) (Arc 1 2)
        [] `shouldBe` res

      it "return empty if start time or end time are greater than 1" $ do
        let res = queryArc (compressArc (Arc 0.1 2) (fast "1 2" "3 4" :: Pattern Time)) (Arc 1 2)
        [] `shouldBe` res

      it "return empty if start or end are less than zero" $ do
        let res = queryArc (compressArc (Arc (-0.8) 0.1) (fast "1 2" "3 4" :: Pattern Time)) (Arc 1 2)
        [] `shouldBe` res

      it "otherwise compress difference between start and end values of Arc" $ do
        let p = fast "1 2" "3 4" :: Pattern Time
        let res = queryArc (stripContext $ compressArc (Arc 0.2 0.8) p) (Arc 0 1)
        let expected = fmap toEvent [(((1 % 5, 1 % 2), (1 % 5, 1 % 2)), 3 % 1), (((1 % 2, 13 % 20), (1 % 2, 13 % 20)), 3 % 1), (((13 % 20, 4 % 5), (13 % 20, 4 % 5)), 4 % 1)]
        expected `shouldBe` res


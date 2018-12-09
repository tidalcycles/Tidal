{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.PatternTest where

import           Test.Microspec
import           TestUtils

import           Prelude             hiding ((*>), (<*))

import           Data.Ratio

import           Sound.Tidal.Control
import           Sound.Tidal.Core
import           Sound.Tidal.Pattern

import qualified Data.Map.Strict     as Map

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

    describe "eventWholeStart" $ do 
      it "retrieve first element of a tuple, inside first element of a tuple, inside the first of another" $ do 
        property $ 1 === eventWholeStart (Event (Arc 1 2) (Arc 3 4) (5 :: Int))

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

    describe "arcCyclesZW" $ do
      it "if start and end time are equal return list of (start, end)" $ do
        let res = arcCyclesZW (Arc 2.5 2.5)
        property $ [(Arc 2.5 2.5)] === res
      it "if start and end time are not equal call arcCycles (start, end) with same rules as above" $ do
        let res = arcCyclesZW (Arc 2.3 2.1)
        property $ [] === res
      it "if start time is less than end time" $ do
        let res = arcCyclesZW (Arc 2.1 2.3)
        property $ [(Arc 2.1 2.3)] === res
      it "if start time is greater than end time" $ do
        let res = arcCyclesZW (Arc 2.1 3.3)
        property $ [(Arc 2.1 3.0), (Arc 3.0 3.3)] === res

    describe "mapArc" $ do
      it "Apply a given function to the start and end values of an Arc" $ do
        let res = mapArc (+1) (Arc 3 5)
        property $ ((Arc 4 6) :: Arc) === res

    describe "mapCycle" $ do
      it "Apply a function to the Arc values minus the start value rounded down (sam'), adding both results to sam' to obtain the new Arc value" $ do
        let res = mapCycle (*2) (Arc 3.3 5)
        property $ ((Arc 3.6 7.0) :: Arc) === res

    describe "toTime" $ do
      it "Convert a number of type Real to a Time value of type Rational, Int test" $ do
        let res = toTime (3 :: Int)
        property $ (3 % 1 :: Time) === res
      it "Convert a number of type Double to a Time value of type Rational" $ do
        let res = toTime (3.2 :: Double)
        property $ (3602879701896397 % 1125899906842624 :: Time) === res

    describe "cyclePos" $ do
      it "Subtract a Time value from its value rounded down (the start of the cycle)" $ do
        let res = cyclePos 2.6
        property $ (0.6 :: Time) === res
      it "If no difference between a given Time and the start of the cycle" $ do
        let res = cyclePos 2
        property $ (0.0 :: Time) === res

    describe "isIn" $ do
      it "Check given Time is inside a given Arc value, Time is greater than start and less than end Arc values" $ do
        let res = isIn (Arc 2.0 2.8) 2.5
        property $ True === res
      it "Given Time is equal to the Arc start value" $ do
        let res = isIn (Arc 2.0 2.8) 2.0
        property $ True === res
      it "Given Time is less than the Arc start value" $ do
        let res = isIn (Arc 2.0 2.8) 1.4
        property $ False === res
      it "Given Time is greater than the Arc end value" $ do
        let res = isIn (Arc 2.0 2.8) 3.2
        property $ False === res

    describe "onsetIn" $ do
      it "If the beginning of an Event is within a given Arc, same rules as 'isIn'" $ do 
         let res = onsetIn (Arc 2.0 2.8) (Event (Arc 2.2 2.7) (Arc 3.3 3.8) (5 :: Int))
         property $ True === res 
      it "Beginning of Event is equal to beggining of given Arc" $ do 
         let res = onsetIn (Arc 2.0 2.8) (Event (Arc 2.0 2.7) (Arc 3.3 3.8) (5 :: Int))
         property $ True === res 
      it "Beginning of an Event is less than the start of the Arc" $ do 
         let res = onsetIn (Arc 2.0 2.8) (Event (Arc 1.2 1.7) (Arc 3.3 3.8) (5 :: Int))
         property $ False === res
      it "Start of Event is greater than the start of the given Arc" $ do 
         let res = onsetIn (Arc 2.0 2.8) (Event (Arc 3.1 3.5) (Arc 4.0 4.6) (5 :: Int))
         property $ False === res

    describe "subArc" $ do
      it "Checks if an Arc is within another, returns Just (max $ (fst a1) (fst a2), min $ (snd a1) (snd a2)) if so, otherwise Nothing" $ do       
        let res = subArc (Arc 2.1 2.4) (Arc 2.4 2.8)
        property $ Nothing === res
      it "if max (fst arc1) (fst arc2) <= min (snd arc1) (snd arc2) return Just (max (fst arc1) (fst arc2), min...)" $ do
        let res = subArc (Arc 2 2.8) (Arc 2.4 2.9)
        property $ Just (Arc 2.4 2.8) === res

    describe "timeToCycleArc" $ do
      it "given a Time value return the Arc in which it resides" $ do
        let res = timeToCycleArc 2.2 
        property $ (Arc 2.0 3.0) === res

    describe "cyclesInArc" $ do 
      it "Return a list of cycles in a given arc, if start is greater than end return empty list" $ do 
        let res = cyclesInArc (Arc 2.4 2.2)
        property $ ([] :: [Int]) === res
      it "If start value of Arc is equal to end value return list with start value rounded down" $ do
        let res = cyclesInArc (Arc 2.4 2.4)
        property $ ([2] :: [Int]) === res
      it "if start of Arc is less than end return list of start rounded down to end rounded up minus one" $ do
        let res = cyclesInArc (Arc 2.2 4.5)
        property $ ([2,3,4] :: [Int]) === res  

    describe "cycleArcsInArc" $ do
      it "generates a list of Arcs based on the cycles found within a given a Arc" $ do
       let res = cycleArcsInArc (Arc 2.2 4.5) 
       property $ [(Arc 2.0 3.0), (Arc 3.0 4.0), (Arc 4.0 5.0)] === res

    describe "isAdjacent" $ do
      it "if the given Events are adjacent parts of the same whole" $ do 
        let res = isAdjacent (Event (Arc 1 2) (Arc 3 4) 5) (Event (Arc 1 2) (Arc 4 3) (5 :: Int))
        property $ True === res 
      it "if first Arc of of first Event is not equal to first Arc of second Event" $ do
        let res = isAdjacent (Event (Arc 1 2) (Arc 3 4) 5) (Event (Arc 7 8) (Arc 4 3) (5 :: Int))
        property $ False === res  
      it "if the value of the first Event does not equal the value of the second Event" $ do 
        let res = isAdjacent (Event (Arc 1 2) (Arc 3 4) 5) (Event (Arc 1 2) (Arc 4 3) (6 :: Int))
        property $ False === res 
      it "second value of second Arc of first Event not equal to first value of second Arc in second Event..." $ do
        let res = isAdjacent (Event (Arc 1 2) (Arc 3 4) 5) (Event (Arc 1 2) (Arc 3 4) (5 :: Int))
        property $ False === res 

    describe "defragParts" $ do 
      it "if empty list with no events return empty list" $ do 
        let res = defragParts ([] :: [Event Int]) 
        property $ [] === res
      it "if list consists of only one Event return it as is" $ do 
        let res = defragParts [(Event (Arc 1 2) (Arc 3 4) (5 :: Int))]
        property $ [Event (Arc 1 2) (Arc 3 4) (5 :: Int)] === res 
      it "if list contains adjacent Events return list with Parts combined" $ do 
        let res = defragParts [(Event (Arc 1 2) (Arc 3 4) (5 :: Int)), (Event (Arc 1 2) (Arc 4 3) (5 :: Int))]
        property $ [(Event (Arc 1 2) (Arc 3 4) 5)] === res
      it "if list contains more than one Event none of which are adjacent, return List as is" $ do 
        let res = defragParts [(Event (Arc 1 2) (Arc 3 4) 5), (Event (Arc 7 8) (Arc 4 3) (5 :: Int))]
        property $ [Event (Arc 1 2) (Arc 3 4) 5, Event (Arc 7 8) (Arc 4 3) (5 :: Int)] === res

    describe "compareDefrag" $ do 
      it "compare list with Events with empty list of Events" $ do
        let res = compareDefrag [Event (Arc 1 2) (Arc 3 4) (5 :: Int), Event (Arc 1 2) (Arc 4 3) (5 :: Int)] []
        property $ False === res 
      it "compare lists containing same Events but of different length" $ do 
        let res = compareDefrag [Event (Arc 1 2) (Arc 3 4) (5 :: Int), Event (Arc 1 2) (Arc 4 3) 5] [Event (Arc 1 2) (Arc 3 4) (5 :: Int)]
        property $ True === res
      it "compare lists of same length with same Events" $ do 
        let res = compareDefrag [Event (Arc 1 2) (Arc 3 4) (5 :: Int)] [Event (Arc 1 2) (Arc 3 4) (5 :: Int)]
        property $ True === res 

    -- pending "Sound.Tidal.Pattern.eventL" $ do
    --  it "succeeds if the first event 'whole' is shorter" $ do
    --    property $ eventL (Event (Arc 0,0),(Arc 0 1)),"x") (((0 0) (Arc 0 1.1)) "x")
    --  it "fails if the events are the same length" $ do
    --    property $ not $ eventL (Event (Arc 0,0),(Arc 0 1)),"x") (((0 0) (Arc 0 1)) "x")
    --  it "fails if the second event is shorter" $ do
    --    property $ not $ eventL (Event (Arc 0,0),(Arc 0 1)),"x") (((0 0) (Arc 0 0.5)) "x")

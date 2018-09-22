import Test.Microspec

import Prelude hiding ((<*), (*>))

import Data.Ratio
import Data.List (sort)

import Sound.Tidal.Pattern
import Sound.Tidal.Utils

main :: IO ()
main = microspec $ do
  describe "Sound.Tidal.Pattern.eventWhole" $ do
    it "returns the first element of a tuple inside the first element of a tuple" $ do
      property $ (1,2) === eventWhole (((1,2),(3,4)), 5 :: Int)

  describe "Sound.Tidal.Pattern.eventPart" $ do
    it "returns the second element of a tuple inside the first element of a tuple" $ do
      property $ (3,4) === eventPart (((1,2),(3,4)), 5 :: Int)

  describe "Sound.Tidal.Utils.delta" $ do
    it "subtracts the second element of a tuple from the first" $ do
      property $ delta (3,10) === (7 :: Int)

  describe "Sound.Tidal.Pattern.pure" $ do
    it "fills a whole cycle" $ do
      property $ query (pure 0) (0,1) === [(((0,1),(0,1)),0 :: Int)]
    it "returns the part of an pure that you ask for, preserving the whole" $ do
      property $ query (pure 0) (0.25,0.75) === [(((0,1),(0.25,0.75)),0 :: Int)]
    it "gives correct fragments when you go over cycle boundaries" $ do
      property $ query (pure 0) (0.25,1.25) === [(((0,1),(0.25,1)),0 :: Int),
                                                (((1,2),(1,1.25)),0)
                                               ]
    it "works with zero-length queries" $ do
      it "0" $
        query (pure "a") (0,0)
          `shouldBe` [(((0,1), (0,0)), "a")]
      it "1/3" $
        query (pure "a") (1%3,1%3)
          `shouldBe` [(((0,1), (1%3,1%3)), "a")]

  describe "Sound.Tidal.Pattern.append" $ do
    it "can switch between the cycles from two pures" $ do
      (query (append (pure "a") (pure "b")) (0,5)) `shouldBe` [(((0,1), (0,1)), "a"),
                                                               (((1,2), (1,2)), "b"),
                                                               (((2,3), (2,3)), "a"),
                                                               (((3,4), (3,4)), "b"),
                                                               (((4,5), (4,5)), "a")
                                                              ]

  describe "Sound.Tidal.Pattern.cat" $ do
    it "can switch between the cycles from three pures" $ do
      query (cat [pure "a", pure "b", pure "c"]) (0,5) `shouldBe` [(((0,1), (0,1)), "a"),
                                                                   (((1,2), (1,2)), "b"),
                                                                   (((2,3), (2,3)), "c"),
                                                                   (((3,4), (3,4)), "a"),
                                                                   (((4,5), (4,5)), "b")
                                                                  ]

  describe "Sound.Tidal.Pattern.fastCat" $ do
    it "can switch between the cycles from three pures inside one cycle" $ do
      it "1" $ query (fastCat [pure "a", pure "b", pure "c"]) (0,1)
        `shouldBe` [(((0,1/3),   (0,1/3)),   "a"),
                    (((1/3,2/3), (1/3,2/3)), "b"),
                    (((2/3,1),   (2/3,1)),   "c")
                   ]
      it "5/3" $ query (fastCat [pure "a", pure "b", pure "c"]) (0,5/3)
        `shouldBe` [(((0,1/3),   (0,1/3)),   "a"),
                    (((1/3,2/3), (1/3,2/3)), "b"),
                    (((2/3,1),   (2/3,1)),   "c"),
                    (((1,4/3),   (1,4/3)),   "a"),
                    (((4/3,5/3), (4/3,5/3)), "b")
                   ]
    it "works with zero-length queries" $ do
      it "0" $
        query (fastCat [pure "a", pure "b"]) (0,0)
          `shouldBe` [(((0,0.5), (0,0)), "a")]
      it "1/3" $
        query (fastCat [pure "a", pure "b"]) (1%3,1%3)
          `shouldBe` [(((0,0.5), (1%3,1%3)), "a")]

  describe "Sound.Tidal.Pattern._fastGap" $ do
    it "copes with cross-cycle queries" $ do
      (query(_fastGap 2 $ fastCat [pure "a", pure "b"]) (0.5,1.5))
        `shouldBe`
        [(((1 % 1,5 % 4),(1 % 1,5 % 4)),"a"),
         (((5 % 4,3 % 2),(5 % 4,3 % 2)),"b")
        ]
    it "does not return events outside of the query" $ do
      (query(_fastGap 2 $ fastCat [pure "a", pure "b"]) (0.5,0.9))
        `shouldBe` []

  describe "Sound.Tidal.Pattern.<*>" $ do
    it "can apply a pattern of values to a pattern of values" $ do
      query ((pure (+1)) <*> (pure 3)) (0,1) `shouldBe` [(((0,1), (0,1)), 4  :: Int)]
    it "can take structure from the left" $ do
      query ((fastCat [pure (+1), pure (+2)]) <*> (pure 3)) (0,1) `shouldBe` [(((0,0.5), (0,0.5)), 4 :: Int),
                                                                              (((0.5,1), (0.5,1)), 5)
                                                                             ]
    it "can take structure from the right" $ do
      query (pure (+1) <*> (fastCat [pure 7, pure 8])) (0,1) `shouldBe` [(((0,0.5), (0,0.5)), 8 :: Int),
                                                                         (((0.5,1), (0.5,1)), 9)
                                                                        ]
    it "can take structure from the both sides" $ do
      it "one" $
        query ((fastCat [pure (+1), pure (+2)]) <*> (fastCat [pure 7, pure 8])) (0,1)
        `shouldBe` [(((0,0.5), (0,0.5)), 8 :: Int),
                    (((0.5,1), (0.5,1)), 10)
                   ]
      it "two" $
        query ((fastCat [pure (+1), pure (+2), pure (+3)]) <*> (fastCat [pure 7, pure 8])) (0,1)
        `shouldBe` [(((0 % 1,1 % 3),(0 % 1,1 % 3)),8 :: Int),
                    (((1 % 3,1 % 2),(1 % 3,1 % 2)),9),
                    (((1 % 2,2 % 3),(1 % 2,2 % 3)),10),
                    (((2 % 3,1 % 1),(2 % 3,1 % 1)),11)
                   ]
    it "obeys pure id <*> v = v" $ do
      let v = (fastCat [fastCat [pure 7, pure 8], pure 9]) :: Pattern Int
      query ((pure id <*> v)) (0,5) `shouldBe` query v (0,5)

    it "obeys pure f <*> pure x = pure (f x)" $ do
      let f = (+3)
          x = 7 :: Int
      query (pure f <*> pure x) (0,5) `shouldBe` query (pure (f x)) (0,5)

    it "obeys u <*> pure y = pure ($ y) <*> u" $ do
      let u = fastCat [pure (+7), pure (+8)]
          y = 6 :: Int
      query (u <*> pure y) (0,5) `shouldBe` query (pure ($ y) <*> u) (0,5)

    it "obeys pure (.) <*> u <*> v <*> w = u <*> (v <*> w)" $ do
      let u = (fastCat [pure (+7), pure (+8)]) :: Pattern (Int -> Int)
          v = fastCat [pure (+3), pure (+4), pure (+5)]
          w = fastCat [pure 1, pure 2]
      query (pure (.) <*> u <*> v <*> w) (0,5) `shouldBe` query (u <*> (v <*> w)) (0,5)

  describe "Sound.Tidal.Pattern.<*" $ do
    it "can apply a pattern of values to a pattern of functions" $ do
      query ((pure (+1)) <* (pure 3)) (0,1) `shouldBe` [(((0,1), (0,1)), 4  :: Int)]
    it "doesn't take structure from the right" $ do
      query (pure (+1) <* (fastCat [pure 7, pure 8])) (0,1)
        `shouldBe` [(((0,1), (0,1)), 8 :: Int)]

  describe "Sound.Tidal.Pattern.*>" $ do
    it "can apply a pattern of values to a pattern of functions" $ do
      it "works within cycles" $ query ((pure (+1)) *> (pure 3)) (0,1) `shouldBe` [(((0,1), (0,1)), 4  :: Int)]
      it "works across cycles" $ query ((pure (+1)) *> (slow 2 $ pure 3)) (0,1) `shouldBe` [(((0,2), (0,1)), 4  :: Int)]
    it "doesn't take structure from the left" $ do
      query (pure (+1) *> (fastCat [pure 7, pure 8])) (0,1)
        `shouldBe` [(((0,0.5), (0,0.5)), 8 :: Int),
                    (((0.5,1), (0.5,1)), 9 :: Int)
                   ]
{-
  describe "Sound.Tidal.Pattern.eventL" $ do
    it "succeeds if the first event 'whole' is shorter" $ do
      property $ eventL (((0,0),(0,1)),"x") (((0,0),(0,1.1)),"x")
    it "fails if the events are the same length" $ do
      property $ not $ eventL (((0,0),(0,1)),"x") (((0,0),(0,1)),"x")
    it "fails if the second event is shorter" $ do
      property $ not $ eventL (((0,0),(0,1)),"x") (((0,0),(0,0.5)),"x")
-}

  describe "Sound.Tidal.Pattern.arcCycles" $ do
    it "leaves a unit cycle intact" $ do
      it "(0,1)" $ arcCycles (0,1) `shouldBe` [(0,1)]
      it "(3,4)" $ arcCycles (3,4) `shouldBe` [(3,4)]
    it "splits a cycle at cycle boundaries" $ do
      it "(0,1.1)" $ arcCycles (0,1.1) `shouldBe` [(0,1),(1,1.1)]
      it "(1,2,1)" $ arcCycles (1,2.1) `shouldBe` [(1,2),(2,2.1)]
      it "(3 + (1%3),5.1)" $
         arcCycles (3 + (1%3),5.1) `shouldBe` [(3+(1%3),4),(4,5),(5,5.1)]

  describe "Sound.Tidal.Pattern.rev" $ do
    it "mirrors events" $ do
      let forward = fastCat [fastCat [pure 7, pure 8], pure 9] :: Pattern Int
          backward = fastCat [pure 9, fastCat [pure 8, pure 7]]
      -- sort the events into time order to compare them
      (sort $ query (rev forward) (0,1)) `shouldBe` (sort $ query (backward) (0,1))

    it "returns the original if you reverse it twice" $ do
      let x = fastCat [fastCat [pure 7, pure 8], pure 9] :: Pattern Int
      (query (rev $ rev x) (0,5)) `shouldBe` (query x (0,5))


  describe "Sound.Tidal.Pattern.compress" $ do
    it "squashes cycles to the start of a cycle" $ do
      let p = compress (0, 0.5) $ fastCat [pure 7, pure 8] :: Pattern Int
      (query p (0,1)) `shouldBe` [(((0,0.25),  (0,0.25)),   7),
                                  (((0.25,0.5),(0.25,0.5)), 8)
                                 ]
    it "squashes cycles to the end of a cycle" $ do
      let p = compress (0.5,1) $ fastCat [pure 7, pure 8] :: Pattern Int
      (query p (0,1)) `shouldBe` [(((0.5,0.75),  (0.5,0.75)), 7 :: Int),
                                  (((0.75,1),    (0.75,1)),   8)
                                 ]
    it "squashes cycles to the middle of a cycle" $ do
      let p = compress (0.25,0.75) $ fastCat [pure 7, pure 8]
      (query p (0,1)) `shouldBe` [(((0.25,0.5),  (0.25,0.5)), 7 :: Int),
                                  (((0.5,0.75),  (0.5,0.75)), 8)
                                 ]

  describe "Sound.Tidal.Pattern.unwrap" $ do
    it "preserves inner structure" $ do
      it "one" $
        (query (unwrap $ pure (fastCat [pure "a", pure "b"])) (0,1))
        `shouldBe` (query (fastCat [pure "a", pure "b"]) (0,1))
      it "two" $
        (query (unwrap $ pure (fastCat [pure "a", pure "b", fastCat [pure "c", pure "d"]])) (0,1))
        `shouldBe` (query (fastCat [pure "a", pure "b", fastCat [pure "c", pure "d"]]) (0,1))
    it "preserves outer structure" $ do
      it "one" $
        (query (unwrap $ fastCat [pure $ pure "a", pure $ pure "b"]) (0,1))
        `shouldBe` (query (fastCat [pure "a", pure "b"]) (0,1))
      it "two" $
        (query (unwrap $ fastCat [pure $ pure "a", pure $ pure "b", fastCat [pure $ pure "c", pure $ pure "d"]]) (0,1))
        `shouldBe` (query (fastCat [pure "a", pure "b", fastCat [pure "c", pure "d"]]) (0,1))
    it "gives events whole/part timespans that are an intersection of that of inner and outer events" $ do
      let a = fastCat [pure "a", pure "b"]
          b = fastCat [pure "c", pure "d", pure "e"]
          pp = fastCat [pure a, pure b]
      query (unwrap pp) (0,1)
        `shouldBe` [(((0 % 1,1 % 2),(0 % 1,1 % 2)),"a"),
                    (((1 % 2,2 % 3),(1 % 2,2 % 3)),"d"),
                    (((2 % 3,1 % 1),(2 % 3,1 % 1)),"e")
                   ]
        
  describe "Sound.Tidal.Pattern.unwrap'" $ do
    it "compresses cycles to fit outer 'whole' timearc of event" $ do
      let a = fastCat [pure "a", pure "b"]
          b = fastCat [pure "c", pure "d", pure "e"]
          pp = fastCat [pure a, pure b]
      query (unwrap' pp) (0,1)
        `shouldBe` [(((0 % 1,1 % 4),(0 % 1,1 % 4)),"a"),
                    (((1 % 4,1 % 2),(1 % 4,1 % 2)),"b"),
                    (((1 % 2,2 % 3),(1 % 2,2 % 3)),"c"),
                    (((2 % 3,5 % 6),(2 % 3,5 % 6)),"d"),
                    (((5 % 6,1 % 1),(5 % 6,1 % 1)),"e")
                   ]

  describe "Sound.Tidal.Pattern.>>=" $ do
    it "can apply functions to patterns" $ do
      let p = fastCat [pure 7, pure 8] :: Pattern Int
          p' = do x <- p
                  return $ x + 1
      (query p' (0,1)) `shouldBe` (query ((+1) <$> p) (0,1))

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
    -- if "conforms to (m >>= f) >>= g ≡ m >>= ( \x -> (f x >>= g) )"
       
  describe "Sound.Tidal.Pattern.saw" $ do
    it "goes from 0 up to 1 every cycle" $ do
      it "0" $
        (query saw (0,0))    `shouldBe` [(((0,0), (0,0)),    0 :: Float)]
      it "0.25" $
        (query saw (0.25,0.25)) `shouldBe` [(((0.25,0.25), (0.25,0.25)), 0.25 :: Float)]
      it "0.5" $
        (query saw (0.5,0.5))  `shouldBe` [(((0.5,0.5), (0.5,0.5) ), 0.5 :: Float)]
      it "0.75" $
        (query saw (0.75,0.75)) `shouldBe` [(((0.75,0.75), (0.75,0.75)), 0.75 :: Float)]
    it "can be added to" $ do
      (map eventValue $ query ((+1) <$> saw) (0.5,0.5)) `shouldBe` [1.5 :: Float]
    it "works on the left of <*>" $ do
      (query ((+) <$> saw <*> pure 3) (0,1))
        `shouldBe` [(((0,1), (0,1)), 3 :: Float)]
    it "works on the right of <*>" $ do
      (query ((fast 4 $ pure (+3)) <*> saw) (0,1))
        `shouldBe` [(((0,0.25), (0,0.25)), 3 :: Float),
                    (((0.25,0.5), (0.25,0.5)), 3.25),
                    (((0.5,0.75), (0.5,0.75)), 3.5),
                    (((0.75,1), (0.75,1)), 3.75)
                   ]
    it "can be reversed" $ do
      it "works with whole cycles" $
        (query (rev saw) (0,1))
          `shouldBe` [(((0,1), (0,1)), 0.5 :: Float)]
      it "works with half cycles" $
        (query (rev saw) (0,0.5))
          `shouldBe` [(((0,0.5), (0,0.5)), 0.75 :: Float)]
      it "works with inset points" $
        (query (rev saw) (0.25,0.25))
          `shouldBe` [(((0.25,0.25), (0.25,0.25)), 0.75 :: Float)]

  describe "Sound.Tidal.Pattern.temporalParam" $ do
    it "works over two cycles" $
      property $ comparePD (0,2) (0.25 ~> pure "a") (0.25 `rotR` pure "a")
    it "works over one cycle" $
      property $ compareP (0,1) (0.25 ~> pure "a") (0.25 `rotR` pure "a")
    it "works with zero width queries" $
      property $ compareP (0,0) (0.25 ~> pure "a") (0.25 `rotR` pure "a")
       
    

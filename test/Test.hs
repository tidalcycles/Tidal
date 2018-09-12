import Test.Microspec
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

  describe "Sound.Tidal.Pattern.atom" $ do
    it "fills a whole cycle" $ do
      property $ query (atom 0) (0,1) === [(((0,1),(0,1)),0 :: Int)]
    it "returns the part of an atom that you ask for, preserving the whole" $ do
      property $ query (atom 0) (0.25,0.75) === [(((0,1),(0.25,0.75)),0 :: Int)]
    it "gives correct fragments when you go over cycle boundaries" $ do
      property $ query (atom 0) (0.25,1.25) === [(((0,1),(0.25,1)),0 :: Int),
                                                (((1,2),(1,1.25)),0)
                                               ]

  describe "Sound.Tidal.Pattern.append" $ do
    it "can switch between the cycles from two atoms" $ do
      (query (append (atom "a") (atom "b")) (0,5)) `shouldBe` [(((0,1), (0,1)), "a"),
                                                               (((1,2), (1,2)), "b"),
                                                               (((2,3), (2,3)), "a"),
                                                               (((3,4), (3,4)), "b"),
                                                               (((4,5), (4,5)), "a")
                                                              ]

  describe "Sound.Tidal.Pattern.cat" $ do
    it "can switch between the cycles from three atoms" $ do
      query (cat [atom "a", atom "b", atom "c"]) (0,5) `shouldBe` [(((0,1), (0,1)), "a"),
                                                                   (((1,2), (1,2)), "b"),
                                                                   (((2,3), (2,3)), "c"),
                                                                   (((3,4), (3,4)), "a"),
                                                                   (((4,5), (4,5)), "b")
                                                                  ]

  describe "Sound.Tidal.Pattern.fastCat" $ do
    it "can switch between the cycles from three atoms inside one cycle" $ do
      query (fastCat [atom "a", atom "b", atom "c"]) (0,5/3) `shouldBe` [(((0,1/3),   (0,1/3)),   "a"),
                                                                         (((1/3,2/3), (1/3,2/3)), "b"),
                                                                         (((2/3,1),   (2/3,1)),   "c"),
                                                                         (((1,4/3),   (1,4/3)),   "a"),
                                                                         (((4/3,5/3), (4/3,5/3)), "b")
                                                                        ]
  describe "Sound.Tidal.Pattern.<*>" $ do
    it "can apply a pattern of values to a pattern of values" $ do
      query ((atom (+1)) <*> (atom 3)) (0,1) `shouldBe` [(((0,1), (0,1)), 4  :: Int)]
    it "can take structure from the left" $ do
      query ((fastCat [atom (+1), atom (+2)]) <*> (atom 3)) (0,1) `shouldBe` [(((0,0.5), (0,0.5)), 4 :: Int),
                                                                              (((0.5,1), (0.5,1)), 5)
                                                                             ]
    it "can take structure from the right" $ do
      query (atom (+1) <*> (fastCat [atom 7, atom 8])) (0,1) `shouldBe` [(((0,0.5), (0,0.5)), 8 :: Int),
                                                                         (((0.5,1), (0.5,1)), 9)
                                                                        ]
    it "can take structure from the both sides" $ do
      it "one" $
        query ((fastCat [atom (+1), atom (+2)]) <*> (fastCat [atom 7, atom 8])) (0,1)
        `shouldBe` [(((0,0.5), (0,0.5)), 8 :: Int),
                    (((0.5,1), (0.5,1)), 10)
                   ]
      it "two" $
        query ((fastCat [atom (+1), atom (+2), atom (+3)]) <*> (fastCat [atom 7, atom 8])) (0,1)
        `shouldBe` [(((0 % 1,1 % 3),(0 % 1,1 % 3)),8 :: Int),
                    (((1 % 3,1 % 2),(1 % 3,1 % 2)),9),
                    (((1 % 2,2 % 3),(1 % 2,2 % 3)),10),
                    (((2 % 3,1 % 1),(2 % 3,1 % 1)),11)
                   ]
    it "obeys pure id <*> v = v" $ do
      let v = (fastCat [fastCat [atom 7, atom 8], atom 9]) :: Pattern Int
      query ((pure id <*> v)) (0,5) `shouldBe` query v (0,5)

    it "obeys pure f <*> pure x = pure (f x)" $ do
      let f = (+3)
          x = 7 :: Int
      query (pure f <*> pure x) (0,5) `shouldBe` query (pure (f x)) (0,5)

    it "obeys u <*> pure y = pure ($ y) <*> u" $ do
      let u = fastCat [atom (+7), atom (+8)]
          y = 6 :: Int
      query (u <*> pure y) (0,5) `shouldBe` query (pure ($ y) <*> u) (0,5)

    it "obeys pure (.) <*> u <*> v <*> w = u <*> (v <*> w)" $ do
      let u = (fastCat [atom (+7), atom (+8)]) :: Pattern (Int -> Int)
          v = fastCat [atom (+3), atom (+4), atom (+5)]
          w = fastCat [atom 1, atom 2]
      query (pure (.) <*> u <*> v <*> w) (0,5) `shouldBe` query (u <*> (v <*> w)) (0,5)
          
{-
  describe "Sound.Tidal.Pattern.eventL" $ do
    it "succeeds if the first event 'whole' is shorter" $ do
      property $ eventL (((0,0),(0,1)),"x") (((0,0),(0,1.1)),"x")
    it "fails if the events are the same length" $ do
      property $ not $ eventL (((0,0),(0,1)),"x") (((0,0),(0,1)),"x")
    it "fails if the second event is shorter" $ do
      property $ not $ eventL (((0,0),(0,1)),"x") (((0,0),(0,0.5)),"x")
-}

  describe "Sound.Tidal.Pattern.spanCycles" $ do
    it "leaves a unit cycle intact" $ do
      it "(0,1)" $ spanCycles (0,1) `shouldBe` [(0,1)]
      it "(3,4)" $ spanCycles (3,4) `shouldBe` [(3,4)]
    it "splits a cycle at cycle boundaries" $ do
      it "(0,1.1)" $ spanCycles (0,1.1) `shouldBe` [(0,1),(1,1.1)]
      it "(1,2,1)" $ spanCycles (1,2.1) `shouldBe` [(1,2),(2,2.1)]
      it "(3 + (1%3),5.1)" $
         spanCycles (3 + (1%3),5.1) `shouldBe` [(3+(1%3),4),(4,5),(5,5.1)]

  describe "Sound.Tidal.Pattern.rev" $ do
    it "mirrors events" $ do
      let forward = fastCat [fastCat [atom 7, atom 8], atom 9] :: Pattern Int
          backward = fastCat [atom 9, fastCat [atom 8, atom 7]]
      -- sort the events into time order to compare them
      (sort $ query (rev forward) (0,1)) `shouldBe` (sort $ query (backward) (0,1))

    it "returns the original if you reverse it twice" $ do
      let x = fastCat [fastCat [atom 7, atom 8], atom 9] :: Pattern Int
      (query (rev $ rev x) (0,5)) `shouldBe` (query x (0,5))


  describe "Sound.Tidal.Pattern.compress" $ do
    it "squashes cycles to the start of a cycle" $ do
      let p = compress (0, 0.5) $ fastCat [atom 7, atom 8] :: Pattern Int
      (query p (0,1)) `shouldBe` [(((0,0.25),  (0,0.25)),   7),
                                  (((0.25,0.5),(0.25,0.5)), 8)
                                 ]
    it "squashes cycles to the end of a cycle" $ do
      let p = compress (0.5,1) $ fastCat [atom 7, atom 8] :: Pattern Int
      (query p (0,1)) `shouldBe` [(((0.5,0.75),  (0.5,0.75)), 7 :: Int),
                                  (((0.75,1),    (0.75,1)),   8)
                                 ]
    it "squashes cycles to the middle of a cycle" $ do
      let p = compress (0.25,0.75) $ fastCat [atom 7, atom 8]
      (query p (0,1)) `shouldBe` [(((0.25,0.5),  (0.25,0.5)), 7 :: Int),
                                  (((0.5,0.75),  (0.5,0.75)), 8)
                                 ]

  describe "Sound.Tidal.Pattern.joinPattern" $ do
    it "preserves inner structure" $ do
      it "one" $
        (query (joinPattern $ atom (fastCat [atom "a", atom "b"])) (0,1))
        `shouldBe` (query (fastCat [atom "a", atom "b"]) (0,1))
      it "two" $
        (query (joinPattern $ atom (fastCat [atom "a", atom "b", fastCat [atom "c", atom "d"]])) (0,1))
        `shouldBe` (query (fastCat [atom "a", atom "b", fastCat [atom "c", atom "d"]]) (0,1))
    it "preserves outer structure" $ do
      it "one" $
        (query (joinPattern $ fastCat [atom $ atom "a", atom $ atom "b"]) (0,1))
        `shouldBe` (query (fastCat [atom "a", atom "b"]) (0,1))
      it "two" $
        (query (joinPattern $ fastCat [atom $ atom "a", atom $ atom "b", fastCat [atom $ atom "c", atom $ atom "d"]]) (0,1))
        `shouldBe` (query (fastCat [atom "a", atom "b", fastCat [atom "c", atom "d"]]) (0,1))

  describe "Sound.Tidal.Pattern.>>=" $ do
    it "can apply functions to patterns" $ do
      let p = fastCat [atom 7, atom 8] :: Pattern Int
          p' = do x <- p
                  return $ x + 1
      (query p' (0,1)) `shouldBe` (query ((+1) <$> p) (0,1))

    it "can add two patterns together" $ do
      let p1 = fastCat [atom 7, atom 8, atom 9] :: Pattern Int
          p2 = fastCat [atom 4, fastCat [atom 5, atom 6]]
          p' = do x <- p1
                  y <- p2
                  return $ x + y
      (sort $ query p' (0,1)) `shouldBe` (sort $ query ((+) <$> p1 <*> p2) (0,1))

    it "conforms to (return v) >>= f = f v" $ do
      let f x = pure $ x + 10
          v = 5 :: Int
      compareP 5 ((return v) >>= f) (f v)
    it "conforms to m >>= return ≡ m" $ do
      let m = fastCat [atom "a", fastCat [atom "b", atom "c"]]
      compareP 5 (m >>= return) m
    -- if "conforms to (m >>= f) >>= g ≡ m >>= ( \x -> (f x >>= g) )"
       

compareP :: (Ord a, Show a) => Time -> Pattern a -> Pattern a -> Property
compareP n p p' = (sort $ query p (0,n)) === (sort $ query p' (0,n))


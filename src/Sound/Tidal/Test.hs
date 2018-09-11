module Sound.Tidal.Test where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Sound.Tidal.Pattern
import Sound.Tidal.Utils
import Data.Ratio

main :: IO ()
main = hspec $ do
  describe "Sound.Tidal.Pattern.eventWhole" $ do
    it "returns the first element of a tuple inside the first element of a tuple" $ do
      property $ (1,2) == eventWhole (((1,2),(3,4)), 5)

  describe "Sound.Tidal.Pattern.eventPart" $ do
    it "returns the second element of a tuple inside the first element of a tuple" $ do
      property $ (3,4) == eventPart (((1,2),(3,4)), 5)

  describe "Sound.Tidal.Utils.delta" $ do
    it "subtracts the second element of a tuple from the first" $ do
      property $ delta (3,10) == 7

  describe "Sound.Tidal.Pattern.atom" $ do
    it "fills a whole cycle" $ do
      property $ query (atom 0) (0,1) == [(((0,1),(0,1)),0)]
    it "returns the part of an atom that you ask for, preserving the whole" $ do
      property $ query (atom 0) (0.25,0.75) == [(((0,1),(0.25,0.75)),0)]
    it "gives correct fragments when you go over cycle boundaries" $ do
      property $ query (atom 0) (0.25,1.25) == [(((0,1),(0.25,1)),0),
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
      query ((atom (+1)) <*> (atom 3)) (0,1) `shouldBe` [(((0,1), (0,1)), 4)]
    it "can take structure from the left" $ do
      query ((fastCat [atom (+1), atom (+2)]) <*> (atom 3)) (0,1) `shouldBe` [(((0,0.5), (0,0.5)), 4),
                                                                              (((0.5,1), (0.5,1)), 5)
                                                                             ]
    it "can take structure from the right" $ do
      query (atom (+1) <*> (fastCat [atom 7, atom 8])) (0,1) `shouldBe` [(((0,0.5), (0,0.5)), 8),
                                                                         (((0.5,1), (0.5,1)), 9)
                                                                        ]
    it "can take structure from the both sides" $ do
      query ((fastCat [atom (+1), atom (+2)]) <*> (fastCat [atom 7, atom 8])) (0,1)
        `shouldBe` [(((0,0.5), (0,0.5)), 8),
                    (((0.5,1), (0.5,1)), 10)
                   ]
      query ((fastCat [atom (+1), atom (+2), atom (+3)]) <*> (fastCat [atom 7, atom 8])) (0,1)
        `shouldBe` [(((0 % 1,1 % 3),(0 % 1,1 % 3)),8),
                    (((1 % 3,1 % 2),(1 % 3,1 % 2)),9),
                    (((1 % 2,2 % 3),(1 % 2,2 % 3)),10),
                    (((2 % 3,1 % 1),(2 % 3,1 % 1)),11)
                   ]
    it "obeys pure id <*> v = v" $ do
      let v = fastCat [fastCat [atom 7, atom 8], atom 9]
      query ((pure id <*> v)) (0,5) `shouldBe` query v (0,5)

    it "obeys pure f <*> pure x = pure (f x)" $ do
      let f = (+3)
          x = 7
      query (pure f <*> pure x) (0,5) `shouldBe` query (pure (f x)) (0,5)

    it "obeys u <*> pure y = pure ($ y) <*> u" $ do
      let u = fastCat [atom (+7), atom (+8)]
          y = 6
      query (u <*> pure y) (0,5) `shouldBe` query (pure ($ y) <*> u) (0,5)

    it "obeys pure (.) <*> u <*> v <*> w = u <*> (v <*> w)" $ do
      let u = fastCat [atom (+7), atom (+8)]
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
      spanCycles (0,1) `shouldBe` [(0,1)]
      spanCycles (3,4) `shouldBe` [(3,4)]
    it "splits a cycle at cycle boundaries" $ do
      spanCycles (0,1.1) `shouldBe` [(0,1),(1,1.1)]
      spanCycles (1,2.1) `shouldBe` [(1,2),(2,2.1)]
      spanCycles (3 + (1%3),5.1) `shouldBe` [(3+(1%3),4),(4,5),(5,5.1)]



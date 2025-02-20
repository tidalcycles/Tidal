{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.UtilsTest where

import Sound.Tidal.Utils
import Test.Hspec
import Prelude hiding ((*>), (<*))

run :: Spec
run =
  describe "Sound.Tidal.Utils" $ do
    it "subtracts the second element of a tuple from the first" $ do
        delta (3, 10) `shouldBe` (7 :: Int)

    it "applies function to both elements of tuple" $ do
        let res = mapBoth (+ 1) (2, 5)
        res `shouldBe` ((3, 6) :: (Int, Int))

    it "apply function to first element of tuple" $ do
        let res = mapFst (+ 1) (2, 5)
        res `shouldBe` ((3, 5) :: (Int, Int))

    it "apply function to second element of tuple" $ do
        let res = mapSnd (+ 1) (2, 5)
        res `shouldBe` ((2, 6) :: (Int, Int))

    it "return midpoint between first and second tuple value" $ do
        let res = mid (2, 5)
        res `shouldBe` (3.5 :: Double)

    it "return of two lists, with unique values to each list" $ do
        let res = removeCommon [1, 2, 5, 7, 12, 16] [2, 3, 4, 5, 15, 16]
        res `shouldBe` (([1, 7, 12], [3, 4, 15]) :: ([Int], [Int]))

    it "wrap around indexing" $ do
        let res = (!!!) [1 .. 5] 7
        res `shouldBe` (3 :: Int)

    it "safe list indexing" $ do
        let res = nth 2 ([] :: [Int])
        res `shouldBe` Nothing 

    it "list accumulation with given list elements" $ do
        let res = accumulate ([1 .. 5] :: [Int])
        res `shouldBe` [1, 3, 6, 10, 15]

    it "index elements in list" $ do
        let res = enumerate ['a', 'b', 'c']
        res `shouldBe` [(0, 'a'), (1, 'b'), (2, 'c')]

    it "split list by given pred" $ do
        let res = wordsBy (== ':') "bd:3"
        res `shouldBe` ["bd", "3"]

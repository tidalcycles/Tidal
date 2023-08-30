{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.UtilsTest where

import Test.Microspec

import Sound.Tidal.Utils

run :: Microspec ()
run =
  describe "Sound.Tidal.Utils" $ do
    describe "delta" $ do
      it "subtracts the second element of a tuple from the first" $ do
        property $ delta (3,10) === (7 :: Int)
   
    describe "applies function to both elements of tuple" $ do
       let res = mapBoth (+1) (2,5) 
       property $ ((3,6) :: (Int, Int)) === res

    describe "apply function to first element of tuple" $ do
       let res = mapFst (+1) (2, 5) 
       property $ ((3, 5) :: (Int, Int)) === res

    describe "apply function to second element of tuple" $ do
       let res = mapSnd (+1) (2, 5)
       property $ ((2, 6) :: (Int, Int)) === res
     
    describe "return midpoint between first and second tuple value" $ do
       let res = mid (2, 5)
       property $ (3.5 :: Double) === res

    describe "return of two lists, with unique values to each list" $ do
       let res = removeCommon [1,2,5,7,12,16] [2,3,4,5,15,16]
       property $ (([1,7,12],[3,4,15]) :: ([Int], [Int])) === res

    describe "wrap around indexing" $ do
       let res = (!!!) [1..5] 7
       property $ (3 :: Int) === res

    describe "safe list indexing" $ do 
       let res = nth 2 ([] :: [Int])
       property $ Nothing === res

    describe "list accumulation with given list elements" $ do
       let res = accumulate ([1..5] :: [Int])
       property $ [1,3,6,10,15] === res 

    describe "index elements in list" $ do
       let res = enumerate ['a', 'b', 'c']
       property $ [(0,'a'),(1,'b'),(2,'c')] === res

    describe "split list by given pred" $ do 
       let res = wordsBy (== ':') "bd:3"
       property $ ["bd", "3"] === res

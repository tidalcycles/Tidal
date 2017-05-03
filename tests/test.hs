{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
-- import Test.Tasty.SmallCheck as SC
-- import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import Sound.Tidal.Context

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [sequences]

sequences :: TestTree
sequences = testGroup "Sequences" [basic1]


basic1 = testGroup "fast / slow"
         [
           testCase "silence" $ same16 "silence" (fast 1.1 silence) (silence :: Pattern Int),
           testCase "fast" $ same16 "silence" silence (silence :: Pattern Int),
           testCase "fast" $ same16 "bd*128" "bd*128" (rep 128 "bd")
         ]

rep :: Int -> String -> Pattern String
rep n v = p $ intercalate " " $ take n $ repeat v

sameN :: (Eq a, Show a) => String -> Time -> Pattern a -> Pattern a -> Assertion
sameN s n a b = assertEqual s (arc a (0,n)) (arc b (0,n))

same16 s = sameN s 16


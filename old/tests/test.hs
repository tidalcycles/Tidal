{-# LANGUAGE OverloadedStrings #-}

-- import Test.Tasty.SmallCheck as SC
-- import Test.Tasty.QuickCheck as QC

import Data.List
import Data.Ord
import Sound.Tidal.Context
import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ basic1,
      parser1
      -- patternsOfPatterns
    ]

basic1 =
  testGroup
    "fast / slow"
    [ testCase "silence" $ same16 (fast 1.1 silence) (silence :: Pattern Double),
      testCase "fast" $ same16 silence (silence :: Pattern Double),
      testCase "fast2" $ same16 "bd*128" (rep 128 "bd")
    ]

parser1 =
  testGroup
    "subpatterns"
    [ testCase "square" $ same16 ("bd sn" :: Pattern String) ("[bd sn]" :: Pattern String)
    ]

patternsOfPatterns =
  testGroup
    "patterns of patterns"
    [ testCase "decimal density" $ same16 (_discretise 0.25 saw) (discretise 0.25 saw)
    ]

rep :: Int -> String -> Pattern String
rep n v = p $ intercalate " " $ take n $ repeat v

sameN :: (Eq a, Show a) => String -> Time -> Pattern a -> Pattern a -> Assertion
sameN s n a b = assertEqual s (arc a (0, n)) (arc b (0, n))

same16 :: (Eq a, Show a) => Pattern a -> Pattern a -> Assertion
same16 = sameN "for 16 cycles," 16

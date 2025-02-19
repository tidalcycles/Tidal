{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module TestUtils where

import Data.List (sort)
import qualified Data.Map.Strict as Map
import Sound.Tidal.Context
import Test.Hspec
import Prelude hiding ((*>), (<*))

class TolerantEq a where
  (~==) :: a -> a -> Bool

instance TolerantEq Double where
  a ~== b = abs (a - b) < 0.000001

instance TolerantEq Value where
  (VS a) ~== (VS b) = a == b
  (VI a) ~== (VI b) = a == b
  (VR a) ~== (VR b) = a == b
  (VF a) ~== (VF b) = abs (a - b) < 0.000001
  _ ~== _ = False

instance (TolerantEq a) => TolerantEq [a] where
  as ~== bs = (length as == length bs) && all (uncurry (~==)) (zip as bs)

instance TolerantEq ValueMap where
  a ~== b = Map.differenceWith (\a' b' -> if a' ~== b' then Nothing else Just a') a b == Map.empty

instance TolerantEq (Event ValueMap) where
  (Event _ w p x) ~== (Event _ w' p' x') = w == w' && p == p' && x ~== x'

-- | Compare the events of two patterns using the given arc
compareP :: (Ord a, Show a) => Arc -> Pattern a -> Pattern a -> Expectation
compareP a p p' =
  sort (queryArc (stripContext p) a)
    `shouldBe` sort (queryArc (stripContext p') a)

-- | Like @compareP@, but tries to 'defragment' the events
comparePD :: (Ord a, Show a) => Arc -> Pattern a -> Pattern a -> Expectation
comparePD a p p' =
  sort (defragParts $ queryArc (stripContext p) a)
    `shouldBe` sort (defragParts $ queryArc (stripContext p') a)

-- | Like @compareP@, but for control patterns, with some tolerance for floating point error
compareTol :: Arc -> ControlPattern -> ControlPattern -> Bool
compareTol a p p' = sort (queryArc (stripContext p) a) ~== sort (queryArc (stripContext p') a)

-- | Utility to create a pattern from a String
ps :: String -> Pattern String
ps = parseBP_E

stripContext :: Pattern a -> Pattern a
stripContext = setContext $ Context []

firstCycleValues :: Pattern a -> [a]
firstCycleValues pat = map value $ queryArc pat (Arc 0 1)

{-# LANGUAGE OverloadedStrings, FlexibleInstances, TypeSynonymInstances #-}

module TestUtils where

import Test.Microspec

import Prelude hiding ((<*), (*>))

import Data.List (sort)

import Sound.Tidal.Context

import qualified Data.Map.Strict as Map

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

instance TolerantEq a => TolerantEq [a] where
  as ~== bs = (length as == length bs) && all (uncurry (~==)) (zip as bs)

instance TolerantEq ValueMap where
  a ~== b = Map.differenceWith (\a' b' -> if a' ~== b' then Nothing else Just a') a b == Map.empty

instance TolerantEq (Event ValueMap) where
  (Event _ w p x) ~== (Event _ w' p' x') = w == w' && p == p' && x ~== x'

-- | Compare the events of two patterns using the given arc
compareP :: (Ord a, Show a) => Arc -> Pattern a -> Pattern a -> Property
compareP a p p' =
  (sort $ query (stripContext p) $ State a Map.empty)
  `shouldBe`
  (sort $ query (stripContext p') $ State a Map.empty)

-- | Like @compareP@, but tries to 'defragment' the events
comparePD :: (Ord a, Show a) => Arc -> Pattern a -> Pattern a -> Property
comparePD a p p' =
  (sort $ defragParts $ query (stripContext p) (State a Map.empty))
  `shouldBe`
  (sort $ defragParts $ query (stripContext p') (State a Map.empty))

-- | Like @compareP@, but for control patterns, with some tolerance for floating point error
compareTol :: Arc -> ControlPattern -> ControlPattern -> Bool
compareTol a p p' = (sort $ queryArc (stripContext p) a) ~== (sort $ queryArc (stripContext p') a)

-- | Utility to create a pattern from a String
ps :: String -> Pattern String
ps = parseBP_E

stripContext :: Pattern a -> Pattern a
stripContext = setContext $ Context []

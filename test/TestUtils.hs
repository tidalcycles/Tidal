{-# LANGUAGE OverloadedStrings #-}

module TestUtils where

import Test.Microspec

import Prelude hiding ((<*), (*>))

import Data.List (sort)

import Sound.Tidal.ParseBP (parseBP_E)
import Sound.Tidal.Pattern

import qualified Data.Map.Strict as Map

-- | Compare the events of two patterns using the given arc
compareP :: (Ord a, Show a) => Arc -> Pattern a -> Pattern a -> Property
compareP a p p' = (sort $ query p $ State a Map.empty) `shouldBe` (sort $ query p' $ State a Map.empty)

-- | Like @compareP@, but tries to 'defragment' the events
comparePD :: (Ord a, Show a) => Arc -> Pattern a -> Pattern a -> Bool
comparePD a p p' = compareDefrag es es'
  where es = query p (State a Map.empty)
        es' = query p' (State a Map.empty)

-- | Like @compareP@, but for control patterns, with some tolerance for floating point error
compareTol :: Arc -> ControlPattern -> ControlPattern -> Bool
compareTol a p p' = (sort $ queryArc p a) ~== (sort $ queryArc p' a)

-- | Utility to create a pattern from a String
ps :: String -> Pattern String
ps = parseBP_E

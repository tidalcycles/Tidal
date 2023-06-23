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
compareP :: (Ord a, Show a) => Arc -> Signal a -> Signal a -> Property
compareP a p p' =
  (sort $ queryArc (stripMetadata p) a)
  `shouldBe`
  (sort $ queryArc (stripMetadata p') a)

-- | Like @compareP@, but tries to 'defragment' the events
comparePD :: (Ord a, Show a) => Arc -> Signal a -> Signal a -> Property
comparePD a p p' =
  (sort $ defragActives $ queryArc (stripMetadata p) a)
  `shouldBe`
  (sort $ defragActives $ queryArc (stripMetadata p') a)

-- | Like @compareP@, but for control patterns, with some tolerance for floating point error
compareTol :: Arc -> ControlSignal -> ControlSignal -> Bool
compareTol a p p' = (sort $ queryArc (stripMetadata p) a) ~== (sort $ queryArc (stripMetadata p') a)

-- | Utility to create a pattern from a String
ps :: String -> Signal String
ps = parseBP_E

stripMetadata :: Signal a -> Signal a
stripMetadata = setMetadata $ Metadata []

toEvent :: (((Time, Time), (Time, Time)), a) -> Event a
toEvent (((ws, we), (ps, pe)), v) = Event (Metadata []) (Just $ Arc ws we) (Arc ps pe) v

stripSequenceMetadata :: Sequence a -> Sequence a
stripSequenceMetadata = withAtom f
  where f (Atom _ d i o v) = Atom mempty d i o v
        f x                = x

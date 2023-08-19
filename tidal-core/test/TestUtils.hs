{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module TestUtils where

import           Data.List       (sort)
import qualified Data.Map.Strict as Map
import           Prelude         hiding ((*>), (<*))
import           Sound.Tidal
import           Test.Microspec

class TolerantEq a where
   (~==) :: a -> a -> Bool

instance TolerantEq Double where
  a ~== b = abs (a - b) < 0.000001

instance TolerantEq Value where
         (VS a) ~== (VS b) = a == b
         (VI a) ~== (VI b) = a == b
         (VR a) ~== (VR b) = a == b
         (VF a) ~== (VF b) = abs (a - b) < 0.000001
         _ ~== _           = False

instance TolerantEq a => TolerantEq [a] where
  as ~== bs = (length as == length bs) && all (uncurry (~==)) (zip as bs)

instance TolerantEq ValueMap where
  a ~== b = Map.differenceWith (\a' b' -> if a' ~== b' then Nothing else Just a') a b == Map.empty

instance TolerantEq (Event ValueMap) where
  (Event _ w p x) ~== (Event _ w' p' x') = w == w' && p == p' && x ~== x'

-- | Compare the events of two patterns using the given span
compareP :: (Ord a, Show a) => Span -> Signal a -> Signal a -> Property
compareP a p p' =
  (sort $ querySpan (stripMetadata p) a)
  `shouldBe`
  (sort $ querySpan (stripMetadata p') a)

-- | Like @compareP@, but tries to 'defragment' the events
comparePD :: (Ord a, Show a) => Span -> Signal a -> Signal a -> Property
comparePD a p p' =
  (sort $ defragActives $ querySpan (stripMetadata p) a)
  `shouldBe`
  (sort $ defragActives $ querySpan (stripMetadata p') a)

-- | Like @compareP@, but for control patterns, with some tolerance for floating point error
compareTol :: Span -> Signal ValueMap -> Signal ValueMap -> Bool
compareTol a p p' = (sort $ querySpan (stripMetadata p) a) ~== (sort $ querySpan (stripMetadata p') a)

-- | Utility to create a pattern from a String
stringPat :: String -> Signal String
stringPat = parseBP_E

toEvent :: (((Time, Time), (Time, Time)), a) -> Event a
toEvent (((ws, we), (ps, pe)), v) = Event mempty (Just $ Span ws we) (Span ps pe) v

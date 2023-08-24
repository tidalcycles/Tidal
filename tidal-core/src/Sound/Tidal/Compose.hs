{-# LANGUAGE FlexibleInstances #-}

-- (c) Alex McLean and contributors 2023
-- Shared under the terms of the GNU Public License v3.0

module Sound.Tidal.Compose where

import qualified Data.Map.Strict     as Map
import           Sound.Tidal.Pattern (flexBind)
import           Sound.Tidal.Types

-- ************************************************************ --
-- Hack to allow 'union' to be used on any value

-- class for types that support a left-biased union
class Unionable a where
  union :: a -> a -> a

-- default union is just to take the left hand side..
instance Unionable a where
  union = const

instance {-# OVERLAPPING #-} Unionable ValueMap where
  union = Map.union

liftP2 :: Pattern p => (a -> b -> c) -> (p a -> p b -> p c)
liftP2 op apat bpat = apat `flexBind` \a -> op a <$> bpat

(#) :: (Pattern p, Unionable a) => p a -> p a -> p a
(#) = liftA2 union

{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE MonoLocalBinds      #-}

-- (c) Alex McLean and contributors 2023
-- Shared under the terms of the GNU Public License v3.0

module Sound.Tidal.Compose where

import           Control.Applicative (Applicative (..))
import           Data.Bits
import           Data.Bool           (bool)
import qualified Data.Map.Strict     as Map
import           Prelude             hiding (Applicative (..))
import           Sound.Tidal.Pattern (filterJusts, flexBind)
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

set, keep :: Pattern p => p a -> p a -> p a
set = liftA2 (flip union)
keep = liftA2 union

keepif :: Pattern p => p a -> p Bool -> p a
keepif pata patb = filterJusts $ liftA2 (\a b -> bool Nothing (Just a) b) pata patb

_add, _sub, _mul :: (Pattern p, Num a) => p a -> p a -> p a
_add = liftA2 (Prelude.+)
_sub = liftA2 (Prelude.-)
_mul = liftA2 (Prelude.*)

_div :: (Pattern p, Fractional a) => p a -> p a -> p a
_div = liftA2 (Prelude./)

_mod, _pow :: (Pattern p, Integral a) => p a -> p a -> p a
_mod = liftA2 mod
_pow = liftA2 (Prelude.^)

_powf :: (Pattern p, Floating a) => p a -> p a -> p a
_powf = liftA2 (Prelude.**)

_concat :: Pattern p => p String -> p String -> p String
_concat = liftA2 (Prelude.++)

_band, _bor, _bxor :: (Pattern p, Bits a) => p a -> p a -> p a
_band = liftA2 (.&.)
_bor = liftA2 (.|.)
_bxor = liftA2 (.^.)

_bshiftl, _bshiftr :: (Pattern p, Bits a) => p a -> p Int -> p a
_bshiftl = liftA2 (.<<.)
_bshiftr = liftA2 (.>>.)

_lt, _gt, _lte, _gte :: (Pattern p, Ord a) => p a -> p a -> p Bool
_lt = liftA2 (Prelude.<)
_gt = liftA2 (Prelude.>)
_lte = liftA2 (Prelude.<=)
_gte = liftA2 (Prelude.>=)

_eq, _ne :: (Pattern p, Eq a) => p a -> p a -> p Bool
_eq = liftA2 (Prelude.==)
_ne = liftA2 (Prelude./=)

_and, _or :: Pattern p => p Bool -> p Bool -> p Bool
_and = liftA2 (Prelude.&&)
_or = liftA2 (Prelude.||)

(#) :: (Pattern p, Unionable a) => p a -> p a -> p a
(#) = liftA2 union

(|=|), (|=), (=|) :: Pattern p => p a -> p a -> p a
a |=| b = (mix a) # b
a |= b = (inner a) # b
a =| b = (outer a) # b

(|+), (+|), (|+|) :: (Num (p a), Pattern p) => p a -> p a -> p a
a |+ b = (inner a) + b
a +| b = (outer a) + b
a |+| b = (mix a) + b

struct :: (Pattern p, Unionable a) => p Bool -> p a -> p a
struct patbool pat = (outer pat) `keepif` patbool

structAll :: (Pattern p, Unionable a) => p a -> p a -> p a
structAll pata patb = (outer patb) `keep` pata

mask :: (Pattern p, Unionable a) => p Bool -> p a -> p a
mask patbool pat = (inner pat) `keepif` patbool

maskAll :: (Pattern p, Unionable a) => p a -> p a -> p a
maskAll pata patb = (inner patb) `keep` pata

{-
reset :: (Unionable a) => Signal Bool -> Signal a -> Signal a
reset = flip keepifTrig

resetAll :: (Unionable a) => Signal a -> Signal a -> Signal a
resetAll = flip keepTrig

restart :: (Unionable a) => Signal Bool -> Signal a -> Signal a
restart = flip keepifTrigzero

restartAll :: (Unionable a) => Signal a -> Signal a -> Signal a
restartAll = flip keepTrigzero
-}

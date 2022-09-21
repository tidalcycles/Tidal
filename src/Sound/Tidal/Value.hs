{-# LANGUAGE DeriveFunctor #-} 
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- (c) Alex McLean 2022
-- Shared under the terms of the GNU Public License v. 3.0

module Sound.Tidal.Value where 

import Data.Ratio
import qualified Data.Map.Strict as Map
import           GHC.Generics
import           Data.Typeable (Typeable)
import           Data.Data (Data) -- toConstr

-- | Note is Double, but with a different parser
newtype Note = Note { unNote :: Double }
  deriving (Typeable, Data, Generic, Eq, Ord, Enum, Num, Fractional, Floating, Real, RealFrac)

instance Show Note where
  show n = (show . unNote $ n) ++ "n (" ++ pitchClass ++ octave ++ ")"
    where
      pitchClass = pcs !! mod noteInt 12
      octave = show $ div noteInt 12 + 5
      noteInt = round . unNote $ n
      pcs = ["c", "cs", "d", "ds", "e", "f", "fs", "g", "gs", "a", "as", "b"]

data Value = S String
           | F Double
           | I Int
           | R Rational
           | N Note
       deriving (Show)

type ValueMap = (Map.Map String Value)

-- | Apply one of three functions to a Value, depending on its type
applyFIS :: (Double -> Double) -> (Int -> Int) -> (String -> String) -> Value -> Value
applyFIRS f _ _ _ (F f') = F (f f')
applyFIRS f _ _ _ (N (Note f')) = N (Note $ f f')
applyFIRS _ f _ _ (I i) = I (f i)
applyFIRS _ _ f _ (R i) = R (f i)
applyFIRS _ _ _ f (S s) = S (f s)
-- applyFIS f f' f'' (VState x) = VState $ \cmap -> (applyFIS f f' f'') <$> (x cmap)
applyFIS _ _ _ v = v

-- | Apply one of two functions to a pair of Values, depending on
-- their types (int or float; strings and rationals are ignored)
fNum2 :: (Int -> Int -> Int) -> (Double -> Double -> Double) -> Value -> Value -> Value
fNum2 fInt _      (I a) (I b) = I (fInt a b)
fNum2 _    fFloat (F a) (F b) = F (fFloat a b)
fNum2 _    fFloat (N (Note a)) (N (Note b)) = N (Note $ fFloat a b)
fNum2 _    fFloat (F a) (N (Note b)) = N (Note $ fFloat a b)
fNum2 _    fFloat (N (Note a)) (F b) = N (Note $ fFloat a b)
fNum2 _    fFloat (I a) (F b) = F (fFloat (fromIntegral a) b)
fNum2 _    fFloat (F a) (I b) = F (fFloat a (fromIntegral b))
-- fNum2 fInt fFloat (VState a) b = VState $ \cmap -> ((\a' -> fNum2 fInt fFloat a' b) <$> (a cmap))
-- fNum2 fInt fFloat a (VState b) = VState $ \cmap -> ((\b' -> fNum2 fInt fFloat a b') <$> (b cmap))
fNum2 _    _      x      _      = x

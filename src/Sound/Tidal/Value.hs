{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Sound.Tidal.Value where

{-
    Value.hs - Polymorphic values
    Copyright (C) 2020, Alex McLean and contributors

    This library is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this library.  If not, see <http://www.gnu.org/licenses/>.
-}

import           Control.DeepSeq (NFData)
import           Data.Word (Word8)
import           GHC.Generics
import           Data.Data (Data) -- toConstr
import           Data.Typeable (Typeable)
import qualified Data.Map.Strict as Map

-- | Polymorphic values

data Value = VS { svalue :: String   }
           | VF { fvalue :: Double   }
           | VN { nvalue :: Note     }
           | VR { rvalue :: Rational }
           | VI { ivalue :: Int      }
           | VB { bvalue :: Bool     }
           | VX { xvalue :: [Word8]  } -- Used for OSC 'blobs'
           deriving (Typeable, Data, Generic)

class Valuable a where
  toValue :: a -> Value
instance NFData Value

type ValueMap = Map.Map String Value

-- | Note is Double, but with a different parser
newtype Note = Note { unNote :: Double } deriving (Typeable, Data, Generic, Eq, Ord, Show, Enum, Num, Fractional, Floating, Real)
instance NFData Note

instance Valuable String where
  toValue a = VS a
instance Valuable Double where
  toValue a = VF a
instance Valuable Rational where
  toValue a = VR a
instance Valuable Int where
  toValue a = VI a
instance Valuable Bool where
  toValue a = VB a
instance Valuable [Word8] where
  toValue a = VX a

instance Eq Value where
  (VS x) == (VS y) = x == y
  (VB x) == (VB y) = x == y
  (VF x) == (VF y) = x == y
  (VI x) == (VI y) = x == y
  (VN x) == (VN y) = x == y
  (VR x) == (VR y) = x == y
  (VX x) == (VX y) = x == y
  
  (VF x) == (VI y) = x == fromIntegral y
  (VI y) == (VF x) = x == fromIntegral y

  (VF x) == (VR y) = toRational x == y
  (VR y) == (VF x) = toRational x == y
  (VI x) == (VR y) = toRational x == y
  (VR y) == (VI x) = toRational x == y

  _ == _ = False
  
instance Ord Value where
  compare (VS x) (VS y) = compare x y
  compare (VB x) (VB y) = compare x y
  compare (VF x) (VF y) = compare x y
  compare (VN x) (VN y) = compare (unNote x) (unNote y)
  compare (VI x) (VI y) = compare x y
  compare (VR x) (VR y) = compare x y
  compare (VX x) (VX y) = compare x y

  compare (VS _) _ = LT
  compare _ (VS _) = GT
  compare (VB _) _ = LT
  compare _ (VB _) = GT
  compare (VX _) _ = LT
  compare _ (VX _) = GT

  compare (VF x) (VI y) = compare x (fromIntegral y)
  compare (VI x) (VF y) = compare (fromIntegral x) y

  compare (VR x) (VI y) = compare x (fromIntegral y)
  compare (VI x) (VR y) = compare (fromIntegral x) y

  compare (VF x) (VR y) = compare x (fromRational y)
  compare (VR x) (VF y) = compare (fromRational x) y

  compare (VN x) (VI y) = compare x (fromIntegral y)
  compare (VI x) (VN y) = compare (fromIntegral x) y

  compare (VN x) (VR y) = compare (unNote x) (fromRational y)
  compare (VR x) (VN y) = compare (fromRational x) (unNote y)

  compare (VF x) (VN y) = compare x (unNote y)
  compare (VN x) (VF y) = compare (unNote x) y

-- | General utilities..

-- | Apply one of three functions to a Value, depending on its type
applyFIS :: (Double -> Double) -> (Int -> Int) -> (String -> String) -> Value -> Value
applyFIS f _ _ (VF f') = VF (f f')
applyFIS f _ _ (VN (Note f')) = VN (Note $ f f')
applyFIS _ f _ (VI i) = VI (f i)
applyFIS _ _ f (VS s) = VS (f s)
applyFIS _ _ _ v = v

-- | Apply one of two functions to a pair of Values, depending on their types (int
-- or float; strings and rationals are ignored)
fNum2 :: (Int -> Int -> Int) -> (Double -> Double -> Double) -> Value -> Value -> Value
fNum2 fInt _      (VI a) (VI b) = VI (fInt a b)
fNum2 _    fFloat (VF a) (VF b) = VF (fFloat a b)
fNum2 _    fFloat (VN (Note a)) (VN (Note b)) = VN (Note $ fFloat a b)
fNum2 _    fFloat (VI a) (VF b) = VF (fFloat (fromIntegral a) b)
fNum2 _    fFloat (VF a) (VI b) = VF (fFloat a (fromIntegral b))
fNum2 _    _      x      _      = x

getI :: Value -> Maybe Int
getI (VI i) = Just i
getI (VR x) = Just $ floor x
getI (VF x) = Just $ floor x
getI _  = Nothing

getF :: Value -> Maybe Double
getF (VF f) = Just f
getF (VR x) = Just $ fromRational x
getF (VI x) = Just $ fromIntegral x
getF _  = Nothing

getN :: Value -> Maybe Note
getN (VF f) = Just $ Note f
getN (VR x) = Just $ Note $ fromRational x
getN (VI x) = Just $ Note $ fromIntegral x
getN _  = Nothing

getS :: Value -> Maybe String
getS (VS s) = Just s
getS _  = Nothing

getB :: Value -> Maybe Bool
getB (VB b) = Just b
getB _  = Nothing

getR :: Value -> Maybe Rational
getR (VR r) = Just r
getR (VF x) = Just $ toRational x
getR (VI x) = Just $ toRational x
getR _  = Nothing

getBlob :: Value -> Maybe [Word8]
getBlob (VX xs) = Just xs
getBlob _  = Nothing


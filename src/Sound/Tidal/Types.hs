{-# LANGUAGE RecordWildCards, DeriveFunctor, DeriveDataTypeable, DeriveGeneric, GeneralizedNewtypeDeriving #-}

-- (c) Alex McLean and contributors 2022
-- Shared under the terms of the GNU Public License v3.0

module Sound.Tidal.Types where

import           GHC.Generics

import qualified Data.Map.Strict as Map
import           Data.Typeable (Typeable)
import           Data.Data (Data) -- toConstr

import Data.List (intercalate, sortOn)
import Data.Ratio (numerator, denominator)
import Data.Maybe (fromMaybe, isJust)
import Data.Word (Word8)

-- | In Tidal, time is rational
type Time = Rational

-- | Note is an alias for Double, but with a different mininotation
-- parser
newtype Note = Note { unNote :: Double }
  deriving (Typeable, Data, Generic, Eq, Ord, Enum, Num, Fractional, Floating, Real, RealFrac)

instance Show Note where
  show n = (show . unNote $ n) ++ "n (" ++ pitchClass ++ octave ++ ")"
    where
      pitchClass = pcs !! mod noteInt 12
      octave = show $ div noteInt 12 + 5
      noteInt = round . unNote $ n
      pcs = ["c", "cs", "d", "ds", "e", "f", "fs", "g", "gs", "a", "as", "b"]

-- | An event value
data Value = VS String
           | VF Double
           | VI Int
           | VR Rational
           | VN Note
           | VB Bool
           | VSignal (Signal Value)
           | VX { xvalue :: [Word8]  } -- Used for OSC 'blobs'
           | VList {lvalue :: [Value]}
           | VState {statevalue :: ValueMap -> (ValueMap, Value)}

-- Class of types able to be turned into a Value
class Valuable a where
  toValue :: a -> Value


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

  -- you can't really compare patterns, state or lists..
  compare (VSignal _) (VSignal _) = EQ
  compare (VSignal _) _ = GT
  compare _ (VSignal _) = LT

  compare (VState _) (VState _) = EQ
  compare (VState _) _          = GT
  compare _ (VState _)          = LT

  compare (VList _) (VList _) = EQ
  compare (VList _) _          = GT
  compare _ (VList _)          = LT

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
instance Valuable Note where
  toValue a = VN a
instance Valuable [Word8] where
  toValue a = VX a
instance Valuable [Value] where
  toValue a = VList a

-- | Maps of values, used for representing synth control/trigger
-- messages, and state
type ValueMap = (Map.Map String Value)

type ControlSignal = Signal ValueMap

-- | A timearc and some named control values, used to query a signal
-- with
data State = State {sArc :: Arc,
                    sControls :: ValueMap
                   }

-- | Time arc (also known as timespan)
data Arc = Arc {aBegin :: Time, aEnd :: Time}
  deriving (Ord, Eq)

-- | Metadata - currently just used for sourcecode positions that
-- caused the event they're stored against
data Metadata = Metadata {metaSrcPos :: [((Int, Int), (Int, Int))]}
  deriving (Eq)

instance Semigroup Metadata where
  (<>) a b = Metadata (metaSrcPos a ++ metaSrcPos b)

instance Monoid Metadata where
  mempty = Metadata []

-- | An event, consisting of a value, its 'whole' timearc, and the
-- timearc that is active (called a 'part' in tidal v1)
data Event a = Event {metadata :: Metadata,
                      whole :: Maybe Arc,
                      active :: Arc,
                      value :: a
                     }
  deriving (Functor, Eq)

-- | A signal - a function from time to events. Known as a Pattern in tidal v1.
data Signal a = Signal {query :: State -> [Event a]}
  deriving (Functor)

-- | A discrete sequence
data Sequence a = Atom Time a
                | Gap Time
                | Sequence [Sequence a]
                | Stack [Sequence a]
              deriving Show

-- | Strategies for aligning two sequences
data Strategy = JustifyLeft
              | JustifyRight
              | JustifyBoth
              | Expand
              | TruncateMax
              | TruncateMin
              | RepeatLCM
              | Centre
              | Squeeze
              deriving Show

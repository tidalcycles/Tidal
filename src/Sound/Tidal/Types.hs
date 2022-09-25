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
data Value = S String
           | F Double
           | I Int
           | R Rational
           | N Note

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
data Arc = Arc {begin :: Time, end :: Time}
  deriving (Ord, Eq)

-- | Metadata - currently just used for sourcecode positions that
-- caused the event they're stored against
data Metadata = Metadata {metaSrcPos :: [((Int, Int), (Int, Int))]}

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
  deriving (Functor)

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

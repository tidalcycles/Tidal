module Sound.Tidal.Types where

import           GHC.Generics

import qualified Data.Map.Strict as Map
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

type Time = Rational

-- | A timearc and some named control values, used to query a signal
-- with
data State = State {sArc :: Arc,
                    sControls :: ValueMap
                   }
-- | Time arc (also known as timespan)
data Arc = Arc {begin :: Time, end :: Time}
  deriving (Show)

data Metadata = Metadata {metaSrcPos :: [((Int, Int), (Int, Int))]}
  deriving (Show)

instance Semigroup Metadata where
  (<>) a b = Metadata (metaSrcPos a ++ metaSrcPos b)

instance Monoid Metadata where
  mempty = Metadata []

-- | An event - a value, its 'whole' timearc, and the timearc that
-- its active (called a 'part' in tidal v1)
data Event a = Event {metadata :: Metadata,
                      whole :: Maybe Arc,
                      active :: Arc,
                      value :: a
                     }
  deriving (Show, Functor)

data Signal a = Signal {query :: State -> [Event a]}
  deriving (Functor)

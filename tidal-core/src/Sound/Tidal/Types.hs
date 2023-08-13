{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}

module Sound.Tidal.Types where

import           Control.DeepSeq (NFData)
import qualified Data.Map.Strict as Map
import           Data.Typeable   (Typeable)
import           Data.Word       (Word8)
import           GHC.Generics

-- | Note is an alias for Double, but with a different mininotation
-- parser
newtype Note = Note { unNote :: Double }
  deriving (Generic, Eq, Ord, Enum, Num,
            Fractional, Floating, Real, RealFrac, NFData)

-- | In Tidal, time is rational
type Time = Rational

-- class Test p where
--   toSeq :: p a -> Sequence (a -> a)

-- instance Test (a -> a) where
--   toSeq = pure

-- instance Test (Sequence (a -> a)) where
--   toSeq = id

-- accept both a and p a

class Applicative t => Applicable t a b where toA :: a -> t b
instance forall a t. Applicative t => Applicable t (a) (a) where toA = pure
instance forall a t. Applicative t => Applicable t (t a) (a) where toA = id

-- | A type class for patterns
class (Functor p, Applicative p, Monad p) => Pattern p where
  {-# MINIMAL (innerBind | innerJoin), (outerBind | outerJoin), (squeezeBind | squeezeJoin), duration, withTime, cat, timeCat, stack, _early, rev, toSignal, withMetadata, silence #-}
  duration :: p a -> Time
  withTime :: (Time -> Time) -> (Time -> Time) -> p a -> p a

  -- Instances must either define binds or joins
  innerBind, outerBind, squeezeBind :: p a -> (a -> p b) -> p b
  innerBind pat f = innerJoin $ fmap f pat
  outerBind pat f = outerJoin $ fmap f pat
  squeezeBind pat f = squeezeJoin $ fmap f pat

  innerJoin, outerJoin, squeezeJoin :: p (p a) -> p a
  innerJoin pat = innerBind pat id
  outerJoin pat = outerBind pat id
  squeezeJoin pat = squeezeBind pat id

  cat :: [p a] -> p a
  timeCat :: [(Time, p a)] -> p a
  stack :: [p a] -> p a
  _early :: Time -> p a -> p a
  rev :: p a -> p a
  toSignal :: p a -> Signal a
  withMetadata :: (Metadata -> Metadata) -> p a -> p a
  silence :: p a

instance Pattern p => Semigroup (p a)
  where a <> b = cat [a,b]

instance Pattern p => Monoid (p a) where
  mempty = silence

data Span = Span { aBegin :: Time, aEnd :: Time}
  deriving (Eq, Ord, Show)

-- | Metadata - currently just used for sourcecode positions that
-- caused the event they're stored against
data Metadata = Metadata {metaSrcPos   :: [((Int, Int), (Int, Int))]}
  deriving (Eq, Typeable, Generic, Ord, Show)
instance NFData Metadata

instance Semigroup Metadata where
  (<>) a b = Metadata (metaSrcPos a ++ metaSrcPos b)

instance Monoid Metadata where
  mempty = Metadata []

-- | A polymorphic event value
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
           deriving (Typeable, Generic)
instance NFData Value

-- | Maps of values, used for representing synth control/trigger
-- messages, and state
type ValueMap = (Map.Map String Value)

-- A discrete value with a whole timespan, or a continuous one without
-- It might be a fragment of an event, in which case its 'active' arc
-- will be a smaller subsection of its 'whole'.
data Event a = Event {eventMetadata :: Metadata,
                      whole         :: Maybe Span,
                      active        :: Span,
                      value         :: a
                     }
  deriving (Functor, Eq, Ord)

-- A pattern that's a function from a timespan to events active during
-- that timespan. A continuous signal, that can nonetheless contain
-- discrete events.
data Signal a = Signal {query :: Span -> [Event a]}
  deriving (Functor, Generic)

instance NFData a => NFData (Signal a)

-- A pattern as a discrete, contiguous, finite sequence, that's
-- structured to support polyphonic stacks, and embedded subsequences
data Sequence a = Atom {atomMetadata :: Metadata,
                        atomDuration :: Time,
                        -- If the atom is a fragment, the 'original'
                        -- duration that it's a fragment of is
                        -- duration + inset + outset.
                        atomInset    :: Time,
                        atomOutset   :: Time,
                        -- A 'gap' or silence is an atom without a value
                        atomValue    :: Maybe a
                       }
                | Cat [Sequence a]
                | Stack [Sequence a]
                deriving (Eq, Ord, Generic)

-- | Strategies for aligning two sequences or patterns over time (horizontally)
data Strategy = JustifyLeft
              | JustifyRight
              | JustifyBoth
              | Expand
              | TruncateLeft
              | TruncateRight
              | TruncateRepeat
              | Repeat
              | Centre
              | SqueezeIn
              | SqueezeOut
              deriving (Eq, Ord, Show)

-- | Once we've aligned two patterns, where does the structure come from?
data Direction = Inner
               | Outer
               | Mix
               deriving (Eq, Ord, Show)

class Alignment a where
  toSeqStrategy :: a b -> SeqStrategy b

-- A wrapper for a sequence that specifies how it should be combined
-- with another sequence.
data SeqStrategy a = SeqStrategy {sStrategy  :: Strategy,
                                  sDirection :: Direction,
                                  sSequence  :: Sequence a
                                 }


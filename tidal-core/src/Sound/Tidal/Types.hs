{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}

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

-- | in Tidal, time is rational
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
  {-# MINIMAL (innerBind | innerJoin),
              (outerBind | outerJoin),
              (squeezeBind | squeezeJoin),
              patBind, patAlign,
              duration, withTime, cat, timeCat, stack, _early, rev, toSignal,
              withMetadata, silence, _zoomSpan
    #-}
  duration :: p a -> Time
  withTime :: (Time -> Time) -> (Time -> Time) -> p a -> p a
  innerBind, outerBind, squeezeBind :: p a -> (a -> p b) -> p b
  innerBind pat f = innerJoin $ fmap f pat
  outerBind pat f = outerJoin $ fmap f pat
  squeezeBind pat f = squeezeJoin $ fmap f pat

  innerJoin, outerJoin, squeezeJoin :: p (p a) -> p a
  innerJoin pat = innerBind pat id
  outerJoin pat = outerBind pat id
  squeezeJoin pat = squeezeBind pat id

  patBind :: p a -> p b -> (b -> p c) -> p c
  patAlign :: p a -> p b -> (p a, p b)

  cat :: [p a] -> p a
  timeCat :: [(Time, p a)] -> p a
  stack :: [p a] -> p a
  _early :: Time -> p a -> p a
  rev :: p a -> p a
  toSignal :: p a -> Signal a
  withMetadata :: (Metadata -> Metadata) -> p a -> p a
  silence :: p a
  -- | Return part of a pattern, zoomed to the same (cycle) duration
  _zoomSpan :: Span -> p a -> p a

instance Pattern p => Semigroup (p a)
  where a <> b = cat [a,b]

instance Pattern p => Monoid (p a) where
  mempty = silence

data Span = Span { aBegin :: Time, aEnd :: Time}
  deriving (Eq, Ord)

-- | Metadata - currently just used for sourcecode positions that
-- caused the event they're stored against
data Metadata = Metadata {metaSrcPos   :: [((Int, Int), (Int, Int))]}
  deriving (Eq, Typeable, Generic, Ord, Show)
instance NFData Metadata

instance Semigroup Metadata where
  (<>) a b = Metadata (metaSrcPos a ++ metaSrcPos b)

instance Monoid Metadata where
  mempty = Metadata []

data SequenceMetadata
  = SequenceMetadata {sBindAlignment :: SeqBindAlignment}
  deriving (Eq, Ord, Generic)
instance NFData SequenceMetadata

data SignalMetadata
  = SignalMetadata {sBind :: Maybe SignalBind}
  deriving (Eq, Ord, Generic)
instance NFData SignalMetadata

instance Semigroup SignalMetadata where
  (<>) _ _ = SignalMetadata Nothing

instance Monoid SignalMetadata where
  mempty = SignalMetadata Nothing

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

  _ == _           = False

instance Ord Value where
  compare (VS x) (VS y)           = compare x y
  compare (VB x) (VB y)           = compare x y
  compare (VF x) (VF y)           = compare x y
  compare (VN x) (VN y)           = compare (unNote x) (unNote y)
  compare (VI x) (VI y)           = compare x y
  compare (VR x) (VR y)           = compare x y
  compare (VX x) (VX y)           = compare x y

  compare (VS _) _                = LT
  compare _ (VS _)                = GT
  compare (VB _) _                = LT
  compare _ (VB _)                = GT
  compare (VX _) _                = LT
  compare _ (VX _)                = GT

  compare (VF x) (VI y)           = compare x (fromIntegral y)
  compare (VI x) (VF y)           = compare (fromIntegral x) y

  compare (VR x) (VI y)           = compare x (fromIntegral y)
  compare (VI x) (VR y)           = compare (fromIntegral x) y

  compare (VF x) (VR y)           = compare x (fromRational y)
  compare (VR x) (VF y)           = compare (fromRational x) y

  compare (VN x) (VI y)           = compare x (fromIntegral y)
  compare (VI x) (VN y)           = compare (fromIntegral x) y

  compare (VN x) (VR y)           = compare (unNote x) (fromRational y)
  compare (VR x) (VN y)           = compare (fromRational x) (unNote y)

  compare (VF x) (VN y)           = compare x (unNote y)
  compare (VN x) (VF y)           = compare (unNote x) y

  -- you can't really compare patterns, state or lists..
  compare (VSignal _) (VSignal _) = EQ
  compare (VSignal _) _           = GT
  compare _ (VSignal _)           = LT

  compare (VState _) (VState _)   = EQ
  compare (VState _) _            = GT
  compare _ (VState _)            = LT

  compare (VList _) (VList _)     = EQ
  compare (VList _) _             = GT
  compare _ (VList _)             = LT

-- | Maps of values, used for representing synth control
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


-- | A timearc and some named control values, used to query a signal
-- with
data State = State {sSpan     :: Span,
                    sControls :: ValueMap
                   }
             deriving Generic

-- A pattern that's a function from a timespan (along with other
-- state) to events active during that timespan. A continuous signal,
-- that can nonetheless contain discrete events.
data Signal a = Signal {sigMetadata :: SignalMetadata,
                        query       :: State -> [Event a]
                       }
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
                | SeqMetadata {seqBindAlignment :: SeqBindAlignment,
                               seqData          :: Sequence a
                              }
                deriving (Eq, Ord, Generic)

-- | Strategies for aligning two sequences or patterns over time (horizontally)
data Alignment = JustifyLeft
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
               deriving (Eq, Ord, Show, Generic)
instance NFData Alignment

-- | Once we've aligned two patterns, where does the structure come from?
data SequenceBind = SeqIn
                  | SeqOut
                  | SeqMix
                  deriving (Eq, Ord, Show, Generic)
instance NFData SequenceBind

data SignalBind = SigIn
                | SigOut
                | SigMix
                -- Signals allow some alignment at bind time
                | SigSqueeze
                | SigTrig
                | SigTrigzero
  deriving (Eq, Ord, Show, Generic)
instance NFData SignalBind

data SeqBindAlignment = SeqBindAlignment {seqAlignment :: Alignment,
                                          seqBind      :: SequenceBind
                                         }
  deriving (Eq, Ord, Show, Generic)
instance NFData SeqBindAlignment

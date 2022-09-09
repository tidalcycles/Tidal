{-# LANGUAGE DeriveFunctor #-}

module Sound.Tidal.Event where

import Data.Maybe (fromJust)
import Data.Monoid

import Sound.Tidal.Span

-- ************************************************************ --
-- Event

-- | Event metadata, currently just a list of source code position
-- ranges that an event is tagged with
data Metadata = Metadata {metaSrcPos :: [((Int, Int), (Int, Int))]}
  deriving (Show)

instance Semigroup Metadata where
  (<>) a b = Metadata (metaSrcPos a ++ metaSrcPos b)

instance Monoid Metadata where
  mempty = Metadata []

-- | An event - a value, its 'whole' timespan, and the timespan that
-- its active (called a 'part' in tidal v1)
data Event a = Event {metadata :: Metadata,
                      whole :: Maybe Span,
                      active :: Span,
                      value :: a
                     }
  deriving (Show, Functor)

isAnalog :: Event a -> Bool
isAnalog (Event {whole = Nothing}) = True
isAnalog _ = False

isDigital :: Event a -> Bool
isDigital = not . isAnalog

-- | Returns true only if an event starts within given timespan
onsetIn :: Span -> Event a -> Bool
onsetIn a e = isIn a (wholeBegin e)

wholeOrActive :: Event a -> Span
wholeOrActive (Event {whole = Just a}) = a
wholeOrActive e = active e

-- | Get the onset of an event's 'whole'
wholeBegin :: Event a -> Time
wholeBegin = begin . wholeOrActive

-- | Get the offset of an event's 'whole'
wholeEnd :: Event a -> Time
wholeEnd = end . wholeOrActive

-- | Get the onset of an event's 'whole'
eventActiveBegin :: Event a -> Time
eventActiveBegin = begin . active

-- | Get the offset of an event's 'active'
eventActiveEnd :: Event a -> Time
eventActiveEnd = end . active

-- | Get the timespan of an event's 'active'
eventActive :: Event a -> Span
eventActive = active

eventValue :: Event a -> a
eventValue = value

eventHasOnset :: Event a -> Bool
eventHasOnset e | isAnalog e = False
                | otherwise = begin (fromJust $ whole e) == begin (active e)

withSpan :: (Span -> Span) -> Event a -> Event a
withSpan f e = e {active = f $ active e,
                  whole  = f <$> whole e
                 }

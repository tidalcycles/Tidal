{-# LANGUAGE DeriveFunctor #-}

module Sound.Tidal2.Event where

import Data.Maybe (fromJust)

import Sound.Tidal2.Span

-- ************************************************************ --
-- Event

-- | Event metadata, currently just a list of source code position
-- ranges that an event is tagged with
data Metadata = Metadata {metaSrcPos :: [((Int, Int), (Int, Int))]}
  deriving (Show)

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
onsetIn a e = isIn a (wholeStart e)

wholeOrActive :: Event a -> Span
wholeOrActive (Event {whole = Just a}) = a
wholeOrActive e = active e

-- | Get the onset of an event's 'whole'
wholeStart :: Event a -> Time
wholeStart = start . wholeOrActive

-- | Get the offset of an event's 'whole'
wholeStop :: Event a -> Time
wholeStop = stop . wholeOrActive

-- | Get the onset of an event's 'whole'
eventActiveStart :: Event a -> Time
eventActiveStart = start . active

-- | Get the offset of an event's 'active'
eventActiveStop :: Event a -> Time
eventActiveStop = stop . active

-- | Get the timespan of an event's 'active'
eventActive :: Event a -> Span
eventActive = active

eventValue :: Event a -> a
eventValue = value

eventHasOnset :: Event a -> Bool
eventHasOnset e | isAnalog e = False
                | otherwise = start (fromJust $ whole e) == start (active e)

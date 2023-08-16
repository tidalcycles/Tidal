module Sound.Tidal.Event where

import           Data.Maybe        (fromJust)
import           Sound.Tidal.Types

isAnalog :: Event a -> Bool
isAnalog Event {whole = Nothing} = True
isAnalog _                       = False

isDigital :: Event a -> Bool
isDigital = not . isAnalog

eventWithSpan :: (Span -> Span) -> Event a -> Event a
eventWithSpan f e = e {active = f $ active e,
                       whole  = f <$> whole e
                      }

wholeOrActive :: Event a -> Span
wholeOrActive Event {whole = Just a} = a
wholeOrActive e                      = active e

eventHasOnset :: Event a -> Bool
eventHasOnset e | isAnalog e = False
                | otherwise = aBegin (fromJust $ whole e) == aBegin (active e)

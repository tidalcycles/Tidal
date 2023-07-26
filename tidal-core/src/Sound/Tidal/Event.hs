module Sound.Tidal.Event where

import           Sound.Tidal.Types

instance (Show a) => Show (Event a) where
  show e = show (whole e) ++ " " ++ show (active e) ++ " " ++ show (value e)

eventWithSpan :: (Span -> Span) -> Event a -> Event a
eventWithSpan f e = e {active = f $ active e,
                       whole  = f <$> whole e
                      }

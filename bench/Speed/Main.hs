module Main where

import           Criterion.Main (defaultMain)
import           Tidal.CoreB
import           Tidal.PatternB
import           Tidal.UIB

patternBs :: [IO ()]
patternBs = defaultMain <$> [ withQueryTimeB
                            , withEventArcB
                            , withQueryArcB
                            , maybeSectB
                            , [sectB]
                            , [hullB]
                            ]

coreBs :: [IO ()] 
coreBs = defaultMain <$> [fromListB, stackB, appendB, concatB, _fastB]

uiBs :: [IO ()]
uiBs = defaultMain <$> [euclidB, fixB]

main :: IO ()
main = do 
  sequence_ coreBs
  sequence_ patternBs
  sequence_ uiBs

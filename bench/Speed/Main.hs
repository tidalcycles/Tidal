module Main where

import Criterion.Main 
import Tidal.PatternB
import Tidal.CoreB
import Tidal.UIB

patternBs :: [IO ()] 
patternBs = defaultMain <$> [withQueryTimeB, withQueryArcB, withResultArcB, withQueryTimeB, subArcB]

coreBs :: [IO ()] 
coreBs = defaultMain <$> [fromListB, stackB, appendB, concatB, _fastB]

uiBs :: [IO ()]
uiBs = defaultMain <$> [euclidB, fixB]

main :: IO ()
main = do 
  _ <- sequence coreBs 
  _ <- sequence patternBs
  _ <- sequence uiBs
  return ()

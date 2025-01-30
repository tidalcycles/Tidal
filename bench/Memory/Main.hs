module Main where

import Tidal.UIB
import Weigh

main :: IO ()
main =
  mainWith $ do
    euclidB
    fixB

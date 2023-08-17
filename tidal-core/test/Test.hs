module Main (main) where

import           Sound.Tidal.SpanTest
import           Test.Microspec

main :: IO ()
main = microspec $ do
  Sound.Tidal.SpanTest.run


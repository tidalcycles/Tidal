module Main (main) where

import           Sound.Tidal.ChordsTest
import           Sound.Tidal.EventTest
import           Sound.Tidal.SpanTest
import           Test.Microspec

main :: IO ()
main = microspec $ do
  Sound.Tidal.ChordsTest.run
  Sound.Tidal.EventTest.run
  Sound.Tidal.SpanTest.run

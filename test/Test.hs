{-# LANGUAGE OverloadedStrings #-}

import Sound.Tidal.StreamTest
import Test.Microspec

main :: IO ()
main = microspec $ do
  Sound.Tidal.StreamTest.run

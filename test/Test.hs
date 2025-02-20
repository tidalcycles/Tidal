{-# LANGUAGE OverloadedStrings #-}

import Sound.Tidal.StreamTest
import Test.Hspec

main :: IO ()
main = hspec $ do
  Sound.Tidal.StreamTest.run

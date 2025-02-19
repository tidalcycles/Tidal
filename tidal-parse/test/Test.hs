{-# LANGUAGE OverloadedStrings #-}

import Sound.Tidal.TidalParseTest
import Test.Hspec

main :: IO ()
main = hspec $ do
  Sound.Tidal.TidalParseTest.run

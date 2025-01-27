{-# LANGUAGE OverloadedStrings #-}

import Sound.Tidal.TidalParseTest
import Test.Microspec

main :: IO ()
main = microspec $ do
  Sound.Tidal.TidalParseTest.run

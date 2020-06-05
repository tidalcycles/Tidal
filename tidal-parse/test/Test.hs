{-# LANGUAGE OverloadedStrings #-}

import Test.Microspec

import Sound.Tidal.TidalParseTest

main :: IO ()
main = microspec $ do
  Sound.Tidal.TidalParseTest.run

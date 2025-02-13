{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit
import Sound.Tidal.TidalParseFFITest (testFullPattern)

main :: IO Counts
main = runTestTT $ TestList [testFullPattern]

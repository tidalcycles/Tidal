{-# LANGUAGE OverloadedStrings #-}

import Test.Microspec

import Sound.Tidal.CoreTest
import Sound.Tidal.MiniTidalTest
import Sound.Tidal.ParseTest
import Sound.Tidal.PatternTest
import Sound.Tidal.ControlTest
import Sound.Tidal.ScalesTest
import Sound.Tidal.UITest
import Sound.Tidal.UtilsTest

main :: IO ()
main = microspec $ do
  Sound.Tidal.CoreTest.run
  Sound.Tidal.MiniTidalTest.run
  Sound.Tidal.ParseTest.run
  Sound.Tidal.PatternTest.run
  Sound.Tidal.ControlTest.run
  Sound.Tidal.ScalesTest.run
  Sound.Tidal.UITest.run
  Sound.Tidal.UtilsTest.run

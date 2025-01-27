{-# LANGUAGE OverloadedStrings #-}

import Sound.Tidal.ChordsTest
import Sound.Tidal.ControlTest
import Sound.Tidal.CoreTest
import Sound.Tidal.ExceptionsTest
import Sound.Tidal.ParamsTest
import Sound.Tidal.ParseTest
import Sound.Tidal.PatternTest
import Sound.Tidal.ScalesTest
import Sound.Tidal.StreamTest
import Sound.Tidal.UITest
import Sound.Tidal.UtilsTest
import Test.Microspec

main :: IO ()
main = microspec $ do
  Sound.Tidal.CoreTest.run
  Sound.Tidal.ParseTest.run
  Sound.Tidal.ParamsTest.run
  Sound.Tidal.PatternTest.run
  Sound.Tidal.ControlTest.run
  Sound.Tidal.ScalesTest.run
  Sound.Tidal.ChordsTest.run
  Sound.Tidal.StreamTest.run
  Sound.Tidal.UITest.run
  Sound.Tidal.UtilsTest.run
  Sound.Tidal.ExceptionsTest.run

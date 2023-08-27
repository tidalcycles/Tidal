{-# LANGUAGE OverloadedStrings #-}

import           Test.Microspec

-- import Sound.Tidal.CoreTest
-- import Sound.Tidal.ParamsTest
-- import Sound.Tidal.ParseTest
-- import Sound.Tidal.ControlTest
-- import Sound.Tidal.ChordsTest
-- import Sound.Tidal.StreamTest
-- import Sound.Tidal.UITest
-- import Sound.Tidal.ExceptionsTest
import           Sound.Tidal.ArcTest
import           Sound.Tidal.ChordsTest
import           Sound.Tidal.EventTest
import           Sound.Tidal.ExceptionsTest
import           Sound.Tidal.ParamsTest
import           Sound.Tidal.ParseBPTest
import           Sound.Tidal.PatternTest
import           Sound.Tidal.ScalesTest
import           Sound.Tidal.SequenceTest
import           Sound.Tidal.SignalBaseTest
import           Sound.Tidal.SignalComposeTest
import           Sound.Tidal.SignalControlTest
import           Sound.Tidal.SignalRandomTest
import           Sound.Tidal.StreamTest
import           Sound.Tidal.TimeTest
import           Sound.Tidal.TypesTest
import           Sound.Tidal.UtilsTest
import           Sound.Tidal.ValueTest
import           Sound.Tidal.WaveformTest

main :: IO ()
main = microspec $ do
--  Sound.Tidal.CoreTest.run
--  Sound.Tidal.ParseTest.run
--  Sound.Tidal.ParamsTest.run
--  Sound.Tidal.ControlTest.run
--  Sound.Tidal.ChordsTest.run
--  Sound.Tidal.StreamTest.run
--  Sound.Tidal.UITest.run
--  Sound.Tidal.ExceptionsTest.run
  Sound.Tidal.ArcTest.run
  Sound.Tidal.ChordsTest.run
  Sound.Tidal.EventTest.run
  Sound.Tidal.ExceptionsTest.run
  Sound.Tidal.ParamsTest.run
  Sound.Tidal.ParseBPTest.run
  Sound.Tidal.PatternTest.run
  Sound.Tidal.ScalesTest.run
  Sound.Tidal.SequenceTest.run
  Sound.Tidal.SignalBaseTest.run
  Sound.Tidal.SignalComposeTest.run
  Sound.Tidal.SignalControlTest.run
  Sound.Tidal.SignalRandomTest.run
  Sound.Tidal.StreamTest.run
  Sound.Tidal.TimeTest.run
  Sound.Tidal.TypesTest.run
  Sound.Tidal.UtilsTest.run
  Sound.Tidal.ValueTest.run
  Sound.Tidal.WaveformTest.run

module Main (main) where

import           Test.Microspec

import           Sound.Tidal.BjorklundTest
import           Sound.Tidal.ChordsTest
import           Sound.Tidal.EventTest
import           Sound.Tidal.ExceptionsTest
import           Sound.Tidal.MininotationTest
import           Sound.Tidal.PatternTest
import           Sound.Tidal.ScalesTest
import           Sound.Tidal.SequenceTest
import           Sound.Tidal.SignalTest
-- import           Sound.Tidal.SignalComposeTest
-- import           Sound.Tidal.SignalControlTest
-- import           Sound.Tidal.SignalRandomTest
-- import           Sound.Tidal.SpanTest
-- import           Sound.Tidal.TimeTest
-- import           Sound.Tidal.TypesTest
-- import           Sound.Tidal.UtilsTest
-- import           Sound.Tidal.ValueTest
-- import           Sound.Tidal.WaveformTest

main :: IO ()
main = microspec $ do
  Sound.Tidal.BjorklundTest.run
  Sound.Tidal.ChordsTest.run
  Sound.Tidal.EventTest.run
  Sound.Tidal.ExceptionsTest.run
  Sound.Tidal.MininotationTest.run
  Sound.Tidal.PatternTest.run
  Sound.Tidal.ScalesTest.run
  Sound.Tidal.SequenceTest.run
  Sound.Tidal.SignalTest.run
  -- Sound.Tidal.SignalComposeTest.run
  -- Sound.Tidal.SignalControlTest.run
  -- Sound.Tidal.SignalRandomTest.run
  -- Sound.Tidal.SpanTest.run
  -- Sound.Tidal.TimeTest.run
  -- Sound.Tidal.TypesTest.run
  -- Sound.Tidal.UtilsTest.run
  -- Sound.Tidal.ValueTest.run
  -- Sound.Tidal.WaveformTest.run

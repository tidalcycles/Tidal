module Sound.Tidal.SerialStream where

import Data.List
import Data.Maybe
import qualified Data.Map.Strict as Map

import qualified Data.ByteString.Char8 as B
import qualified System.Hardware.Serialport as Serial

import Sound.Tidal.Time
import Sound.Tidal.Stream
import Sound.Tidal.Transition
import Sound.Tidal.Pattern
import Sound.Tidal.Params

type SerialMap = Map.Map Param (Maybe String)

toSerialString :: Value -> Maybe String
toSerialString (VF x) = Just $ show x
toSerialString (VI x) = Just $ show x
toSerialString (VS x) = Just $ x

toSerialMap :: ParamMap -> SerialMap
toSerialMap m = Map.map (toSerialString) (Map.mapMaybe (id) m)

send' s content = do
  Serial.send s $ B.pack $ content ++ "\n"
  return ()


send s shape change tick (o, m) = msg
  where
    msg = doAt logicalOnset $ send' s params''
    -- get the first value of the first param for now
    params'' = case length params' of
      0 -> ""
      _ -> head $ params'
    params' = catMaybes $ map snd $ Map.toList m
    logicalOnset = logicalOnset' change tick o ((latency shape))
    -- nudge = maybe 0 (toF) (Map.lookup (F "nudge" (Just 0)) m)
    -- toF (Just (VF f)) = f
    -- toF _ = 0



makeConnection :: String -> IO (ToMessageFunc)
makeConnection device = do
  s <- Serial.openSerial device Serial.defaultSerialSettings
  return (\ shape change tick (o, m) -> do
             m' <- fmap (toSerialMap) (applyShape' shape m)
             return $ send s shape change tick (o, m')
         )
serialBackend n = do
  s <- makeConnection n
  return $ Backend s

blinkenStream n = do
  backend <- serialBackend n
  stream backend blinken

blinkenState n = do
  backend <- serialBackend n
  state backend blinken

blinkenSetters :: String -> IO Time -> IO (ParamPattern -> IO (), (Time -> [ParamPattern] -> ParamPattern) -> ParamPattern -> IO ())
blinkenSetters n getNow = do
  ds <- blinkenState n
  return (setter ds, transition getNow ds)

light :: Pattern String -> ParamPattern
light = make' VS light_p
light_p = S "light" Nothing

blinken = Shape {
  params = [
     light_p
     ],
  cpsStamp = True,
  latency = 0.1
  }

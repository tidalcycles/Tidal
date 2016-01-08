module Sound.Tidal.SerialStream (
  serialDevices,
  serialBackend,
  blinken,
  blinkenStream,
  blinkenState,
  blinkenSetters,
  light) where

import Data.List
import Data.Maybe
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Char8 as B

import Control.Exception
import Control.Concurrent.MVar
import qualified System.Hardware.Serialport as Serial

import Sound.Tidal.Time
import Sound.Tidal.Stream
import Sound.Tidal.Transition
import Sound.Tidal.Pattern
import Sound.Tidal.Params

type SerialMap = Map.Map Param (Maybe String)
type SerialDeviceMap = Map.Map String Serial.SerialPort

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
    logicalOnset = logicalOnset' change tick o ((latency shape) + nudge)
    nudge = maybe 0 (toF) (Map.lookup (F "nudge" (Just 0)) m)
    toF (Just s) = read s
    toF _ = 0


useOutput outsM name = do
  outs <- readMVar outsM
  let outM = Map.lookup name outs
  case outM of
    Just o -> do
      putStrLn "Cached Serial Device output"
      return $ Just o
    Nothing -> do
      o <- Serial.openSerial name Serial.defaultSerialSettings { Serial.commSpeed = Serial.CS115200 }
      swapMVar outsM $ Map.insert name o outs
      return $ Just o

makeConnection :: MVar (SerialDeviceMap) -> String -> IO (ToMessageFunc)
makeConnection devices device = do
  moutput <- useOutput devices device
  case moutput of
    Just s ->
      return $ (\ shape change tick (o, m) -> do
                   m' <- fmap (toSerialMap) (applyShape' shape m)
                   return $ send s shape change tick (o, m')
               )

    Nothing ->
      error ("Failed connecting to serial device: '" ++ device ++  "'")


serialDevices :: IO (MVar (SerialDeviceMap))
serialDevices = do
  d <- newMVar $ Map.fromList []
  return d

serialBackend d n = do
  s <- makeConnection d n
  return $ Backend s

blinkenStream d n = do
  backend <- serialBackend d n
  stream backend blinken

blinkenState d n = do
  backend <- serialBackend d n
  state backend blinken

blinkenSetters :: MVar (SerialDeviceMap) -> String -> IO Time -> IO (ParamPattern -> IO (), (Time -> [ParamPattern] -> ParamPattern) -> ParamPattern -> IO ())
blinkenSetters d n getNow = do
  ds <- blinkenState d n
  return (setter ds, transition getNow ds)

light :: Pattern String -> ParamPattern
light = make' VS light_p
light_p = S "light" Nothing

blinken = Shape {
  params = [
     light_p
     ],
  cpsStamp = True,
  latency = 0.01
  }

module Sound.Tidal.MidiStream (midiStream, midiBackend, midiState) where

-- generics
import qualified Data.Map as Map
import Data.List (sortBy)
import Data.Maybe
import Data.Ord (comparing)
import Data.Time (getCurrentTime, UTCTime, diffUTCTime)
import Data.Time.Clock.POSIX
import Control.Concurrent
import Control.Concurrent.MVar
import Data.Bits
import Foreign.C

-- Tidal specific
import Sound.Tidal.Tempo (Tempo, cps)
import Sound.Tidal.Stream as S
import Sound.Tidal.Utils

-- MIDI specific
import Sound.Tidal.MIDI.Device
import Sound.Tidal.MIDI.Control
import Sound.Tidal.MIDI.Params
import qualified Sound.PortMidi as PM


data Output = Output {
                       conn :: PM.PMStream,
                       lock :: MVar (),
                       offset :: Double,
                       buffer :: MVar [PM.PMEvent]
                     }

type MidiMap = Map.Map S.Param (Maybe Int)

toMidiEvent :: Value -> Maybe Int
toMidiEvent (VF x) = Just $ floor (x * 127) -- converts floats to Midi Int
toMidiEvent (VI x) = Just x
toMidiEvent (VS x) = Nothing -- ignore strings for now, we might 'read' them later

toMidiMap :: S.ParamMap -> MidiMap
toMidiMap m = Map.map (toMidiEvent) (Map.mapMaybe (id) m)



send s ch cshape shape change tick (o, m) = midi
    where
      midi = sendmidi s cshape (fromIntegral ch) (fromIntegral note, fromIntegral vel, fromIntegral dur) (diff) m'
      m' = Map.filterWithKey (\k _ -> (k /= F "dur" (Just 0.05)) && (k /= I "note" (Just (-1))) && (k /= F "velocity" (Just 0))) m
      diff = floor $ (*1000) $ (logicalOnset - (offset s)) -- timeDiff (sec, usec) (offset stream)
      note = maybe 0 (toI) (Map.lookup (I "note" (Just (-1))) m)
      dur = maybe 0 (toI) (Map.lookup (F "dur" (Just 0.05)) m)
      vel = maybe 0 (toI) (Map.lookup (F "velocity" (Just 0)) m)
      toI (Just i) = i
      toI _ = -1
      logicalOnset = logicalOnset' change tick o (0) -- missing nudge

makeConnection :: Int -> String -> Int -> ControllerShape -> IO (S.ToMessageFunc)
makeConnection latency deviceName channel cshape = do
  mid <- getIDForDeviceName deviceName
  case mid of
    Nothing -> do putStrLn "List of Available Device Names"
                  putStrLn =<< displayOutputDevices
                  error ("Device '" ++ show deviceName ++ "' not found")
    Just id -> do econn <- outputDevice id latency
                  case econn of
                    Right err -> error ("Failed opening MIDI Output on Device ID: " ++ show id ++ " - " ++ show err)
                    Left s -> do
                      putStrLn ("Successfully initialized Device '" ++ deviceName ++ "'")
                      sendevents s
                      return $ (\ shape change tick (o,m) -> do
                                 m' <- fmap (toMidiMap) (S.applyShape' shape m)
                                 return $ send s channel cshape shape change tick (o, m')
                                 )

midiBackend l n c cs = do
  s <- makeConnection l n c cs
  return $ Backend s

midiStream l n c s = do
  backend <- midiBackend l n c s
  stream backend (toOscShape s)

midiState l n c s = do
  backend <- midiBackend l n c s
  S.state backend (toOscShape s)

-- actual midi interaction
sendevents :: Output -> IO ThreadId
sendevents stream = do
  forkIO $ do loop stream
    where loop stream = do act stream
                           delay
                           loop stream
          act stream = do
            let buf = buffer stream
                o = conn stream
            buf' <- tryTakeMVar buf
            case buf' of
              Nothing ->  do
                return Nothing
              Just [] -> do
                putMVar buf []
                return Nothing
              (Just evts@(x:xs)) -> do
                midiTime <- PM.time
                let evts' = sortBy (comparing PM.timestamp) evts
                    nextTick = fromIntegral $ midiTime + 1 -- advance on millisecond, i.e. the next call of this loop
                    (evts'',later) = span (\x -> (((PM.timestamp x) < midiTime)) || ((PM.timestamp x) < nextTick)) evts'
                putMVar buf later

                err <- PM.writeEvents o evts''
                case err of
                  PM.NoError -> return Nothing
                  e -> return $ Just (userError ("Error '" ++ show e ++ "' sending Events: " ++ show evts))

          delay = threadDelay 1000 -- in microseconds, i.e. one millisecond


sendctrls  :: Output -> ControllerShape -> CLong -> CULong -> MidiMap -> IO ()
sendctrls stream shape ch t ctrls = do
  let ctrls' = filter ((>=0) . snd) $ Map.toList $ Map.mapMaybe (id) ctrls -- (zip (toKeynames shape) ctrls)
  sequence_ $ map (\(param, ctrl) -> makeCtrl stream ch (paramN shape param) ctrl t) ctrls'
  return ()

sendnote :: RealFrac s => Output -> t -> CLong -> (CLong, CLong, s) -> CULong -> IO ThreadId
sendnote stream shape ch (note,vel, dur) t =
  do forkIO $ do noteOn stream ch note vel t
                 noteOff stream ch note (t + (floor $ 1000 * dur))
                 return ()

sendmidi :: (Show s, RealFrac s) => Output -> ControllerShape -> CLong -> (CLong, CLong, s) -> CULong -> MidiMap -> IO ()
sendmidi stream shape ch (128,vel,dur) t ctrls = do
  sendctrls stream shape ch t ctrls
  return ()
sendmidi stream shape ch (note,vel,dur) t ctrls = do
  sendnote stream shape ch (note,vel,dur) t
  sendctrls stream shape ch t ctrls
  return ()


-- MIDI Utils
encodeChannel :: (Bits a, Num a) => a -> a -> a
encodeChannel ch cc = (((-) ch 1) .|. cc)


-- MIDI Messages
noteOn :: Output -> CLong -> CLong -> CLong -> CULong -> IO (Maybe a)
noteOn o ch val vel t = do
  let evt = makeEvent 0x90 val ch vel t
  sendEvent o evt

noteOff :: Output -> CLong -> CLong -> CULong -> IO (Maybe a)
noteOff o ch val t = do
  let evt = makeEvent 0x80 val ch 60 t
  sendEvent o evt

makeCtrl :: Output -> CLong -> ControlChange -> Int -> CULong -> IO (Maybe a)
makeCtrl o ch (CC {midi=midi, range=range, scalef=f}) n t = makeCC o ch (fromIntegral midi) scaledN t
  where scaledN = fromIntegral n -- fromIntegral (f range (n))
makeCtrl o ch (NRPN {midi=midi, range=range, scalef=f}) n t = makeNRPN o ch (fromIntegral midi) scaledN t
  where scaledN = fromIntegral n -- fromIntegral $ (f range (n))
-- makeCtrl o ch (C.SysEx {C.midi=midi, C.range=range, C.scalef=f}) n t = makeSysEx o ch (fromIntegral midi) scaledN t
--   where scaledN = fromIntegral $ (f range (n))

-- This is sending CC
makeCC :: Output -> CLong -> CLong -> CLong -> CULong -> IO (Maybe a)
makeCC o ch c n t = do
  let evt = makeEvent 0xB0 c ch n t
  sendEvent o evt

-- This is sending NRPN
makeNRPN :: Output -> CLong -> CLong -> CLong -> CULong -> IO (Maybe a)
makeNRPN o ch c n t = do
  let nrpn = makeEvent 0xB0
      evts = [nrpn 0x63 ch (shift (c .&. 0x3F80) (-7)) t,
              nrpn 0x62 ch (c .&. 0x7F) t,
              nrpn 0x06 ch (shift (n .&. 0x3F80) (-7)) t,
              nrpn 0x26 ch (n .&. 0x7F) t
             ]
  mapM (sendEvent o) evts
  return Nothing


-- Port Midi Wrapper

outputDevice :: PM.DeviceID -> Int -> IO (Either Output PM.PMError)
outputDevice deviceID latency = do
  PM.initialize
  now <- getCurrentTime
  result <- PM.openOutput deviceID latency
  case result of
    Left dev ->
      do
        info <- PM.getDeviceInfo deviceID
        putStrLn ("Opened: " ++ show (PM.interface info) ++ ": " ++ show (PM.name info))
        sem <- newEmptyMVar
        putMVar sem () -- initially fill MVar to be taken by the first user of this output
        buffer <- newMVar []

        midiOffset <- PM.time

        let posixNow = realToFrac $ utcTimeToPOSIXSeconds now
            syncedNow = posixNow - ((0.001*) $ fromIntegral midiOffset)
            -- sec = floor syncedNow
            -- usec = floor $ 1000000 * (syncedNow - (realToFrac sec))
        return (Left Output { conn=dev, lock=sem, offset=syncedNow, buffer=buffer })
    Right err -> return (Right err)


makeEvent :: CLong -> CLong -> CLong -> CLong -> CULong -> PM.PMEvent
makeEvent st n ch v t = PM.PMEvent msg (t)
  where msg = PM.PMMsg (encodeChannel ch st) (n) (v)

-- now with a semaphore since PortMIDI is NOT thread safe
sendEvent :: Output -> PM.PMEvent -> IO (Maybe a)
sendEvent o evt = do
  let sem = lock o
      buf = buffer o
  cbuf <- takeMVar buf
  putMVar buf (cbuf ++ [evt])
  return Nothing

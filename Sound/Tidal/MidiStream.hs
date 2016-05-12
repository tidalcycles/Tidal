module Sound.Tidal.MidiStream (midiStream, midiBackend, midiState, midiSetters, midiDevices) where

import Control.Monad.Trans.Maybe
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
import Control.Applicative

-- Tidal specific
import Sound.Tidal.Tempo (Tempo, cps)
import Sound.Tidal.Stream as S
import Sound.Tidal.Utils
import Sound.Tidal.Time
import Sound.Tidal.Transition (transition)

-- MIDI specific
import Sound.Tidal.MIDI.Device
import Sound.Tidal.MIDI.Control
import qualified Sound.PortMidi as PM

data Output = Output {
                       conn :: PM.PMStream,
                       lock :: MVar (),
                       offset :: Double,
                       buffer :: MVar [PM.PMEvent]
                     }

type MidiMap = Map.Map S.Param (Maybe Int)
type MidiDeviceMap = Map.Map String Output

toMidiEvent :: ControllerShape -> S.Param -> Value -> Maybe Int
toMidiEvent s p (VF x) = ($) <$> mscale <*> mrange <*> pure x
    where
      mrange = fmap range mcc
      mscale = fmap scalef mcc
      mcc = paramN s p
toMidiEvent s p (VI x) = Just x
toMidiEvent s p (VS x) = Nothing -- ignore strings for now, we might 'read' them later

toMidiMap :: ControllerShape -> S.ParamMap -> MidiMap
toMidiMap s m = Map.mapWithKey (toMidiEvent s) (Map.mapMaybe (id) m)


send s ch cshape shape change tick o ctrls (tdur:tnote:trest) = midi
    where
      midi = sendmidi s cshape ch' (note, vel, dur) (diff) ctrls
      diff = floor $ (*1000) $ (logicalOnset - (offset s))
      note = fromIntegral $ ivalue $ snd tnote
      dur = realToFrac $ fvalue $ snd tdur
      (vel, nudge) = case length trest of
        2 -> (mkMidi $ trest !! 1, fvalue $ snd $ trest !! 0)
        1 -> (mkMidi $ trest !! 0, 0)
      ch' = fromIntegral ch
      mkMidi = fromIntegral . floor . (*127) . fvalue . snd
      logicalOnset = logicalOnset' change tick o nudge

mkSend cshape channel s = return $ (\ shape change tick (o,m) -> do
                        let defaulted = (S.applyShape' shape m)
                            -- split ParamMap into Properties and Controls
                            mpartition = fmap (Map.partitionWithKey (\k _ -> (name k) `elem` ["dur", "n", "velocity", "nudge"])) defaulted
                            props = fmap fst mpartition
                            ctrls = fmap snd mpartition
                            props' = fmap (Map.toAscList) $ fmap (Map.mapMaybe (id)) props
                            -- only send explicitly set Control values
                            ctrls' = fmap (Map.filterWithKey (\k v -> v /= (defaultValue k))) ctrls
                            ctrls'' = fmap (toMidiMap cshape) ctrls'
                            send' = fmap (send s channel cshape shape change tick o) ctrls''
                        ($) <$> send' <*> props'
                        )

connected cshape channel name s = do
  putStrLn ("Successfully initialized Device '" ++ name ++ "'")
  sendevents s
  mkSend cshape channel s

failed di err = do
  error (show err ++ ": " ++ show di)

notfound name = do
  putStrLn "List of Available Device Names"
  putStrLn =<< displayOutputDevices
  error ("Device '" ++ show name ++ "' not found")

useOutput outsM name lat = do
  outs <- readMVar outsM -- maybe
  let outM = Map.lookup name outs -- maybe
  -- if we have a valid output by now, return
  case outM of
    Just o -> do
      putStrLn "Cached Device Output"
      return $ Just o
    Nothing -> do
      -- otherwise open a new output and store the result in the mvar
      devidM <- (>>= maybe (failed name "Failed opening MIDI Output Device ID") return) (getIDForDeviceName name)
      econn <- outputDevice devidM lat  -- either
      case econn of
        Left o -> do
          swapMVar outsM $ Map.insert name o outs
          return $ Just o
        Right _ -> return Nothing



makeConnection :: MVar (MidiDeviceMap) -> String -> Int -> ControllerShape -> IO (S.ToMessageFunc)
makeConnection devicesM deviceName channel cshape = do
  let lat = (floor $ (*100) $ Sound.Tidal.MIDI.Control.latency cshape)
  moutput <- useOutput devicesM deviceName lat
  case moutput of
    Just o ->
      connected cshape channel deviceName o
    Nothing ->
      --failed o
      error "Failed"
--  devidM'' <- devidM'  -- maybe

midiDevices :: IO (MVar (MidiDeviceMap))
midiDevices = do
  newMVar $ Map.fromList []

midiBackend d n c cs = do
  s <- makeConnection d n c cs
  return $ Backend s

midiStream d n c s = do
  backend <- midiBackend d n c s
  stream backend (toShape s)

midiState d n c s = do
  backend <- midiBackend d n c s
  S.state backend (toShape s)

midiSetters :: MVar (MidiDeviceMap) -> String -> Int -> ControllerShape -> IO Time -> IO (ParamPattern -> IO (), (Time -> [ParamPattern] -> ParamPattern) -> ParamPattern -> IO ())
midiSetters d n c s getNow = do
  ds <- midiState d n c s
  return (setter ds, transition getNow ds)


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
  let ctrls' = filter ((>=0) . snd) $ Map.toList $ Map.mapMaybe (id) ctrls
  sequence_ $ map (\(param, ctrl) -> makeCtrl stream ch (fromJust $ paramN shape param) (fromIntegral ctrl) t) ctrls' -- FIXME: we should be sure param has ControlChange
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

makeCtrl :: Output -> CLong -> ControlChange -> CLong -> CULong -> IO (Maybe a)
makeCtrl o ch (CC {midi=midi, range=range}) n t = makeCC o ch (fromIntegral midi) n t
makeCtrl o ch (NRPN {midi=midi, range=range}) n t = makeNRPN o ch (fromIntegral midi) n t
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
        return (Left Output { conn=dev, lock=sem, offset=syncedNow, buffer=buffer })
    Right err -> return (Right err)


makeEvent :: CLong -> CLong -> CLong -> CLong -> CULong -> PM.PMEvent
makeEvent st n ch v t = PM.PMEvent msg (t)
  where msg = PM.encodeMsg $ PM.PMMsg (encodeChannel ch st) (n) (v)

-- now with a semaphore since PortMIDI is NOT thread safe
sendEvent :: Output -> PM.PMEvent -> IO (Maybe a)
sendEvent o evt = do
  let sem = lock o
      buf = buffer o
  cbuf <- takeMVar buf
  putMVar buf (cbuf ++ [evt])
  return Nothing

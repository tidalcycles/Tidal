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

import Numeric

-- Tidal specific
import Sound.Tidal.Tempo (Tempo, cps, clockedTick)
import Sound.Tidal.Stream as S
import Sound.Tidal.Utils
import Sound.Tidal.Time
import Sound.Tidal.Transition (transition)

-- MIDI specific
import Sound.Tidal.MIDI.Device
import Sound.Tidal.MIDI.Control
import Sound.Tidal.MIDI.Params
import qualified Sound.PortMidi as PM


type ConnectionCount = Int
type TickedConnectionCount = Int
type OutputOnline = Bool
type OutputState = (TickedConnectionCount, ConnectionCount, OutputOnline)
type MIDITime = (Tempo, Int, Double, Double)
type MIDIEncoder = CULong -> PM.PMEvent
type MIDIEvent = (MIDITime, MIDIEncoder)

data Output = Output {
                       conn :: PM.PMStream,
                       buffer :: MVar [MIDIEvent],
                       bufferstate :: MVar OutputState
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
      midi = sendmidi s cshape ch' (note, vel, dur) (change, tick, o, offset) ctrls
      note = fromIntegral $ ivalue $ snd tnote
      dur = realToFrac $ fvalue $ snd tdur
      (vel, nudge) = case length trest of
        2 -> (mkMidi $ trest !! 1, fvalue $ snd $ trest !! 0)
        1 -> (mkMidi $ trest !! 0, 0)
      ch' = fromIntegral ch
      mkMidi = fromIntegral . floor . (*127) . fvalue . snd
      offset = ((Sound.Tidal.MIDI.Control.latency cshape) + nudge)

mkSend cshape channel s = return $ (\ shape change tick (o,m) -> do
                        let defaulted = (S.applyShape' shape m)
                            -- split ParamMap into Properties and Controls
                            mpartition = fmap (Map.partitionWithKey (\k _ -> (name k) `elem` ["dur", "note", "velocity", "nudge"])) defaulted
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
  changeState goOnline s
  mkSend cshape channel s

failed di err = do
  error (show err ++ ": " ++ show di)

notfound name = do
  putStrLn "List of Available Device Names"
  putStrLn =<< displayOutputDevices
  error ("Device '" ++ show name ++ "' not found")

readState f o = do
  s <- readMVar $ bufferstate o
  let fs = f s
      (ticked, conns, online) = s
  return fs

isCycling (0, conns, True) = True
isCycling _ = False

displayState (ticked, conns, online) = show ticked ++ "/" ++ show conns ++ "[" ++ show online ++ "]"

changeState f o = do
  bs <- takeMVar stateM
  let fs = f bs
      (ticked, conns, online) = fs      
  putMVar stateM $ fs
    where
      stateM = bufferstate o

goOnline (ticked, conns, online) = (ticked, conns, True)
addConnection (ticked, conns, online) = (ticked, conns + 1, online)
tickConnections (ticked, conns, online) = ((ticked + 1) `mod` conns, conns, online)

useOutput outsM name lat = do
  outs <- readMVar outsM -- blocks
  let outM = Map.lookup name outs -- maybe
  -- if we have a valid output by now, return
  case outM of
    Just o -> do
      putStrLn "Cached Device Output"
      changeState addConnection o -- blocks
      return $ Just o
    Nothing -> do
      -- otherwise open a new output and store the result in the mvar
      devidM <- (>>= maybe (failed name "Failed opening MIDI Output Device ID") return) (getIDForDeviceName name)
      econn <- outputDevice devidM lat  -- either
      case econn of
        Left o -> do
          changeState addConnection o
          swapMVar outsM $ Map.insert name o outs
          return $ Just o
        Right _ -> return Nothing

makeConnection :: MVar (MidiDeviceMap) -> String -> Int -> ControllerShape -> IO ((S.ToMessageFunc), Output)
makeConnection devicesM deviceName channel cshape = do
  moutput <- useOutput devicesM deviceName 1
  case moutput of
    Just o -> do
      s <- connected cshape channel deviceName o
      return (s, o)
    Nothing ->
      --failed o
      error "Failed"

showLate :: (CULong, Double, PM.PMEvent, CULong, UTCTime) -> String
showLate (o, t, e, m, n) =
  unwords ["late",
           (show $ (\x -> [PM.status x, PM.data1 x, PM.data2 x]) $ PM.decodeMsg $ PM.message e),
           "midi now ", show m, " midi onset: ", show o,
           "onset (relative): ", show $ showFFloat (Just 3) (t - (realToFrac $ utcTimeToPOSIXSeconds n)) "",
           ", sched: ", show $ PM.timestamp e]


-- should only send out events if all connections have ticked
flushBackend :: Output -> S.Shape -> Tempo -> Int -> IO ()
flushBackend o shape change ticks = do
  changeState tickConnections o
  cycling <- readState isCycling o

  case cycling of
    True -> do
      late <- sendevents o shape change ticks
      let len = length late
  
      case len of
        0 ->
          return ()
        _ -> do
          putStrLn $ showLate $ head late
          putStrLn $ "and " ++ show (len - 1) ++ " more"
    False -> do
      s <- readState displayState o
      
      return ()
      

midiDevices :: IO (MVar (MidiDeviceMap))
midiDevices = do
  newMVar $ Map.fromList []

midiBackend d n c cs = do
  (s, o) <- makeConnection d n c cs
  return $ Backend s (flushBackend o)

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


toDescriptor midiTime now (o,m,t,e) = (o,t,e, midiTime, now)

calcOnsets (a@(tempo, tick, onset, offset), e) = (a, logicalOnset' tempo tick onset offset, e)

showEvent :: PM.PMEvent -> String
showEvent e = show t ++ " " ++ show msg 
  where msg = PM.decodeMsg $ PM.message e
        t = PM.timestamp e

showRawEvent :: (CULong, MIDITime, Double, PM.PMEvent) -> String
showRawEvent (midionset, (tempo,tick,onset,offset), logicalOnset, e) = show onset ++ " " ++ " / " ++ show logicalOnset ++ " " ++  showEvent e

sendevents :: Output -> S.Shape -> Tempo -> Int -> IO ([(CULong, Double, PM.PMEvent, CULong, UTCTime)])
sendevents stream shape change ticks = do
  let buf = buffer stream
      output = conn stream
  buf' <- tryTakeMVar buf
  case buf' of
    Nothing -> do
      return []
    Just [] -> do
      -- make sure we put back an empty buffer
      putMVar buf []
      return []
    (Just evts@(x:xs)) -> do
          midiTime <- PM.time
          now <- getCurrentTime
          
          let offset = S.latency shape
              nextTick = logicalOnset' change (ticks+1) 0 offset
              onsets = map calcOnsets evts
              -- split into events sent now and later (e.g. a noteOff that would otherwise cut off noteOn's in the next tick)
              (evts', later) = span ((< nextTick).(\(_,o,_) -> o)) $ sortBy (comparing (\(_,o,_) -> o)) onsets
              -- calculate MIDI time to schedule events, putting time into fn to create PM.PMEvents
              evts'' = map (\(t, o, e) -> let midionset = scheduleTime midiTime now o
                                         in (midionset, t, o, e midionset)) evts'
              later' = map (\(t,o,e) -> (t,e)) later
              evts''' = map (\(_,_,_,e) -> e) evts''
              -- filter events that are too late
              late = map (toDescriptor midiTime now) $ filter (\(_,_,t,_) -> t < (realToFrac $ utcTimeToPOSIXSeconds now)) $ evts''

          -- write events for this tick to stream
          err <- PM.writeEvents output evts'''
          -- store later events for nextTick
          putMVar buf later'
          case err of
            PM.NoError -> do
              -- return events for logging in outer scope
              return late
            e -> do
              putStrLn "sending failed"
              return []


sendctrls  :: Output -> ControllerShape -> CLong -> MIDITime -> MidiMap -> IO ()
sendctrls stream shape ch t ctrls = do
  let ctrls' = filter ((>=0) . snd) $ Map.toList $ Map.mapMaybe (id) ctrls
  sequence_ $ map (\(param, ctrl) -> makeCtrl stream ch (fromJust $ paramN shape param) (fromIntegral ctrl) t) ctrls' -- FIXME: we should be sure param has ControlChange
  return ()

sendnote :: Output -> t -> CLong -> (CLong, CLong, Double) -> MIDITime -> IO ()
sendnote stream shape ch (note,vel, dur) (tempo,tick,onset,offset) = do
  noteOn stream ch note vel (tempo,tick,onset,offset)
  noteOff stream ch note (tempo, tick, onset, offset + dur)
  return ()

scheduleTime :: CULong -> UTCTime -> Double -> CULong
scheduleTime mnow' now' logicalOnset = t
  where
    now = realToFrac $ utcTimeToPOSIXSeconds $ now'
    mnow = fromIntegral mnow'
    t = floor $ mnow + (1000 * (logicalOnset - now)) -- 1 second are 1000 microseconds as is the unit of timestamps in PortMidi
    

sendmidi :: Output -> ControllerShape -> CLong -> (CLong, CLong, Double) -> MIDITime -> MidiMap -> IO ()
sendmidi stream shape ch n t ctrls = do
  sendmidi' stream shape ch n t ctrls
  return ()


sendmidi' stream shape ch (128,vel,dur) t ctrls = do
  sendctrls stream shape ch t ctrls
  return ()
sendmidi' stream shape ch (note,vel,dur) t ctrls = do
  sendnote stream shape ch (note,vel,dur) t
  sendctrls stream shape ch t ctrls
  return ()


-- MIDI Utils
encodeChannel :: (Bits a, Num a) => a -> a -> a
encodeChannel ch cc = (((-) ch 1) .|. cc)


-- MIDI Messages
noteOn :: Output -> CLong -> CLong -> CLong -> MIDITime -> IO (Maybe a)
noteOn o ch val vel t = do
  let evt = (t, \t' -> makeEvent 0x90 val ch vel t')
  sendEvent o evt

noteOff :: Output -> CLong -> CLong -> MIDITime -> IO (Maybe a)
noteOff o ch val t = do
  let evt = (t, \t' -> makeEvent 0x80 val ch 60 t')
  sendEvent o evt

makeCtrl :: Output -> CLong -> ControlChange -> CLong -> MIDITime -> IO (Maybe a)
makeCtrl o ch (CC {midi=midi, range=range}) n t = makeCC o ch (fromIntegral midi) n t
makeCtrl o ch (NRPN {midi=midi, range=range}) n t = makeNRPN o ch (fromIntegral midi) n t

-- This is sending CC
makeCC :: Output -> CLong -> CLong -> CLong -> MIDITime -> IO (Maybe a)
makeCC o ch c n t = do
  let evt = (t, \t' -> makeEvent 0xB0 c ch n t')
  sendEvent o evt

-- This is sending NRPN
makeNRPN :: Output -> CLong -> CLong -> CLong -> MIDITime -> IO (Maybe a)
makeNRPN o ch c n t = do
  let nrpn = makeEvent 0xB0
      evts = [(t, (\t' -> nrpn 0x63 ch (shift (c .&. 0x3F80) (-7)) t')),
              (t, (\t' -> nrpn 0x62 ch (c .&. 0x7F) t')),
              (t, (\t' -> nrpn 0x06 ch (shift (n .&. 0x3F80) (-7)) t')),
              (t, (\t' -> nrpn 0x26 ch (n .&. 0x7F) t'))
             ]
  mapM (sendEvent o) evts
  return Nothing


-- Port Midi Wrapper

outputDevice :: PM.DeviceID -> Int -> IO (Either Output PM.PMError)
outputDevice deviceID latency = do
  PM.initialize
  now <- getCurrentTime
  result <- PM.openOutput deviceID latency
  bs <- newMVar (0, 0, False)
  case result of
    Left dev ->
      do
        info <- PM.getDeviceInfo deviceID
        putStrLn ("Opened: " ++ show (PM.interface info) ++ ": " ++ show (PM.name info))
        buffer <- newMVar []

        return (Left Output { conn=dev, buffer=buffer, bufferstate=bs })
    Right err -> return (Right err)


makeEvent :: CLong -> CLong -> CLong -> CLong -> CULong -> PM.PMEvent
makeEvent st n ch v t = PM.PMEvent msg (t)
  where msg = PM.encodeMsg $ PM.PMMsg (encodeChannel ch st) (n) (v)

sendEvent :: Output -> (MIDITime, (CULong -> PM.PMEvent)) -> IO (Maybe a)
sendEvent o evt = do
  let buf = buffer o

  cbuf <- takeMVar buf
  putMVar buf (cbuf ++ [evt])
  return Nothing

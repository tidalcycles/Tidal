{-# LANGUAGE ForeignFunctionInterface #-}

module Sound.Tidal.Link where

#include "abl_link.h"

import Foreign
import Foreign.C.Types
import Data.Int()

data AbletonLinkImpl
data SessionStateImpl

newtype AbletonLink = AbletonLink (Ptr AbletonLinkImpl)
newtype SessionState = SessionState (Ptr SessionStateImpl)

instance Show AbletonLink where
  show _ = "-unshowable-"

instance Show SessionState where
  show _ = "-unshowable-"

type Beat    = CDouble
type BPM     = CDouble
type Micros  = Int64
type Quantum = CDouble

instance Storable AbletonLink where
  alignment _ = #{alignment abl_link}
  sizeOf    _ = #{size abl_link}
  peek ptr = do
    impl <- #{peek abl_link,impl} ptr
    return (AbletonLink impl)
  poke ptr (AbletonLink impl) = do
    #{poke abl_link,impl} ptr impl

instance Storable SessionState where
  alignment _ = #{alignment abl_link_session_state}
  sizeOf    _ = #{size abl_link_session_state}
  peek ptr = do
    impl <- #{peek abl_link_session_state,impl} ptr
    return (SessionState impl)
  poke ptr (SessionState impl) = do
    #{poke abl_link_session_state,impl} ptr impl

foreign import ccall "abl_link.h abl_link_create"
  create :: BPM -> IO AbletonLink

foreign import ccall "abl_link.h abl_link_enable"
  abl_link_enable :: AbletonLink -> CBool -> IO ()

setEnabled :: Bool -> AbletonLink -> IO ()
setEnabled True al = abl_link_enable al (CBool 1)
setEnabled False al = abl_link_enable al (CBool 0)

enable :: AbletonLink -> IO ()
enable = setEnabled True

disable :: AbletonLink -> IO ()
disable = setEnabled False

foreign import ccall "abl_link.h abl_link_create_session_state"
  createSessionState :: IO SessionState

foreign import ccall "abl_link.h abl_link_capture_app_session_state"
  captureAppSessionState :: AbletonLink -> SessionState -> IO ()

createAndCaptureAppSessionState :: AbletonLink -> IO SessionState
createAndCaptureAppSessionState al = do
  sessionState <- createSessionState
  captureAppSessionState al sessionState
  return sessionState

foreign import ccall "abl_link.h abl_link_commit_app_session_state"
  commitAppSessionState :: AbletonLink -> SessionState -> IO ()

foreign import ccall "abl_link.h abl_link_destroy_session_state"
  destroySessionState :: SessionState -> IO ()

commitAndDestroyAppSessionState :: AbletonLink -> SessionState -> IO ()
commitAndDestroyAppSessionState al ss = do
  commitAppSessionState al ss
  destroySessionState ss

foreign import ccall "abl_link.h abl_link_clock_micros"
  clock :: AbletonLink -> IO Micros

foreign import ccall "abl_link.h abl_link_beat_at_time"
  beatAtTime :: SessionState -> Micros -> Quantum -> IO Beat

foreign import ccall "abl_link.h abl_link_time_at_beat"
  timeAtBeat :: SessionState -> Beat -> Quantum -> IO Micros

foreign import ccall "abl_link.h abl_link_tempo"
  getTempo :: SessionState -> IO BPM

foreign import ccall "abl_link.h abl_link_set_tempo"
  setTempo :: SessionState -> BPM -> Micros -> IO ()

foreign import ccall "abl_link.h abl_link_request_beat_at_time"
  requestBeatAtTime :: SessionState -> Beat -> Micros -> Quantum -> IO ()

foreign import ccall "abl_link.h abl_link_force_beat_at_time"
  forceBeatAtTime :: SessionState -> Beat -> Micros -> Quantum -> IO ()

-- |Test
hello :: IO ()
hello = do
    print "hello"
    link <- create 88
    print "Created link"
    _ <- getLine
    print "gotline"
    now <- clock link
    print $ "Now: " ++ show now
    print "gotline"
    enable link
    print "Link enabled"
    _ <- getLine
    print "gotline"
    sessionState <- createSessionState
    print "Created sessionState"
    _ <- getLine
    print "gotline"
    captureAppSessionState link sessionState
    _line_1 <- getLine
    print "gotline"
    setTempo sessionState 130 now
    beat <- beatAtTime sessionState now 1
    print $ "beat: " ++ show beat
    _line_2 <- getLine
    print "gotline"
    now' <- clock link
    print $ "Now': " ++ show now'
    beat' <- beatAtTime sessionState now' 1
    print $ "beat': " ++ show beat'
    commitAndDestroyAppSessionState link sessionState
    _line_3 <- getLine
    print $ "done"

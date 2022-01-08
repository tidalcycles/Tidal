{-# LANGUAGE ForeignFunctionInterface #-}

-- module Sound.Tidal.Link (hello, wrapper_create, show_beat, enable_link, beat_time, set_tempo_at_beat, LinkWrapper) where
module Sound.Tidal.Link (hello, abl_link_create, abl_link_enable, AbletonLink) where

#include "abl_link.h"

import Foreign
-- import Foreign.C.String
import Foreign.C.Types
import Data.Int()

newtype AbletonLink = AbletonLink (Ptr ())
newtype SessionState = SessionState (Ptr ())

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
  abl_link_create :: CDouble -> IO AbletonLink

foreign import ccall "abl_link.h abl_link_enable"
  abl_link_enable :: AbletonLink -> CBool -> IO ()

foreign import ccall "abl_link.h abl_link_create_session_state"
  abl_link_create_session_state :: IO SessionState

foreign import ccall "abl_link.h abl_link_capture_app_session_state"
  abl_link_capture_app_session_state :: AbletonLink -> SessionState -> IO ()

foreign import ccall "abl_link.h abl_link_clock_micros"
  abl_link_clock_micros :: AbletonLink -> IO Int64

foreign import ccall "abl_link.h abl_link_beat_at_time"
  abl_link_beat_at_time :: SessionState -> Int64 -> CDouble -> IO CDouble

-- |Test
hello :: IO ()
hello = do
    print "hello"
    link <- abl_link_create 88
    print "Created link"
    _ <- getLine
    print "gotline"
    now <- abl_link_clock_micros link
    print $ "Now: " ++ show now
    print "gotline"
    abl_link_enable link (CBool 1)
    print "Link enabled"
    _ <- getLine
    print "gotline"
    sessionState <- abl_link_create_session_state
    print "Created sessionState"
    _ <- getLine
    _ <- getLine
    print "gotline"
    abl_link_capture_app_session_state link sessionState
    _line_1 <- getLine
    print "gotline"
    beat <- abl_link_beat_at_time sessionState now 1
    print $ "beat: " ++ show beat
    _line_2 <- getLine
    print "gotline"
    now' <- abl_link_clock_micros link
    print $ "Now': " ++ show now'
    beat' <- abl_link_beat_at_time sessionState now' 1
    print $ "beat': " ++ show beat'

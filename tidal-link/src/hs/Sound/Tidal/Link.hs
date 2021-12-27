{-# LANGUAGE ForeignFunctionInterface #-}

module Sound.Tidal.Link (hello, link_wrapper_test, wrapper_create, show_beat, enable_link, beat_time, set_tempo_at_beat, LinkWrapper) where

-- import Foreign
import Foreign.Ptr
-- import Foreign.C.String
import Foreign.C.Types

type LinkWrapper = ()

foreign import ccall "link_wrapper.h link_wrapper_test"
  link_wrapper_test :: IO CDouble

foreign import ccall "link_wrapper.h wrapper_create"
  wrapper_create :: IO (Ptr LinkWrapper)

foreign import ccall "link_wrapper.h beat_time"
  beat_time :: Ptr LinkWrapper -> IO (CDouble)

foreign import ccall "link_wrapper.h enable_link"
  enable_link :: Ptr LinkWrapper -> IO (CDouble)

foreign import ccall "link_wrapper.h set_tempo_at_beat"
  set_tempo_at_beat :: Ptr LinkWrapper -> CDouble -> CDouble -> IO ()

show_beat :: (Ptr LinkWrapper) -> IO ()
show_beat link = do
  a <- beat_time link
  print $ "beat: " ++ show a

-- |Test
hello :: IO ()
hello = do
    a <- link_wrapper_test
    print $ "have test val: " ++ show a
    b <- wrapper_create
    print $ "Created b"
    c <- enable_link b
    print $ "Link enabled: " ++ show c
    d <- beat_time b
    print $ "beat: " ++ show d
    line_1 <- getLine
    print line_1
    e <- beat_time b
    print $ "beat: " ++ show e
    line_2 <- getLine
    print line_2
    f <- beat_time b
    print $ "beat: " ++ show f
{-# LANGUAGE ForeignFunctionInterface #-}

module Sound.Tidal.Link (hello, link_wrapper_test) where

-- import Foreign
import Foreign.Ptr
-- import Foreign.C.String
import Foreign.C.Types

type LinkWrapper = ()

foreign import ccall "link_wrapper.h link_wrapper_test"
  link_wrapper_test :: IO CDouble

foreign import ccall "link_wrapper.h wrapper_create"
  wrapper_create :: IO (Ptr LinkWrapper)

foreign import ccall unsafe "link_wrapper.h beat_time"
  beat_time :: Ptr LinkWrapper -> IO (CDouble)

hello :: IO ()
hello = do a <- link_wrapper_test
           b <- wrapper_create
           c <- beat_time b
           print $ "got test: " ++ show a ++ " and beat: " ++ show c

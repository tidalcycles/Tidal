{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Sound.Tidal.TidalParseFFITest where

import Foreign.C.String (CString, peekCString, newCString)
import Test.HUnit
import Data.Aeson (Value, decode, encode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as B

-- Foreign function import
foreign import ccall "eval_pattern_c" eval_pattern_c :: CString -> CString -> IO CString

-- Utility function to run FFI test
ffiTest :: String -> String -> IO Bool
ffiTest input arcLen = do
    cInput <- newCString input 
    cArcLen <- newCString arcLen 
    resultPtr <- eval_pattern_c cInput cArcLen
    result <- peekCString resultPtr
    let expected = B.unpack $ encode mockJSON
    return (result == expected)

-- Mock the exact expected JSON output
mockJSON :: Value
mockJSON = object
    [ "arcLen" .= (1 :: Int)
    , "events" .= 
        [ object [
                 "part" .= object ["start" .= (0 :: Double), "stop" .= (0.5 :: Double)]
                 , "value" .= object ["s" .= ("bd" :: String)]
                 , "whole" .= object ["start" .= (0 :: Double), "stop" .= (0.5 :: Double)]
                 ]
        , object [
                 "part" .= object ["start" .= (0.5 :: Double), "stop" .= (1 :: Double)]
                 , "value" .= object ["s" .= ("cd" :: String)]
                 , "whole" .= object ["start" .= (0.5 :: Double), "stop" .= (1 :: Double)]
                 ]
        ]
    ]

-- Test case with the mocked JSON output
testFullPattern :: Test
testFullPattern = TestCase $ do
    result <- ffiTest "s $ \"bd cd\"" "1"
    assertBool "Full pattern 's $ \"bd cd\"' should return the expected JSON" result

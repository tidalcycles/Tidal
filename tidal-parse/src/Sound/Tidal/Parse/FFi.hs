{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Sound.Tidal.Parse.FFI where

import Foreign.C.String (CString, peekCString, newCString)
import qualified Data.Aeson as Aeson
import Data.Aeson (ToJSON(..), object, (.=))
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Data.Ratio ((%))

import Sound.Tidal.Parse (parseTidal)
import Sound.Tidal.Context
import Sound.Tidal.Pattern (Value) 
import Sound.Tidal.Show (show)

-- Ensure Tidal's `Value` is converted to JSON properly
instance ToJSON Value where
    toJSON (VS s) = toJSON s
    toJSON (VI i) = toJSON i
    toJSON (VF f) = toJSON f
    toJSON (VN n) = toJSON $ show n
    toJSON (VR r) = toJSON $ show r
    toJSON (VB b) = toJSON b
    toJSON (VX xs) = toJSON xs
    toJSON (VPattern pat) = toJSON $ show pat
    toJSON (VState f) = toJSON $ show $ f Map.empty
    toJSON (VList vs) = toJSON vs

-- JSON serialization for ArcF
instance (Real a) => ToJSON (ArcF a) where
    toJSON (Arc start stop) =
        object ["start" .= (realToFrac start :: Double), 
                "stop"  .= (realToFrac stop :: Double)]

-- JSON serialization for EventF
instance (ToJSON a, ToJSON b) => ToJSON (EventF a b) where
    toJSON (Event ctx whole part value) =
        object [ "context" .= show ctx
               , "whole"   .= whole
               , "part"    .= part
               , "value"   .= value
               ]

-- Foreign export wrapper function
foreign export ccall eval_pattern_c :: CString -> CString -> IO CString
eval_pattern_c :: CString -> CString -> IO CString
eval_pattern_c cStr cArc = do
    hsStr <- peekCString cStr
    arcStr <- peekCString cArc
    let arcLength = fromMaybe 16 (readMaybe arcStr :: Maybe Double)
    result <- evalPattern hsStr arcLength
    newCString result

-- Function to evaluate and return pattern events as a JSON string
evalPattern :: String -> Double -> IO String
evalPattern pat arcLen = do
    let parsedResult = parseAndQuery pat arcLen
    return $ B.unpack $ Aeson.encode (either encodeError (encodeSuccess arcLen) parsedResult)

encodeError :: String -> Aeson.Value
encodeError err = Aeson.object ["error" Aeson..= err]

encodeSuccess :: Double -> [Event (Map.Map String Value)] -> Aeson.Value
encodeSuccess arcLen events = 
    Aeson.object ["arcLen" .= arcLen, "events" .= events]

-- Helper functions to handle parsing and querying
parseAndQuery :: String -> Double -> Either String [Event (Map.Map String Value)]
parseAndQuery str arcLen =
    case parseTidal str of
        Left err -> Left (show err)
        Right parsed -> 
            let arcTime = toRational arcLen
            in Right $ query (stripContext parsed) (State (Arc 0 arcTime) Map.empty)

stripContext :: Pattern a -> Pattern a
stripContext = setContext $ Context []

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Sound.Tidal.Parse.FFI where

import Foreign.C.String (CString, peekCString, newCString)
import qualified Data.Aeson as Aeson
import Data.Aeson (ToJSON(..), object, (.=))
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import GHC.Generics (Generic)

import Sound.Tidal.Parse (parseTidal)
import Sound.Tidal.Pattern
import Sound.Tidal.Params ()
import Sound.Tidal.Show ()

-- Newtype wrappers to avoid orphan instances
newtype JSONValue = JSONValue { unJSONValue :: Value }
  deriving (Generic)

instance ToJSON JSONValue where
    toJSON (JSONValue (VS str)) = toJSON str
    toJSON (JSONValue (VI i)) = toJSON i
    toJSON (JSONValue (VF f)) = toJSON f
    toJSON (JSONValue (VN num)) = toJSON $ show num
    toJSON (JSONValue (VR r)) = toJSON $ show r
    toJSON (JSONValue (VB b)) = toJSON b
    toJSON (JSONValue (VX xs)) = toJSON xs
    toJSON (JSONValue (VPattern pat)) = toJSON $ show pat
    toJSON (JSONValue (VState f)) = toJSON $ show $ f Map.empty
    toJSON (JSONValue (VList vs)) = toJSON $ map JSONValue vs

newtype JSONArcF = JSONArcF (ArcF Rational)
  deriving (Generic)

instance ToJSON JSONArcF where
    toJSON (JSONArcF (Arc arcStart arcStop)) =
        object ["start" .= (realToFrac arcStart :: Double), 
                "stop"  .= (realToFrac arcStop :: Double)]

newtype JSONEventF = JSONEventF (Event (Map.Map String Value))
  deriving (Generic)

instance ToJSON JSONEventF where
    toJSON (JSONEventF (Event _ctx evWhole evPart evValue)) =
        object [ "whole"   .= fmap JSONArcF evWhole  -- Handle Maybe
               , "part"    .= JSONArcF evPart
               , "value"   .= fmap JSONValue evValue ]



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
    Aeson.object ["arcLen" .= arcLen, "events" .= map JSONEventF events]

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

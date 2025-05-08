-- Released under terms of GNU Public License version 3
-- (c) Alex McLean 2017

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Drum.Client where

import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Exception       (try)
import           Control.Monad           (when)
import           Control.Monad.Trans     (liftIO)
import           Data.List
import           Data.Maybe
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.IO            as T
import           Data.Unique
import qualified Network.WebSockets      as WS
import           System.Directory
import           System.Environment      (getArgs, lookupEnv)
import           System.FilePath
import           System.IO

-- import Data.Ratio
-- import System.Process
-- import Data.Time.Clock.POSIX
-- import Data.Fixed (mod')
-- import Text.JSON
-- import qualified Data.ByteString.Lazy.Char8 as C
-- import Control.Applicative

port = 6010

run f = do addr <- fromMaybe "127.0.0.1" <$> lookupEnv "CIRCLE_ADDR"
           port <- fromMaybe "6010" <$> lookupEnv "CIRCLE_PORT"
           WS.runClient addr (read port) "/" f

snapshot snapName conn =
  do WS.sendTextData conn $ T.pack $ "/takeSnapshots " ++ snapName
     msg <- WS.receiveData conn
     putStrLn $ T.unpack msg
     msg <- WS.receiveData conn
     putStrLn $ T.unpack msg

loadSnapshot snapName conn =
  do WS.sendTextData conn $ T.pack $ "/loadSnapshot " ++ snapName
     msg <- WS.receiveData conn
     putStrLn $ T.unpack msg

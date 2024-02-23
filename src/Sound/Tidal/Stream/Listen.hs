module Sound.Tidal.Stream.Listen where

import           Data.Maybe (fromJust, catMaybes, isJust)
import           Control.Concurrent.MVar
import           Control.Monad (when)
import           System.IO (hPutStrLn, stderr)
import qualified Data.Map as Map
import qualified Sound.Osc.Fd as O
import qualified Sound.Osc.Time.Timeout as O
import qualified Network.Socket         as N
import qualified Control.Exception as E

import           Sound.Tidal.ID
import           Sound.Tidal.Pattern

import           Sound.Tidal.Stream.Config
import           Sound.Tidal.Stream.Types
import           Sound.Tidal.Stream.UI

{-
    Listen.hs - logic for listening and acting on incoming OSC messages
    Copyright (C) 2020, Alex McLean and contributors

    This library is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this library.  If not, see <http://www.gnu.org/licenses/>.
-}


openListener :: Config -> IO (Maybe O.Udp)
openListener c
  | cCtrlListen c = catchAny run (\_ -> do verbose c "That port isn't available, perhaps another Tidal instance is already listening on that port?"
                                           return Nothing
                                 )
  | otherwise  = return Nothing
  where
        run = do sock <- O.udpServer (cCtrlAddr c) (cCtrlPort c)
                 when (cCtrlBroadcast c) $ N.setSocketOption (O.udpSocket sock) N.Broadcast 1
                 return $ Just sock
        catchAny :: IO a -> (E.SomeException -> IO a) -> IO a
        catchAny = E.catch

-- Listen to and act on OSC control messages
ctrlResponder :: Int -> Config -> Stream -> IO ()
ctrlResponder waits c (stream@(Stream {sListen = Just sock}))
  = do ms <- recvMessagesTimeout 2 sock
       if (null ms)
         then do checkHandshake -- there was a timeout, check handshake
                 ctrlResponder (waits+1) c stream
         else do mapM_ act ms
                 ctrlResponder 0 c stream
     where
        checkHandshake = do busses <- readMVar (sBusses stream)
                            when (null busses) $ do when  (waits == 0) $ verbose c $ "Waiting for SuperDirt (v.1.7.2 or higher).."
                                                    sendHandshakes stream

        act (O.Message "/dirt/hello" _) = sendHandshakes stream
        act (O.Message "/dirt/handshake/reply" xs) = do prev <- swapMVar (sBusses stream) $ bufferIndices xs
                                                        -- Only report the first time..
                                                        when (null prev) $ verbose c $ "Connected to SuperDirt."
                                                        return ()
          where
            bufferIndices [] = []
            bufferIndices (x:xs') | x == (O.AsciiString $ O.ascii "&controlBusIndices") = catMaybes $ takeWhile isJust $ map O.datum_integral xs'
                                  | otherwise = bufferIndices xs'
        -- External controller commands
        act (O.Message "/ctrl" (O.Int32 k:v:[]))
          = act (O.Message "/ctrl" [O.string $ show k,v])
        act (O.Message "/ctrl" (O.AsciiString k:v@(O.Float _):[]))
          = add (O.ascii_to_string k) (VF (fromJust $ O.datum_floating v))
        act (O.Message "/ctrl" (O.AsciiString k:O.AsciiString v:[]))
          = add (O.ascii_to_string k) (VS (O.ascii_to_string v))
        act (O.Message "/ctrl" (O.AsciiString k:O.Int32 v:[]))
          = add (O.ascii_to_string k) (VI (fromIntegral v))
        -- Stream playback commands
        act (O.Message "/mute" (k:[]))
          = withID k $ streamMute stream
        act (O.Message "/unmute" (k:[]))
          = withID k $ streamUnmute stream
        act (O.Message "/solo" (k:[]))
          = withID k $ streamSolo stream
        act (O.Message "/unsolo" (k:[]))
          = withID k $ streamUnsolo stream
        act (O.Message "/muteAll" [])
          = streamMuteAll stream
        act (O.Message "/unmuteAll" [])
          = streamUnmuteAll stream
        act (O.Message "/unsoloAll" [])
          = streamUnsoloAll stream
        act (O.Message "/hush" [])
          = streamHush stream
        act (O.Message "/silence" (k:[]))
          = withID k $ streamSilence stream
        act m = hPutStrLn stderr $ "Unhandled OSC: " ++ show m
        add :: String -> Value -> IO ()
        add k v = do sMap <- takeMVar (sStateMV stream)
                     putMVar (sStateMV stream) $ Map.insert k v sMap
                     return ()
        withID :: O.Datum -> (ID -> IO ()) -> IO ()
        withID (O.AsciiString k) func = func $ (ID . O.ascii_to_string) k
        withID (O.Int32 k) func = func $ (ID . show) k
        withID _ _ = return ()
ctrlResponder _ _ _ = return ()

verbose :: Config -> String -> IO ()
verbose c s = when (cVerbose c) $ putStrLn s

recvMessagesTimeout :: (O.Transport t) => Double -> t -> IO [O.Message]
recvMessagesTimeout n sock = fmap (maybe [] O.packetMessages) $ O.recvPacketTimeout n sock

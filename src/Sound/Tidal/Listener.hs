module Sound.Tidal.Listener where

import Sound.Tidal.Hint
import Sound.OSC.FD as O
import Control.Concurrent
import Control.Concurrent.MVar
import qualified Network.Socket as N

{-
https://github.com/tidalcycles/tidal-listener/wiki
-}

listenPort = 6011
remotePort = 6012

listen :: IO ()
listen = do -- start Haskell interpreter, with input and output mutable variables to
            -- communicate with it
            (mIn, mOut) <- startHint
            -- listen
            (remote_addr:_) <- N.getAddrInfo Nothing (Just "127.0.0.1") Nothing
            local <- udpServer "127.0.0.1" listenPort
            let (N.SockAddrInet _ a) = N.addrAddress remote_addr
                remote = N.SockAddrInet (fromIntegral remotePort) a
            loop mIn mOut local remote
              where
                loop mIn mOut local remote = 
                  do -- wait for, read and act on OSC message
                     m <- recvMessage local
                     act mIn mOut local remote m
                     loop mIn mOut local remote

startHint = do mIn <- newEmptyMVar
               mOut <- newEmptyMVar
               forkIO $ hintJob mIn mOut
               return (mIn, mOut)

act :: MVar String -> MVar Response -> UDP -> N.SockAddr -> Maybe O.Message -> IO ()
act mIn mOut local remote (Just (Message "/code" [ASCII_String a_ident, ASCII_String a_code])) =
  do let ident = ascii_to_string a_ident
         code = ascii_to_string a_code
         respond (HintOK pat) = sendOK remote ident
         respond (HintError s) = sendError remote ident s
     putMVar mIn code
     r <- takeMVar mOut
     respond r
     return ()
       where sendOK remote ident = O.sendTo local (O.p_message "/code/ok" [string ident]) remote
             sendError remote ident s = O.sendTo local (O.p_message "/code/error" [string ident, string s]) remote

act mIn mOut local remote (Just (Message "/ping" [])) = O.sendTo local (O.p_message "/pong" []) remote

act _ _ _ _ Nothing = do putStrLn "not a message?"
                         return ()
act _ _ _ _ (Just m) = do putStrLn $ "Unhandled message: " ++ show m
                          return ()

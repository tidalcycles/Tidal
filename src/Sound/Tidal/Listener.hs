module Sound.Tidal.Listener where

import Sound.Tidal.Hint
import Sound.OSC.FD as O
import Control.Concurrent
import Control.Concurrent.MVar
import qualified Network.Socket as N

{-

Run code, get ok or errors back

/highlights on, get stream of active code spans+durations back
/highlights off, 


-}

listenPort = 6011
remotePort = 6012
  
listen :: IO ()
listen = do (mIn, mOut) <- startHint
            udp <- udpServer "127.0.0.1" listenPort
            loop mIn mOut udp
              where
                loop mIn mOut udp = 
                  do m <- recvMessage udp
                     act mIn mOut udp m
                     loop mIn mOut udp


startHint = do mIn <- newEmptyMVar
               mOut <- newEmptyMVar
               hintJob mIn mOut
               return (mIn, mOut)

act :: MVar String -> MVar Response -> UDP -> Maybe O.Message -> IO ()
act mIn mOut local (Just (Message "/run" [ASCII_String a_ident, ASCII_String a_code])) =
  do (remote_addr:_) <- N.getAddrInfo Nothing (Just "127.0.0.1") Nothing
     let ident = ascii_to_string a_ident
         code = ascii_to_string a_code
         (N.SockAddrInet _ a) = N.addrAddress remote_addr
         remote = N.SockAddrInet (fromIntegral remotePort) a
         respond (HintOK pat) = sendOK remote ident
         respond (HintError s) = sendError remote ident s
     putMVar mIn code
     r <- takeMVar mOut
     respond r
     return ()
       where sendOK remote ident = O.sendTo local (O.p_message "/ok" [string ident]) remote
             sendError remote ident s = O.sendTo local (O.p_message "/error" [string ident, string s]) remote

act _ _ _ Nothing = do putStrLn "not a message?"
                       return ()
act _ _ _ (Just m) = do putStrLn $ "Unhandled message: " ++ show m
                        return ()

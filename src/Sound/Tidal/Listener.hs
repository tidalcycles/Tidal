module Sound.Tidal.Listener where

import Sound.Tidal.Hint
import Sound.OSC.FD as O
import Control.Concurrent
import Control.Concurrent.MVar
import qualified Network.Socket as N

{-
https://github.com/tidalcycles/tidal-listener/wiki
-}

data State = State {sIn :: MVar String,
                    sOut :: MVar Response,
                    sLocal :: UDP,
                    sRemote :: N.SockAddr
                   }

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
                st = State mIn mOut local remote
            loop st
              where
                loop st = 
                  do -- wait for, read and act on OSC message
                     m <- recvMessage (sLocal st)
                     st' <- act st m
                     loop st'

-- TODO - use Chan or TChan for in/out channels instead of mvars directly?
startHint = do mIn <- newEmptyMVar
               mOut <- newEmptyMVar
               forkIO $ hintJob mIn mOut
               return (mIn, mOut)

act :: State -> Maybe O.Message -> IO State
act st (Just (Message "/code" [ASCII_String a_ident, ASCII_String a_code])) =
  do let ident = ascii_to_string a_ident
         code = ascii_to_string a_code
         respond (HintOK pat) = sendOK ident
         respond (HintError s) = sendError ident s
     putMVar (sIn st) code
     r <- takeMVar (sOut st)
     respond r
     return st
       where sendOK ident =
               O.sendTo (sLocal st) (O.p_message "/code/ok" [string ident]) (sRemote st)
             sendError ident s =
               O.sendTo (sLocal st) (O.p_message "/code/error" [string ident, string s]) (sRemote st)

act st (Just (Message "/ping" [])) =
  do O.sendTo (sLocal st) (O.p_message "/pong" []) (sRemote st)
     return st

act st Nothing = do putStrLn "not a message?"
                    return st
act st (Just m) = do putStrLn $ "Unhandled message: " ++ show m
                     return st

{-# LANGUAGE RecordWildCards #-}
module Sound.Tidal.Listener where

import Sound.Tidal.Stream (streamGetCPS)
import qualified Sound.Tidal.Context as T
import Sound.Tidal.Hint
import Sound.Tidal.Listener.Config
import Sound.Osc.Fd as O
import Control.Concurrent
import qualified Network.Socket as N


data State = State {sIn :: MVar InterpreterMessage,
                    sOut :: MVar InterpreterResponse,
                    sLocal :: Udp,
                    sRemote :: N.SockAddr,
                    sStream :: T.Stream
                   }


-- | Start Haskell interpreter, with input and output mutable variables to
-- communicate with it
listenWithConfig :: Config -> IO ()
listenWithConfig Config{..} = do
            putStrLn $ "Starting Tidal Listener " ++ if noGHC then "without installed GHC" else "with installed GHC"
            putStrLn $ "Listening for OSC commands on port " ++ show listenPort
            putStrLn $ "Sending replies to port " ++ show replyPort

            --start the stream
            stream <- startListenerStream replyPort dirtPort

            mIn <- newEmptyMVar
            mOut <- newEmptyMVar

            putStrLn "Starting tidal interpreter.. "
            _ <- forkIO $ startHintJob True stream mIn mOut

            (remote_addr:_) <- N.getAddrInfo Nothing (Just "127.0.0.1") Nothing
            local <- udpServer "127.0.0.1" listenPort

            let (N.SockAddrInet _ a) = N.addrAddress remote_addr
                remote  = N.SockAddrInet (fromIntegral replyPort) a
                st      = State mIn mOut local remote stream
            loop st
              where
                loop st =
                  do -- wait for, read and act on OSC message
                     m <- recvMessage (sLocal st)
                     st' <- act st m
                     loop st'


act :: State -> Maybe O.Message -> IO State

-- ask the interpreter to execute a statment: statments are expressions of type IO a or bindings/definitions,
-- in case of execution of an action of type IO a, the interpreter will try to show a and send it back
-- if a doesn't have a Show instance, an error is thrown
act st (Just (Message "/eval" [AsciiString statement])) =
  do putMVar (sIn st) (MStat $ ascii_to_string statement)
     r <- takeMVar (sOut st)
     case r of
       RStat (Just x) -> O.sendTo (sLocal st) (O.p_message "/eval/value" [string x]) (sRemote st)
       RStat Nothing -> O.sendTo (sLocal st) (O.p_message "/eval/ok" []) (sRemote st)
       RError e -> O.sendTo (sLocal st) (O.p_message "/eval/error" [string e]) (sRemote st)
       _ -> return ()
     return st

-- ask the interpreter for the type of an expression
act st (Just (Message "/type" [AsciiString expression])) =
   do putMVar (sIn st) (MType $ ascii_to_string expression)
      r <- takeMVar (sOut st)
      case r of
        RType t -> O.sendTo (sLocal st) (O.p_message "/type/ok" [string t]) (sRemote st)
        RError e -> O.sendTo (sLocal st) (O.p_message "/type/error" [string e]) (sRemote st)
        _ -> return ()
      return st

act st (Just (Message "/load" [AsciiString path])) =
   do putMVar (sIn st) (MLoad $ ascii_to_string path)
      r <- takeMVar (sOut st)
      case r of
        RStat (Just x) -> O.sendTo (sLocal st) (O.p_message "/load/value" [string x]) (sRemote st) --cannot happen
        RStat Nothing -> O.sendTo (sLocal st) (O.p_message "/load/ok" []) (sRemote st)
        RError e -> O.sendTo (sLocal st) (O.p_message "/load/error" [string e]) (sRemote st)
        _ -> return ()
      return st

-- test if the listener is responsive
act st (Just (Message "/ping" [])) =
  do O.sendTo (sLocal st) (O.p_message "/pong" []) (sRemote st)
     return st

-- get the current cps of the running stream
act st (Just (Message "/cps" [])) =
  do cps <- streamGetCPS (sStream st)
     O.sendTo (sLocal st) (O.p_message "/cps" [float cps]) (sRemote st)
     return st

act st Nothing = do putStrLn "Not a message?"
                    return st
act st (Just m) = do putStrLn $ "Unhandled message: " ++ show m
                     return st

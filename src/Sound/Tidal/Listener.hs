module Sound.Tidal.Listener where

import Sound.Tidal.Stream (Target(..))
import qualified Sound.Tidal.Context as T
import Sound.Tidal.Hint
import Sound.OSC.FD as O
import Control.Concurrent
import Control.Concurrent.MVar
import qualified Network.Socket as N
import qualified Sound.Tidal.Tempo as Tempo

{-
https://github.com/tidalcycles/tidal-listener/wiki
-}

data State = State {sIn :: MVar String,
                    sOut :: MVar Response,
                    sLocal :: UDP,
                    sRemote :: N.SockAddr,
                    sStream :: T.Stream
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
            putStrLn $ "Listening for OSC commands on port " ++ show listenPort
            putStrLn $ "Sending replies to port " ++ show remotePort
            putStrLn "Starting tidal interpreter.. "
            let remoteTarget = Target {oName = "atom",
                                       oAddress = "127.0.0.1",
                                       oPort = remotePort,
                                       oBusPort = Nothing,
                                       oLatency = 0.1,
                                       oWindow = Nothing,
                                       oSchedule = T.Live,
                                       oHandshake = True}
            stream <- T.startStream T.defaultConfig [(T.superdirtTarget {oLatency = 0.1},
                                                      [T.superdirtShape]
                                                     ),
                                                     (remoteTarget,
                                                      [T.OSCContext "/code/highlight"]
                                                     )
                                                    ]
            let (N.SockAddrInet _ a) = N.addrAddress remote_addr
                remote  = N.SockAddrInet (fromIntegral remotePort) a
                st      = State mIn mOut local remote stream
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

getcps st = do tempo <- readMVar $ T.sTempoMV (sStream st)
               return (Tempo.cps tempo)

act :: State -> Maybe O.Message -> IO State
act st (Just (Message "/code" [ASCII_String a_ident, ASCII_String a_code])) =
  do let ident = ascii_to_string a_ident
         code = ascii_to_string a_code
     putMVar (sIn st) code
     r <- takeMVar (sOut st)
     respond ident r
     return st
       where respond ident (HintOK pat) =
               do T.streamReplace (sStream st) ident pat
                  O.sendTo (sLocal st) (O.p_message "/code/ok" [string ident]) (sRemote st)
             respond ident (HintError s) =
               O.sendTo (sLocal st) (O.p_message "/code/error" [string ident, string s]) (sRemote st)

act st (Just (Message "/ping" [])) =
  do O.sendTo (sLocal st) (O.p_message "/pong" []) (sRemote st)
     return st

act st (Just (Message "/cps" [])) =
  do cps <- getcps st
     O.sendTo (sLocal st) (O.p_message "/cps" [float cps]) (sRemote st)
     return st

act st Nothing = do putStrLn "not a message?"
                    return st
act st (Just m) = do putStrLn $ "Unhandled message: " ++ show m
                     return st

> {-# LANGUAGE OverloadedStrings #-}
> import Data.Char (isPunctuation, isSpace)
> import Data.Monoid (mappend)
> import Data.Text (Text)
> import Control.Exception (fromException)
> import Control.Monad (forM_, forever)
> import Control.Concurrent (MVar, newMVar, modifyMVar_, readMVar, putMVar, takeMVar)
> import Control.Monad.IO.Class (liftIO)
> import qualified Data.Text as T
> import qualified Data.Text.IO as T
> import Data.Maybe
> import Data.Time (getCurrentTime, UTCTime, diffUTCTime)
> import qualified Network.WebSockets as WS

> import Tempo

> type Client = WS.Sink WS.Hybi00

> type ClientState = [Client]

(Time, Beat, Tempo)

> updateTempo :: MVar Tempo -> Maybe Double -> IO ()
> updateTempo mt Nothing = return ()
> updateTempo mt (Just bps') = do t <- takeMVar mt
>                                 now <- getCurrentTime
>                                 let delta = realToFrac $ diffUTCTime now (at t)
>                                     beat' = (beat t) + ((bps t) * delta)
>                                 putMVar mt $ Tempo now beat' bps'


> newTempo :: IO (Tempo)
> newTempo = do start <- getCurrentTime
>               return $ Tempo start 0 1

> newClientState :: ClientState
> newClientState = []

> numClients :: ClientState -> Int
> numClients = length

> addClient :: Client -> ClientState -> ClientState
> addClient client clients = client : clients

> removeClient :: Client -> ClientState -> ClientState
> removeClient client = filter (/= client)

> broadcast :: Text -> ClientState -> IO ()
> broadcast message clients = do
>     T.putStrLn message
>     forM_ clients $ \sink -> WS.sendSink sink $ WS.textData message

> main :: IO ()
> main = do
>     foo <- newTempo
>     tempoState <- newMVar foo
>     clientState <- newMVar newClientState
>     WS.runServer "0.0.0.0" 9160 $ application tempoState clientState

> application :: MVar Tempo -> MVar ClientState -> WS.Request -> WS.WebSockets WS.Hybi00 ()
> application tempoState clientState rq = do
>     WS.acceptRequest rq
>     WS.getVersion >>= liftIO . putStrLn . ("Client version: " ++)
>     sink <- WS.getSink
>     tempo <- liftIO $ readMVar tempoState
>     liftIO $ WS.sendSink sink $ WS.textData $ T.pack $ show tempo
>     clients <- liftIO $ readMVar clientState
>     liftIO $ modifyMVar_ clientState $ \s -> return $ addClient sink s
>     talk tempoState clientState sink

> talk :: WS.Protocol p => MVar Tempo -> MVar ClientState -> Client -> WS.WebSockets p ()
> talk tempoState clientState client = flip WS.catchWsError catchDisconnect $ 
>   forever $ do
>     msg <- WS.receiveData
>     liftIO $ putStrLn $ "Got message: " ++ (T.unpack msg)
>     liftIO $ updateTempo tempoState $ maybeRead $ T.unpack msg
>     tempo <- liftIO $ readMVar tempoState
>     liftIO $ readMVar clientState >>= broadcast (T.pack $ show tempo)
>   where
>     catchDisconnect e = case fromException e of
>         Just WS.ConnectionClosed -> liftIO $ modifyMVar_ clientState $ \s -> do
>             let s' = removeClient client s
>             --broadcast ("someone disconnected") s'
>             return s'
>         _ -> return ()

> maybeRead = fmap fst . listToMaybe . reads

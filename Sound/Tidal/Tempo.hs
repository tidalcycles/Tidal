{-# LANGUAGE ScopedTypeVariables #-}
module Sound.Tidal.Tempo where

import Data.Time (getCurrentTime, UTCTime, diffUTCTime)
import Data.Time.Clock.POSIX
import Control.Monad (forM_, forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Monad.Trans (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS
import qualified Control.Exception as E
import qualified System.IO.Error as Error
import GHC.Conc.Sync (ThreadId)
import System.Environment (getEnv)

import Sound.Tidal.Utils

data Tempo = Tempo {at :: UTCTime, beat :: Double, bps :: Double}

type ClientState = [WS.Connection]

instance Eq WS.Connection

instance Show Tempo where
  show x = show (at x) ++ "," ++ show (beat x) ++ "," ++ show (bps x)

getClockIp :: IO String
getClockIp = getEnvDefault "127.0.0.1" "TEMPO_ADDR"

getServerPort :: IO Int
getServerPort = fmap read (getEnvDefault "9160" "TIDAL_SERVER_PORT")

readTempo :: String -> Tempo
readTempo x = Tempo (read a) (read b) (read c)
  where (a:b:c:_) = wordsBy (== ',') x

logicalTime :: Tempo -> Double -> Double
logicalTime t b = changeT + timeDelta
  where beatDelta = b - (beat t)
        timeDelta = beatDelta / (bps t)
        changeT = realToFrac $ utcTimeToPOSIXSeconds $ at t

tempoMVar :: IO (MVar (Tempo))
tempoMVar = do now <- getCurrentTime
               mv <- newMVar (Tempo now 0 (126/60))
               forkIO $ clocked $ f mv
               return mv
  where f mv change _ = do swapMVar mv change
                           return ()

beatNow :: Tempo -> IO (Double)
beatNow t = do now <- getCurrentTime
               let delta = realToFrac $ diffUTCTime now (at t)
               let beatDelta = bps t * delta               
               return $ beat t + beatDelta

clientApp :: MVar Tempo -> MVar Double -> WS.ClientApp ()
clientApp mTempo mBps conn = do
  --sink <- WS.getSink
    liftIO $ forkIO $ sendBps conn mBps
    forever loop
  where
    loop = do
        msg <- WS.receiveData conn
        let tempo = readTempo $ T.unpack msg
        liftIO $ tryTakeMVar mTempo
        liftIO $ putMVar mTempo tempo

sendBps :: WS.Connection -> MVar Double -> IO ()
sendBps conn mBps = forever $ do
    bps <- takeMVar mBps 
    WS.sendTextData conn (T.pack $ show bps)

connectClient :: Bool -> String -> MVar Tempo -> MVar Double -> IO ()
connectClient secondTry ip mTempo mBps = do 
  let errMsg = "Failed to connect to tidal server. Try specifying a " ++
               "different port (default is 9160) setting the " ++
               "environment variable TIDAL_SERVER_PORT"
  serverPort <- getServerPort
  WS.runClient ip serverPort "/tempo" (clientApp mTempo mBps) `E.catch` 
    \(_ :: E.SomeException) -> do
      case secondTry of
        True -> error errMsg
        _ -> do
          res <- E.try (void startServer)
          case res of
            Left (_ :: E.SomeException) -> error errMsg
            Right _ -> do
              threadDelay 500000
              connectClient True ip mTempo mBps

runClient :: IO ((MVar Tempo, MVar Double))
runClient = 
  do clockip <- getClockIp
     mTempo <- newEmptyMVar 
     mBps <- newEmptyMVar 
     forkIO $ connectClient False clockip mTempo mBps
     return (mTempo, mBps)

bpsSetter :: IO (Double -> IO ())
bpsSetter = do (_, mBps) <- runClient 
               return $ (\b -> putMVar mBps b)

clocked :: (Tempo -> Int -> IO ()) -> IO ()
clocked callback = 
  do (mTempo, mBps) <- runClient
     t <- readMVar mTempo
     now <- getCurrentTime
     let delta = realToFrac $ diffUTCTime now (at t)
         beatDelta = bps t * delta
         nowBeat = beat t + beatDelta
         nextBeat = ceiling nowBeat
         -- next4 = nextBeat + (4 - (nextBeat `mod` 4))
     loop mTempo nextBeat
  where loop mTempo b = 
          do t <- readMVar mTempo
             now <- getCurrentTime
             let delta = realToFrac $ diffUTCTime now (at t)
                 actualBeat = (beat t) + ((bps t) * delta)
                 beatDelta = (fromIntegral b) - actualBeat
                 delay = beatDelta / (bps t)
             threadDelay $ floor (delay * 1000000)
             callback t b
             loop mTempo $ b + 1

clockedTick :: Int -> (Tempo -> Int -> IO ()) -> IO ()
clockedTick tpb callback = 
  do (mTempo, mBps) <- runClient
     t <- readMVar mTempo
     now <- getCurrentTime
     let delta = realToFrac $ diffUTCTime now (at t)
         beatDelta = bps t * delta
         nowBeat = beat t + beatDelta
         nextTick = ceiling (nowBeat * (fromIntegral tpb))
         -- next4 = nextBeat + (4 - (nextBeat `mod` 4))
     loop mTempo nextTick
  where loop mTempo tick = 
          do t <- readMVar mTempo
             now <- getCurrentTime
             let tps = (fromIntegral tpb) * bps t
                 delta = realToFrac $ diffUTCTime now (at t)
                 actualTick = ((fromIntegral tpb) * beat t) + (tps * delta)
                 tickDelta = (fromIntegral tick) - actualTick
                 delay = tickDelta / tps
             threadDelay $ floor (delay * 1000000)
             callback t tick
             loop mTempo $ tick + 1

updateTempo :: MVar Tempo -> Maybe Double -> IO ()
updateTempo mt Nothing = return ()
updateTempo mt (Just bps') = do t <- takeMVar mt
                                now <- getCurrentTime
                                let delta = realToFrac $ diffUTCTime now (at t)
                                    beat' = (beat t) + ((bps t) * delta)
                                putMVar mt $ Tempo now beat' bps'

addClient :: WS.Connection -> ClientState -> ClientState
addClient client clients = client : clients
  
removeClient :: WS.Connection -> ClientState -> ClientState
removeClient client = filter (/= client)

broadcast :: Text -> ClientState -> IO ()
broadcast message clients = do
  T.putStrLn message
  forM_ clients $ \conn -> WS.sendTextData conn $ message

startServer :: IO (ThreadId)
startServer = do
  serverPort <- getServerPort
  start <- getCurrentTime
  tempoState <- newMVar (Tempo start 0 1)
  clientState <- newMVar []
  forkIO $ WS.runServer "0.0.0.0" serverPort $ serverApp tempoState clientState

serverApp :: MVar Tempo -> MVar ClientState -> WS.ServerApp
serverApp tempoState clientState pending = do
    conn <- WS.acceptRequest pending
    tempo <- liftIO $ readMVar tempoState
    liftIO $ WS.sendTextData conn $ T.pack $ show tempo
    clients <- liftIO $ readMVar clientState
    liftIO $ modifyMVar_ clientState $ \s -> return $ addClient conn s
    serverLoop conn tempoState clientState

serverLoop :: WS.Connection -> MVar Tempo -> MVar ClientState -> IO ()
serverLoop conn tempoState clientState = E.handle catchDisconnect $ 
  forever $ do
    msg <- WS.receiveData conn
    liftIO $ updateTempo tempoState $ maybeRead $ T.unpack msg
    tempo <- liftIO $ readMVar tempoState
    liftIO $ readMVar clientState >>= broadcast (T.pack $ show tempo) 
  where
    catchDisconnect e = case E.fromException e of
        Just WS.ConnectionClosed -> liftIO $ modifyMVar_ clientState $ \s -> do
            let s' = removeClient conn s
            return s'
        _ -> return ()


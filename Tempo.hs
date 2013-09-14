module Tempo where

import Data.Time (getCurrentTime, UTCTime, diffUTCTime)
import Data.Time.Clock.POSIX
import Control.Monad (forM_, forever)
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

import Utils

data Tempo = Tempo {at :: UTCTime, beat :: Double, bps :: Double}

type Client = WS.Sink WS.Hybi00
type ClientState = [Client]

instance Show Tempo where
  show x = show (at x) ++ "," ++ show (beat x) ++ "," ++ show (bps x)

getClockIp :: IO (String)
getClockIp = do addr <- E.try (getEnv "TEMPO_ADDR")
                return $ either (const "127.0.0.1") (id) (addr :: Either E.IOException String)

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
               mv <- newMVar (Tempo now 0 1)
               forkIO $ clocked $ f mv
               return mv
  where f mv change _ = do swapMVar mv change
                           return ()

beatNow :: Tempo -> IO (Double)
beatNow t = do now <- getCurrentTime
               let delta = realToFrac $ diffUTCTime now (at t)
               let beatDelta = bps t * delta               
               return $ beat t + beatDelta

clientApp :: MVar Tempo -> MVar Double -> WS.WebSockets WS.Hybi10 ()
clientApp mTempo mBps = do
    sink <- WS.getSink
    liftIO $ forkIO $ sendBps sink mBps
    forever loop
  where
    loop = do
        msg <- WS.receiveData
        let tempo = readTempo $ T.unpack msg
        liftIO $ tryTakeMVar mTempo
        liftIO $ putMVar mTempo tempo

sendBps :: (WS.TextProtocol p) => WS.Sink p -> MVar Double -> IO ()
sendBps sink mBps = forever $ do
    bps <- takeMVar mBps 
    WS.sendSink sink $ WS.textData $ T.pack $ show bps

connectClient clockip mTempo mBps = do 
  E.handle
    ((\err -> 
      do putStrLn "Couldn't connect to tempo clock, starting local clock.."
         startServer
         threadDelay 500000
         cx "127.0.0.1"
     ) :: E.SomeException -> IO ())
    (cx clockip)
  where cx ip = WS.connect ip 9160 "/tempo" (clientApp mTempo mBps)

runClient :: IO ((MVar Tempo, MVar Double))
runClient = 
  do clockip <- getClockIp
     mTempo <- newEmptyMVar 
     mBps <- newEmptyMVar 
     forkIO $ connectClient clockip mTempo mBps
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
             --putStrLn $ "tick: " ++ (show tick) ++ " actualTick " ++ (show actualTick)
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

addClient :: Client -> ClientState -> ClientState
addClient client clients = client : clients

removeClient :: Client -> ClientState -> ClientState
removeClient client = filter (/= client)

broadcast :: Text -> ClientState -> IO ()
broadcast message clients = do
    T.putStrLn message
    forM_ clients $ \sink -> WS.sendSink sink $ WS.textData message

startServer :: IO (ThreadId)
startServer = do
    start <- getCurrentTime
    tempoState <- newMVar (Tempo start 0 1)
    clientState <- newMVar []
    forkIO $ WS.runServer "0.0.0.0" 9160 $ serverApp tempoState clientState

serverApp :: MVar Tempo -> MVar ClientState -> WS.Request -> WS.WebSockets WS.Hybi00 ()
serverApp tempoState clientState rq = do
    WS.acceptRequest rq
    -- WS.getVersion >>= liftIO . putStrLn . ("Client version: " ++)
    sink <- WS.getSink
    tempo <- liftIO $ readMVar tempoState
    liftIO $ WS.sendSink sink $ WS.textData $ T.pack $ show tempo
    clients <- liftIO $ readMVar clientState
    liftIO $ modifyMVar_ clientState $ \s -> return $ addClient sink s
    serverLoop tempoState clientState sink

serverLoop :: WS.Protocol p => MVar Tempo -> MVar ClientState -> Client -> WS.WebSockets p ()
serverLoop tempoState clientState client = flip WS.catchWsError catchDisconnect $ 
  forever $ do
    msg <- WS.receiveData
    liftIO $ updateTempo tempoState $ maybeRead $ T.unpack msg
    tempo <- liftIO $ readMVar tempoState
    liftIO $ readMVar clientState >>= broadcast (T.pack $ show tempo)
  where
    catchDisconnect e = case E.fromException e of
        Just WS.ConnectionClosed -> liftIO $ modifyMVar_ clientState $ \s -> do
            let s' = removeClient client s
            return s'
        _ -> return ()


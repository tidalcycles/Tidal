module TempoClient where

import Control.Monad (forever)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Monad.Trans (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS
import Data.Time (getCurrentTime, UTCTime, diffUTCTime)

import Tempo

app :: MVar (Tempo) -> WS.WebSockets WS.Hybi10 ()
app mt = do
    liftIO $ print "Connected"
    -- Fork off a separate thread that reads from stdin and writes to the sink.
    sink <- WS.getSink
    --liftIO $ forkIO $ readInput sink
    forever loop
  where
    loop = do
        msg <- WS.receiveData
        let tempo = readTempo $ T.unpack msg
        liftIO $ tryTakeMVar mt
        liftIO $ putMVar mt tempo
        liftIO $ putStrLn $ show $ tempo

readInput :: (WS.TextProtocol p) => WS.Sink p -> IO ()
readInput sink = forever $ do
    line <- T.getLine
    WS.sendSink sink $ WS.textData line

run :: String -> IO (MVar Tempo)
run clockip = do mt <- newEmptyMVar 
                 forkIO $ WS.connect clockip 9160 "/chat" (app mt)
                 return mt
         

clocked :: String -> (Tempo -> Int -> IO ()) -> IO ()
clocked clockip callback = 
  do mt <- run clockip
     t <- readMVar mt
     now <- getCurrentTime
     let delta = realToFrac $ diffUTCTime now (at t)
         beatDelta = bps t * delta
         nowBeat = beat t + beatDelta
         nextBeat = ceiling nowBeat
         -- next4 = nextBeat + (4 - (nextBeat `mod` 4))
     loop mt nextBeat
  where loop mt b = do t <- readMVar mt
                       now <- getCurrentTime
                       let delta = realToFrac $ diffUTCTime now (at t)
                           actualBeat = (beat t) + ((bps t) * delta)
                           beatDelta = (fromIntegral b) - actualBeat
                           delay = beatDelta / (bps t)
                       threadDelay $ floor (delay * 1000000)
                       callback t b
                       loop mt $ b + 1

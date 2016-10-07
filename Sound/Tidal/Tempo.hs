{-# LANGUAGE ScopedTypeVariables #-}
module Sound.Tidal.Tempo where

import Data.Time (getCurrentTime, UTCTime, NominalDiffTime, diffUTCTime, addUTCTime)
import Data.Time.Clock.POSIX
import Control.Applicative ((<$>), (<*>))
import Control.Monad (forM_, forever, void)
--import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Monad.Trans (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Unique
import qualified Network.WebSockets as WS
import qualified Control.Exception as E
import qualified System.IO.Error as Error
import GHC.Conc.Sync (ThreadId)
import System.Environment (getEnv)
import Sound.OSC.FD

import Sound.Tidal.Utils

data Tempo = Tempo {at :: UTCTime, beat :: Double, cps :: Double, paused :: Bool, clockLatency :: Double}

type ClientState = [TConnection]

data TConnection = TConnection Unique WS.Connection

wsConn :: TConnection -> WS.Connection
wsConn (TConnection _ c) = c

instance Eq TConnection where
   TConnection a _ == TConnection b _ = a == b

instance Show Tempo where
  show x = show (at x) ++ "," ++ show (beat x) ++ "," ++ show (cps x) ++ "," ++ show (paused x) ++ "," ++ (show $ clockLatency x)

getLatency :: IO Double
getLatency = fmap (read) (getEnvDefault "0.04" "TIDAL_CLOCK_LATENCY")

getClockIp :: IO String
getClockIp = getEnvDefault "127.0.0.1" "TIDAL_TEMPO_IP"

getServerPort :: IO Int
getServerPort = fmap read (getEnvDefault "9160" "TIDAL_TEMPO_PORT")

readTempo :: String -> Tempo
readTempo x = Tempo (read a) (read b) (read c) (read d) (read e)
  where (a:b:c:d:e:_) = wordsBy (== ',') x

logicalTime :: Tempo -> Double -> Double
logicalTime t b = changeT + timeDelta
  where beatDelta = b - (beat t)
        timeDelta = beatDelta / (cps t)
        changeT = realToFrac $ utcTimeToPOSIXSeconds $ at t

tempoMVar :: IO (MVar (Tempo))
tempoMVar = do now <- getCurrentTime
               l <- getLatency
               mv <- newMVar (Tempo now 0 0.5 False l)
               forkIO $ clocked $ f mv
               return mv
  where f mv change _ = do swapMVar mv change
                           return ()

beatNow :: Tempo -> IO (Double)
beatNow t = do now <- getCurrentTime
               let delta = realToFrac $ diffUTCTime now (at t)
               let beatDelta = cps t * delta               
               return $ beat t + beatDelta

clientApp :: MVar Tempo -> MVar Double -> WS.ClientApp ()
clientApp mTempo mCps conn = do
  --sink <- WS.getSink
    liftIO $ forkIO $ sendCps conn mTempo mCps
    forever loop
  where
    loop = do
        msg <- WS.receiveData conn
        let s = T.unpack msg
        let tempo = readTempo $ s
        old <- liftIO $ tryTakeMVar mTempo
        -- putStrLn $ "from: " ++ show old
        -- putStrLn $ "to: " ++ show tempo
        liftIO $ putMVar mTempo tempo

sendCps :: WS.Connection -> MVar Tempo -> MVar Double -> IO ()
sendCps conn mTempo mCps = forever $ do
    cps <- takeMVar mCps
    t <- readMVar mTempo
    t' <- updateTempo t cps
    WS.sendTextData conn (T.pack $ show t')

connectClient :: Bool -> String -> MVar Tempo -> MVar Double -> IO ()
connectClient secondTry ip mTempo mCps = do 
  let errMsg = "Failed to connect to tidal server. Try specifying a " ++
               "different port (default is 9160) setting the " ++
               "environment variable TIDAL_TEMPO_PORT"
  serverPort <- getServerPort
  WS.runClient ip serverPort "/tempo" (clientApp mTempo mCps) `E.catch` 
    \(_ :: E.SomeException) -> do
      case secondTry of
        True -> error errMsg
        _ -> do
          res <- E.try (void startServer)
          case res of
            Left (_ :: E.SomeException) -> error errMsg
            Right _ -> do
              threadDelay 500000
              connectClient True ip mTempo mCps

runClient :: IO ((MVar Tempo, MVar Double))
runClient = 
  do clockip <- getClockIp
     mTempo <- newEmptyMVar 
     mCps <- newEmptyMVar 
     forkIO $ connectClient False clockip mTempo mCps
     return (mTempo, mCps)

cpsUtils :: IO ((Double -> IO (), IO (Rational)))
cpsUtils = do (mTempo, mCps) <- runClient
              let currentTime = do tempo <- readMVar mTempo
                                   now <- beatNow tempo
                                   return $ toRational now
              return (putMVar mCps, currentTime)

-- Backwards compatibility
bpsUtils :: IO ((Double -> IO (), IO (Rational)))
bpsUtils = cpsUtils

cpsSetter :: IO (Double -> IO ())
cpsSetter = do (f, _) <- cpsUtils
               return f
clocked :: (Tempo -> Int -> IO ()) -> IO ()
clocked = clockedTick 1

{-
clocked callback = 
  do (mTempo, mCps) <- runClient
     t <- readMVar mTempo
     now <- getCurrentTime
     let delta = realToFrac $ diffUTCTime now (at t)
         beatDelta = cps t * delta
         nowBeat = beat t + beatDelta
         nextBeat = ceiling nowBeat
         -- next4 = nextBeat + (4 - (nextBeat `mod` 4))
     loop mTempo nextBeat
  where loop mTempo b = 
          do t <- readMVar mTempo
             b' <- doSlice t b
             loop mTempo $ b'
        -- wait an eighth of a cycle if we're paused
        doSlice t b | paused t = threadDelay $ floor ((0.125 / cps t) * 1000000)
                    | otherwise =
          do now <- getCurrentTime
             let delta = realToFrac $ diffUTCTime now (at t)
                 actualBeat = (beat t) + ((cps t) * delta)
                 beatDelta = (fromIntegral b) - actualBeat
                 delay = beatDelta / (cps t)
             threadDelay $ floor (delay * 1000000)
             callback t b
             return $ b + 1
-}
                         
clockedTick :: Int -> (Tempo -> Int -> IO ()) -> IO ()
clockedTick tpb callback = 
  do (mTempo, _mCps) <- runClient
     t <- readMVar mTempo
     now <- getCurrentTime
     let delta = realToFrac $ diffUTCTime now (at t)
         beatDelta = cps t * delta
         nowBeat = beat t + beatDelta
         nextTick = ceiling (nowBeat * (fromIntegral tpb))
         -- next4 = nextBeat + (4 - (nextBeat `mod` 4))
     loop mTempo nextTick
  where loop mTempo tick = 
          do tempo <- readMVar mTempo
             tick' <- doTick tempo tick
             loop mTempo tick'
        doTick tempo tick | paused tempo =
          do let pause = 0.01
             -- TODO - do this via blocking read on the mvar somehow
             -- rather than polling
             threadDelay $ floor (pause * 1000000)
             -- reset tick to 0 if cps is negative
             return $ if cps tempo < 0 then 0 else tick
                          | otherwise =
          do now <- getCurrentTime
             let tps = (fromIntegral tpb) * cps tempo
                 delta = realToFrac $ diffUTCTime now (at tempo)
                 actualTick = ((fromIntegral tpb) * beat tempo) + (tps * delta)
                 -- only wait by up to two ticks
                 tickDelta = min 2 $ (fromIntegral tick) - actualTick
                 delay = tickDelta / tps
             -- putStrLn $ "tick delta: " ++ show tickDelta
             --putStrLn ("Delay: " ++ show delay ++ "s Beat: " ++ show (beat tempo))
             threadDelay $ floor (delay * 1000000)
             callback tempo tick
             -- putStrLn $ "hmm diff: " ++ show (abs $ (floor actualTick) - tick)
             let newTick | (abs $ (floor actualTick) - tick) > 4 = floor actualTick
                         | otherwise = tick + 1
             return $ newTick

--updateTempo :: MVar Tempo -> Maybe Double -> IO ()
--updateTempo mt Nothing = return ()
--updateTempo mt (Just cps') = do t <- takeMVar mt
--                                now <- getCurrentTime
--                                let delta = realToFrac $ diffUTCTime now (at t)
--                                    beat' = (beat t) + ((cps t) * delta)
--                                putMVar mt $ Tempo now beat' cps' (paused t)


updateTempo :: Tempo -> Double -> IO (Tempo)
updateTempo t cps'
  | paused t == True && cps' > 0 =
    -- unpause
    do now <- getCurrentTime
       return $ t {at = addUTCTime (realToFrac $ clockLatency t) now, cps = cps', paused = False}
  | otherwise = 
    do now <- getCurrentTime
       let delta = realToFrac $ diffUTCTime now (at t)
           beat' = (beat t) + ((cps t) * delta)
           beat'' = if cps' < 0 then 0 else beat'
       return $ t {at = now, beat = beat'', cps = cps', paused = (cps' <= 0)}

removeClient :: TConnection -> ClientState -> ClientState
removeClient client = filter (/= client)

broadcast :: Text -> ClientState -> IO ()
broadcast message clients = do
  -- T.putStrLn message
  forM_ clients $ \conn -> WS.sendTextData (wsConn conn) $ message

startServer :: IO (ThreadId)
startServer = do
  serverPort <- getServerPort
  start <- getCurrentTime
  l <- getLatency
  tempoState <- newMVar (Tempo start 0 1 False l)
  clientState <- newMVar []
  liftIO $ oscBridge clientState
  forkIO $ WS.runServer "0.0.0.0" serverPort $ serverApp tempoState clientState

serverApp :: MVar Tempo -> MVar ClientState -> WS.ServerApp
serverApp tempoState clientState pending = do
    conn <- TConnection <$> newUnique <*> WS.acceptRequest pending
    tempo <- liftIO $ readMVar tempoState
    liftIO $ WS.sendTextData (wsConn conn) $ T.pack $ show tempo
    clients <- liftIO $ readMVar clientState
    liftIO $ modifyMVar_ clientState $ return . (conn:)
    serverLoop conn tempoState clientState

oscBridge :: MVar ClientState -> IO ()
oscBridge clientState =
  do -- putStrLn $ "start osc bridge"
     osc <- liftIO $ udpServer "0.0.0.0" 6060
     _ <- forkIO $ loop osc
     return ()
  where loop osc =
          do b <- recvBundle osc
             -- putStrLn $ "received bundle" ++ (show b)
             let timestamp = addUTCTime (realToFrac $ ntpr_to_ut $ bundleTime b) ut_epoch
                 msg = head $ bundleMessages b
                 -- todo - Data.Maybe version of !!
                 tick = datum_floating $ (messageDatum msg) !! 0
                 tempo = datum_floating $ (messageDatum msg) !! 1
                 address = messageAddress msg
             act address timestamp tick tempo
             loop osc
        act "/sync" timestamp (Just tick) (Just tempo)
          = do -- putStrLn $ "time " ++ show timestamp ++ " tick " ++ show tick ++ " tempo " ++ show tempo
               let t = Tempo {at = timestamp, beat = tick, cps = tempo,
                              paused = False,
                              clockLatency = 0
                             }
                   msg = T.pack $ show t
               clients <- readMVar clientState
               broadcast msg clients
               return ()
        act _ _ _ _  = return ()

serverLoop :: TConnection -> MVar Tempo -> MVar ClientState -> IO ()
serverLoop conn _tempoState clientState = E.handle catchDisconnect $
  forever $ do
    msg <- WS.receiveData $ wsConn conn
    --liftIO $ updateTempo tempoState $ maybeRead $ T.unpack msg
    liftIO $ readMVar clientState >>= broadcast msg
    --tempo <- liftIO $ readMVar tempoState
    -- liftIO $ readMVar clientState >>= broadcast (T.pack $ show tempo) 
  where
    catchDisconnect e = case E.fromException e of
        Just WS.ConnectionClosed -> liftIO $ modifyMVar_ clientState $ \s -> do
            let s' = removeClient conn s
            return s'
        _ -> return ()


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
import Data.Maybe (fromMaybe, maybe, isJust, fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Unique
import qualified Network.WebSockets as WS
import qualified Control.Exception as E
import Safe (readNote)
import System.Environment (lookupEnv)
import qualified System.IO.Error as Error
import GHC.Conc.Sync (ThreadId)
import Sound.OSC.Datum.Datem
import Sound.OSC.FD

import Sound.Tidal.Utils

data Tempo = Tempo {at :: UTCTime, beat :: Double, cps :: Double, paused :: Bool, clockLatency :: Double}

type ClientState = [TConnection]

data ServerMode = Master
                | Slave UDP

instance Show ServerMode where
  show Master = "Master"
  show _ = "Slave"

data TConnection = TConnection Unique WS.Connection

wsConn :: TConnection -> WS.Connection
wsConn (TConnection _ c) = c

instance Eq TConnection where
   TConnection a _ == TConnection b _ = a == b

instance Show Tempo where
  show x = show (at x) ++ "," ++ show (beat x) ++ "," ++ show (cps x) ++ "," ++ show (paused x) ++ "," ++ (show $ clockLatency x)

getLatency :: IO Double
getLatency =
   maybe 0.04 (readNote "latency parse") <$> lookupEnv "TIDAL_CLOCK_LATENCY"

getClockIp :: IO String
getClockIp = fromMaybe "127.0.0.1" <$> lookupEnv "TIDAL_TEMPO_IP"

getServerPort :: IO Int
getServerPort =
   maybe 9160 (readNote "port parse") <$> lookupEnv "TIDAL_TEMPO_PORT"

getMasterPort :: IO Int
getMasterPort =
   maybe 6042 (readNote "port parse") <$> lookupEnv "TIDAL_MASTER_PORT"

getSlavePort :: IO Int
getSlavePort =
   maybe 6043 (readNote "port parse") <$> lookupEnv "TIDAL_SLAVE_PORT"

readTempo :: String -> Tempo
readTempo x = Tempo (read a) (read b) (read c) (read d) (read e)
  where (a:b:c:d:e:_) = wordsBy (== ',') x

-- given a Tempo and a cycle position (aka "a beat")
-- returns the POSIX time of that cycle position (aka beat)
logicalTime :: Tempo -> Double -> Double
logicalTime t b = changeT + timeDelta
  where beatDelta = b - (beat t)
        timeDelta = beatDelta / (cps t)
        changeT = realToFrac $ utcTimeToPOSIXSeconds $ at t


-- beatNow: accesses a clock and returns the time now in terms of
-- beats relative to metrical grid of a given Tempo
beatNow :: Tempo -> IO (Double)
beatNow t = do now <- getCurrentTime
               let delta = realToFrac $ diffUTCTime now (at t)
               let beatDelta = cps t * delta
               return $ beat t + beatDelta

-- getCurrentBeat: given current Tempo grid, gets the current beat
getCurrentBeat :: MVar Tempo -> IO Rational
getCurrentBeat t = (readMVar t) >>= (beatNow) >>= (return . toRational)

clientApp :: MVar Tempo -> MVar Double -> MVar Double -> WS.ClientApp ()
clientApp mTempo mCps mNudge conn = do
    liftIO $ forkIO $ sendCps conn mCps
    liftIO $ forkIO $ sendNudge conn mNudge
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

sendTempo :: [WS.Connection] -> Tempo -> IO ()
sendTempo conns t = mapM_ (\conn -> WS.sendTextData conn (T.pack $ show t)) conns

sendCps :: WS.Connection -> MVar Double -> IO ()
sendCps conn mCps = forever $ do cps <- takeMVar mCps
                                 let m = "cps " ++ (show cps)
                                 WS.sendTextData conn (T.pack m)

sendNudge :: WS.Connection -> MVar Double -> IO ()
sendNudge conn mNudge = forever $ do nudge <- takeMVar mNudge
                                     let m = "nudge " ++ (show nudge)
                                     WS.sendTextData conn (T.pack m)

connectClient :: Bool -> String -> MVar Tempo -> MVar Double -> MVar Double -> IO ()
connectClient secondTry ip mTempo mCps mNudge = do
  let errMsg = "Failed to connect to tidal server. Try specifying a " ++
               "different port (default is 9160) setting the " ++
               "environment variable TIDAL_TEMPO_PORT"
  serverPort <- getServerPort
  WS.runClient ip serverPort "/tempo" (clientApp mTempo mCps mNudge) `E.catch`
    \(_ :: E.SomeException) -> do
      case secondTry of
        True -> error errMsg
        _ -> do
          res <- E.try (void startServer)
          case res of
            Left (_ :: E.SomeException) -> error errMsg
            Right _ -> do
              threadDelay 500000
              connectClient True ip mTempo mCps mNudge

runClient :: IO ((MVar Tempo, MVar Double, MVar Double))
runClient =
  do clockip <- getClockIp
     mTempo <- newEmptyMVar
     mCps <- newEmptyMVar
     mNudge <- newEmptyMVar
     forkIO $ connectClient False clockip mTempo mCps mNudge
     return (mTempo, mCps, mNudge)

cpsUtils' :: IO ((Double -> IO (), (Double -> IO ()), IO Rational))
cpsUtils' = do (mTempo, mCps, mNudge) <- runClient
               let cpsSetter = putMVar mCps
                   nudger = putMVar mNudge
                   currentTime = do tempo <- readMVar mTempo
                                    now <- beatNow tempo
                                    return $ toRational now
               return (cpsSetter, nudger, currentTime)

-- backward compatibility
cpsUtils = do (cpsSetter, _, currentTime) <- cpsUtils'
              return (cpsSetter, currentTime)

-- Backwards compatibility
bpsUtils :: IO ((Double -> IO (), IO (Rational)))
bpsUtils = cpsUtils

cpsSetter :: IO (Double -> IO ())
cpsSetter = do (f, _) <- cpsUtils
               return f

clocked :: (Tempo -> Int -> IO ()) -> IO ()
clocked = clockedTick 1
                         
clockedTick :: Int -> (Tempo -> Int -> IO ()) -> IO ()
clockedTick tpb callback =
  do (mTempo, _, mCps) <- runClient
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

nudgeTempo :: Tempo -> Double -> Tempo
nudgeTempo t secs = t {at = addUTCTime (realToFrac secs) (at t)}

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
  serverState <- newMVar Master
  --liftIO $ oscBridge clientState
  liftIO $ slave serverState clientState
  forkIO $ WS.runServer "0.0.0.0" serverPort $ serverApp tempoState serverState clientState

serverApp :: MVar Tempo -> MVar ServerMode -> MVar ClientState -> WS.ServerApp
serverApp tempoState serverState clientState pending = do
    conn <- TConnection <$> newUnique <*> WS.acceptRequest pending
    tempo <- liftIO $ readMVar tempoState
    liftIO $ WS.sendTextData (wsConn conn) $ T.pack $ show tempo
    clients <- liftIO $ readMVar clientState
    liftIO $ modifyMVar_ clientState $ return . (conn:)
    serverLoop conn tempoState serverState clientState

slave :: MVar ServerMode -> MVar ClientState -> IO ()
slave serverState clientState =
  do slavePort <- getSlavePort
     slaveSock <- udpServer "127.0.0.1" (fromIntegral slavePort)
     _ <- forkIO $ loop slaveSock
     return ()
  where loop slaveSock =
          do ms <- recvMessages slaveSock
             mapM_ (\m -> slaveAct (messageAddress m) serverState clientState m) ms
             loop slaveSock

slaveAct :: String -> MVar ServerMode -> MVar ClientState -> Message -> IO ()
slaveAct "/tempo" serverState clientState m
  | isJust t = do clients <- readMVar clientState
                  setSlave serverState
                  sendTempo (map wsConn clients) (fromJust t)
  | otherwise = return ()
  where t = do beat' <- datum_floating $ (messageDatum m) !! 2
               cps' <- datum_floating $ (messageDatum m) !! 3
               return $ Tempo {at = ut,
                               beat = beat',
                               cps = cps',
                               paused = False,
                               clockLatency = 0
                              }
        ut = addUTCTime (realToFrac $ dsec) ut_epoch
        sec = fromJust $ datum_int32 $ (messageDatum m) !! 0
        usec = fromJust $ datum_int32 $ (messageDatum m) !! 1
        dsec = ((fromIntegral sec) + ((fromIntegral usec) / 1000000)) :: Double

setSlave :: MVar ServerMode -> IO ()
setSlave serverState = do s <- takeMVar serverState
                          s' <- updateState s
                          putMVar serverState s'
                          return ()
     where updateState Master = do putStrLn "Slaving tempo.."
                                   masterPort <- getMasterPort
                                   sock <- openUDP "127.0.0.1" (fromIntegral masterPort)
                                   return (Slave sock)
           -- already slaving..
           updateState s = return s
                          
serverLoop :: TConnection -> MVar Tempo -> MVar ServerMode -> MVar ClientState -> IO ()
serverLoop conn tempoState serverState clientState = E.handle catchDisconnect $
  forever $ do
    msg <- WS.receiveData $ wsConn conn
    --liftIO $ updateTempo tempoState $ maybeRead $ T.unpack msg
    mode <- readMVar serverState
    serverAct (T.unpack msg) mode tempoState clientState
    -- 
    --tempo <- liftIO $ readMVar tempoState
    -- liftIO $ readMVar clientState >>= broadcast (T.pack $ show tempo)
  where
    catchDisconnect e = case E.fromException e of
        Just WS.ConnectionClosed -> liftIO $ modifyMVar_ clientState $ \s -> do
            let s' = removeClient conn s
            return s'
        _ -> return ()

serverAct :: String -> ServerMode -> MVar Tempo -> MVar ClientState -> IO ()
serverAct ('c':'p':'s':' ':n) mode tempoState clientState = setCps (read n) mode tempoState clientState
serverAct ('n':'u':'d':'g':'e':' ':n) mode tempoState clientState = setNudge (read n) mode tempoState clientState
serverAct s _ _ _ = do putStrLn $ "tempo server received unknown message " ++ s
                       return ()

setCps :: Double -> ServerMode -> MVar Tempo -> MVar ClientState -> IO ()
setCps n Master tempoState clientState = do tempo <- takeMVar tempoState
                                            tempo' <- updateTempo tempo (n :: Double)
                                            clients <- readMVar clientState
                                            sendTempo (map wsConn clients) (tempo')
                                            putMVar tempoState tempo'
                                            return ()
                                            
setCps n (Slave sock) tempoState clientState = sendOSC sock $ Message "/cps" [Float (realToFrac n)]


setNudge :: Double -> ServerMode -> MVar Tempo -> MVar ClientState -> IO ()
setNudge n Master tempoState clientState = do tempo <- takeMVar tempoState
                                              let tempo' = nudgeTempo tempo n
                                              clients <- readMVar clientState
                                              sendTempo (map wsConn clients) (tempo')
                                              putMVar tempoState tempo'
                                              return ()
                                              
setNudge n (Slave sock) tempoState clientState = sendOSC sock $ Message "/nudge" [Float (realToFrac n)]
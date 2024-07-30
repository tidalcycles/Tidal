{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Sound.Tidal.Stream.UI where

import           Control.Concurrent.MVar
import qualified Control.Exception          as E
import qualified Data.Map                   as Map
import           Data.Maybe                 (isJust)
import qualified Sound.Osc.Fd               as O
import           System.IO                  (hPutStrLn, stderr)
import           System.Random              (getStdRandom, randomR)

import qualified Sound.Tidal.Clock          as Clock
import           Sound.Tidal.Stream.Config
import           Sound.Tidal.Stream.Process
import           Sound.Tidal.Stream.Target
import           Sound.Tidal.Stream.Types

import           Sound.Tidal.ID
import           Sound.Tidal.Pattern

streamNudgeAll :: Stream -> Double -> IO ()
streamNudgeAll s = Clock.setNudge (sClockRef s)

streamResetCycles :: Stream -> IO ()
streamResetCycles s = streamSetCycle s 0

streamSetCycle :: Stream -> Time -> IO ()
streamSetCycle s = Clock.setClock (sClockRef s)

streamSetCPS :: Stream -> Time -> IO ()
streamSetCPS s = Clock.setCPS (cClockConfig $ sConfig s) (sClockRef s)

streamSetBPM :: Stream -> Time -> IO ()
streamSetBPM s = Clock.setBPM (sClockRef s)

streamGetCPS :: Stream -> IO Time
streamGetCPS s = Clock.getCPS (cClockConfig $ sConfig s)(sClockRef s)

streamGetBPM :: Stream -> IO Time
streamGetBPM s = Clock.getBPM (sClockRef s)

streamGetNow :: Stream -> IO Time
streamGetNow s = Clock.getCycleTime (cClockConfig $ sConfig s)(sClockRef s)

streamEnableLink :: Stream -> IO ()
streamEnableLink s = Clock.enableLink (sClockRef s)

streamDisableLink :: Stream -> IO ()
streamDisableLink s = Clock.disableLink (sClockRef s)

streamList :: Stream -> IO ()
streamList s = do pMap <- readMVar (sPMapMV s)
                  let hs = hasSolo pMap
                  putStrLn $ concatMap (showKV hs) $ Map.toList pMap
  where showKV :: Bool -> (PatId, PlayState) -> String
        showKV True  (k, (PlayState {psSolo = True})) = k ++ " - solo\n"
        showKV True  (k, _) = "(" ++ k ++ ")\n"
        showKV False (k, (PlayState {psSolo = False})) = k ++ "\n"
        showKV False (k, _) = "(" ++ k ++ ") - muted\n"

streamReplace :: Stream -> ID -> ControlPattern -> IO ()
streamReplace stream k !pat = do
                  t <- Clock.getCycleTime (cClockConfig $ sConfig stream) (sClockRef stream)
                  E.handle (\ (e :: E.SomeException) -> do
                    hPutStrLn stderr $ "Failed to Stream.streamReplace: " ++ show e
                    hPutStrLn stderr $ "Return to previous pattern."
                    setPreviousPatternOrSilence (sPMapMV stream)) (updatePattern stream k t pat)

-- streamFirst but with random cycle instead of always first cicle
streamOnce :: Stream -> ControlPattern -> IO ()
streamOnce st p = do i <- getStdRandom $ randomR (0, 8192)
                     streamFirst st $ rotL (toRational (i :: Int)) p

streamFirst :: Stream -> ControlPattern -> IO ()
streamFirst stream pat = onSingleTick (cClockConfig $ sConfig stream) (sClockRef stream) (sStateMV stream) (sBusses stream) (sPMapMV stream) (sGlobalFMV stream) (sCxs stream) (sListen stream) pat

streamMute :: Stream -> ID -> IO ()
streamMute s k = withPatIds s [k] (\x -> x {psMute = True})

streamMutes :: Stream -> [ID] -> IO ()
streamMutes s ks = withPatIds s ks (\x -> x {psMute = True})

streamUnmute :: Stream -> ID -> IO ()
streamUnmute s k = withPatIds s [k] (\x -> x {psMute = False})

streamSolo :: Stream -> ID -> IO ()
streamSolo s k = withPatIds s [k] (\x -> x {psSolo = True})

streamUnsolo :: Stream -> ID -> IO ()
streamUnsolo s k = withPatIds s [k] (\x -> x {psSolo = False})

withPatIds :: Stream -> [ID] -> (PlayState -> PlayState) -> IO ()
withPatIds s ks f
  = do playMap <- takeMVar $ sPMapMV s
       let pMap' = foldr (Map.update (\x -> Just $ f x)) playMap (map fromID ks)
       putMVar (sPMapMV s) pMap'
       return ()

-- TODO - is there a race condition here?
streamMuteAll :: Stream -> IO ()
streamMuteAll s = modifyMVar_ (sPMapMV s) $ return . fmap (\x -> x {psMute = True})

streamHush :: Stream -> IO ()
streamHush s = modifyMVar_ (sPMapMV s) $ return . fmap (\x -> x {psPattern = silence, psHistory = silence:psHistory x})

streamUnmuteAll :: Stream -> IO ()
streamUnmuteAll s = modifyMVar_ (sPMapMV s) $ return . fmap (\x -> x {psMute = False})

streamUnsoloAll :: Stream -> IO ()
streamUnsoloAll s = modifyMVar_ (sPMapMV s) $ return . fmap (\x -> x {psSolo = False})

streamSilence :: Stream -> ID -> IO ()
streamSilence s k = withPatIds s [k] (\x -> x {psPattern = silence, psHistory = silence:psHistory x})

streamAll :: Stream -> (ControlPattern -> ControlPattern) -> IO ()
streamAll s f = do _ <- swapMVar (sGlobalFMV s) f
                   return ()

streamGet :: Stream -> String -> IO (Maybe Value)
streamGet s k = Map.lookup k <$> readMVar (sStateMV s)

streamSet :: Valuable a => Stream -> String -> Pattern a -> IO ()
streamSet s k pat = do sMap <- takeMVar $ sStateMV s
                       let pat' = toValue <$> pat
                           sMap' = Map.insert k (VPattern pat') sMap
                       putMVar (sStateMV s) $ sMap'

streamSetI :: Stream -> String -> Pattern Int -> IO ()
streamSetI = streamSet

streamSetF :: Stream -> String -> Pattern Double -> IO ()
streamSetF = streamSet

streamSetS :: Stream -> String -> Pattern String -> IO ()
streamSetS = streamSet

streamSetB :: Stream -> String -> Pattern Bool -> IO ()
streamSetB = streamSet

streamSetR :: Stream -> String -> Pattern Rational -> IO ()
streamSetR = streamSet

-- It only really works to handshake with one target at the moment..
sendHandshakes :: Stream -> IO ()
sendHandshakes stream = mapM_ sendHandshake $ filter (oHandshake . cxTarget) (sCxs stream)
  where sendHandshake cx = if (isJust $ sListen stream)
                           then
                             do -- send it _from_ the udp socket we're listening to, so the
                                -- replies go back there
                                sendO False (sListen stream) cx $ O.Message "/dirt/handshake" []
                           else
                             hPutStrLn stderr "Can't handshake with SuperCollider without control port."

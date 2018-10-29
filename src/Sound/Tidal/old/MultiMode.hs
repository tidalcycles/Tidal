module Sound.Tidal.MultiMode where

import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad (forever)
import Control.Monad.Loops (iterateM_)
import Sound.OSC.FD
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Calendar (fromGregorian)

import Sound.Tidal.Tempo
import Sound.Tidal.Time as T
import Sound.Tidal.Stream
import Sound.Tidal.Dirt
import Sound.Tidal.EspGrid
import Sound.Tidal.Transition (transition)
import Sound.Tidal.Pattern (silence)

data StreamType = Dirt | SuperDirt

data SyncType = NoSync | Esp

initializeStreamType :: IO (MVar StreamType)
initializeStreamType = newMVar SuperDirt

changeStreamType :: MVar StreamType -> StreamType -> IO (IO StreamType)
changeStreamType mvar t = return (swapMVar mvar t)

initializeSyncType :: IO (MVar SyncType)
initializeSyncType = newMVar NoSync

changeSyncType :: MVar SyncType -> SyncType -> IO (IO SyncType)
changeSyncType mvar t = return (swapMVar mvar t)

type CpsUtils = (Double -> IO(), IO Rational)

multiModeCpsUtils :: CpsUtils -> CpsUtils -> MVar SyncType -> IO CpsUtils
multiModeCpsUtils (cpsNone,getNowNone) (cpsEsp,getNowEsp) mSync = return (cps,getNow)
  where cps x = do s <- readMVar mSync
                   case s of NoSync -> cpsNone x
                             Esp -> cpsEsp x
        getNow = do s <- readMVar mSync
                    case s of NoSync -> getNowNone
                              Esp -> getNowEsp

multiModeSetters :: IO Rational -> IO Rational -> MVar SyncType -> MVar StreamType -> IO (ParamPattern -> IO ())
multiModeSetters getNowNone getNowEsp mSync mStream = do
  (classicDirt,tClassic) <- dirtSetters getNowNone
  (espDirt,tEsp) <- dirtSettersEsp getNowEsp
  (superDirt,tSuper) <- superDirtSetters getNowNone
  (espSuperDirt,tEspSuper) <- superDirtSettersEsp getNowEsp
  let f NoSync Dirt p = do classicDirt p
                           espDirt silence
                           superDirt silence
                           espSuperDirt silence
      f Esp Dirt p = do espDirt p
                        classicDirt silence
                        superDirt silence
                        espSuperDirt silence
      f NoSync SuperDirt p = do superDirt p
                                classicDirt silence
                                espDirt silence
                                espSuperDirt silence
      f Esp SuperDirt p = do espSuperDirt p
                             classicDirt silence
                             espDirt silence
                             superDirt silence
  return $ \p -> readMVar mSync >>= \s -> readMVar mStream >>= \t -> f s t p

{-

Example of using the above definitions:
(note: if using Atom, evaluate each of the lines below one by one using shift-Enter)

syncType <- initializeSyncType
nosync <- changeSyncType syncType NoSync
esp <- changeSyncType syncType Esp
(cpsNone,getNowNone) <- cpsUtils
(cpsEsp,getNowEsp,getTempoEsp) <- cpsUtilsEsp
(cps,getNow) <- multiModeCpsUtils (cpsNone,getNowNone) (cpsEsp,getNowEsp) syncType

streamType <- initializeStreamType
classicDirt <- changeStreamType streamType Dirt
superDirt <- changeStreamType streamType SuperDirt
d1 <- multiModeSetters getNowNone getNowEsp syncType streamType
d2 <- multiModeSetters getNowNone getNowEsp syncType streamType
d3 <- multiModeSetters getNowNone getNowEsp syncType streamType
d4 <- multiModeSetters getNowNone getNowEsp syncType streamType
d5 <- multiModeSetters getNowNone getNowEsp syncType streamType
d6 <- multiModeSetters getNowNone getNowEsp syncType streamType
d7 <- multiModeSetters getNowNone getNowEsp syncType streamType
d8 <- multiModeSetters getNowNone getNowEsp syncType streamType
d9 <- multiModeSetters getNowNone getNowEsp syncType streamType
d10 <- multiModeSetters getNowNone getNowEsp syncType streamType

let bps x = cps (x/2)
let hush = mapM_ ($ silence) [d1,d2,d3,d4,d5,d6,d7,d8,d9,d10]
let solo = (>>) hush

then you can evaluate "classicDirt" to switch to classic Dirt
and "superDirt" to switch back to SuperDirt (the default)
and "esp" to turn on EspGrid-aware synchronization
and "nosync" to switch off EspGrid-aware synchronization (the default)
(switching between sync types is only noticeable after the next time a pattern is redefined right now)
-}

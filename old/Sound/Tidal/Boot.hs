{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Sound.Tidal.Boot
  ( Tidally (..)
  , OscMap
  , mkConfig
  , mkOscMap
  , mkTidal
  , mkTidalWith
  , only
  , p
  , hush
  , panic
  , list
  , mute
  , unmute
  , unmuteAll
  , unsoloAll
  , solo
  , unsolo
  , once
  , asap
  , first
  , nudgeAll
  , all
  , resetCycles
  , setCycle
  , setcps
  , getcps
  , getnow
  , d1
  , d2
  , d3
  , d4
  , d5
  , d6
  , d7
  , d8
  , d9
  , d10
  , d11
  , d12
  , d13
  , d14
  , d15
  , d16
  , getState
  , setI
  , setF
  , setS
  , setR
  , setB
  , module Sound.Tidal.Context
  )
where

{-
    Boot.hs - Shortcuts for using an in-scope Tidal Stream.
    Copyright (C) 2023, Alex McLean and contributors

    This library is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this library.  If not, see <http://www.gnu.org/licenses/>.
-}

import Prelude hiding (all, (*>), (<*))
import Sound.Tidal.Context hiding (mute, solo)
import Sound.Tidal.ID (ID)
import System.IO (hSetEncoding, stdout, utf8)

-- | Functions using this constraint can access the in-scope Tidal instance.
-- You must implement an instance of this in 'BootTidal.hs'. Note that GHC
-- will complain that it is an "orphan" instance, but that is ok.
class Tidally where
  tidal :: Stream

type OscMap = [(Target, [OSC])]

-- | A reasonable config.
mkConfig :: Config
mkConfig = defaultConfig {cVerbose = True, cFrameTimespan = 1 / 20}

-- | A reasonable OscMap
mkOscMap :: OscMap
mkOscMap = [(superdirtTarget {oLatency = 0.05, oAddress = "127.0.0.1", oPort = 57120}, [superdirtShape])]

-- | Creates a Tidal instance using default config. Use 'mkTidalWith' to customize.
mkTidal :: IO Stream
mkTidal = mkTidalWith mkConfig mkOscMap

-- | See 'Sound.Tidal.Stream.startStream'.
mkTidalWith :: Config -> OscMap -> IO Stream
mkTidalWith config oscmap = do
  hSetEncoding stdout utf8
  startStream config oscmap

-- | 'hush' then execute the given action.
only :: Tidally => IO () -> IO ()
only = (hush >>)

-- | See 'Sound.Tidal.Stream.streamReplace'.
p :: Tidally => ID -> ControlSignal -> IO ()
p = streamReplace tidal

-- | See 'Sound.Tidal.Stream.streamHush'.
hush :: Tidally => IO ()
hush = streamHush tidal

panic :: Tidally => IO ()
panic = hush >> once (sound "superpanic")

-- | See 'Sound.Tidal.Stream.streamList'.
list :: Tidally => IO ()
list = streamList tidal

-- | See 'Sound.Tidal.Stream.streamMute'.
mute :: Tidally => ID -> IO ()
mute = streamMute tidal

-- | See 'Sound.Tidal.Stream.streamUnmute'.
unmute :: Tidally => ID -> IO ()
unmute = streamUnmute tidal

-- | See 'Sound.Tidal.Stream.streamUnmuteAll'.
unmuteAll :: Tidally => IO ()
unmuteAll = streamUnmuteAll tidal

-- | See 'Sound.Tidal.Stream.streamUnsoloAll'.
unsoloAll :: Tidally => IO ()
unsoloAll = streamUnsoloAll tidal

-- | See 'Sound.Tidal.Stream.streamSolo'.
solo :: Tidally => ID -> IO ()
solo = streamSolo tidal

-- | See 'Sound.Tidal.Stream.streamUnsolo'.
unsolo :: Tidally => ID -> IO ()
unsolo = streamUnsolo tidal

-- | See 'Sound.Tidal.Stream.streamOnce'.
once :: Tidally => ControlSignal -> IO ()
once = streamOnce tidal

-- | An alias for 'once'.
asap :: Tidally => ControlSignal -> IO ()
asap = once

-- | See 'Sound.Tidal.Stream.first'.
first :: Tidally => ControlSignal -> IO ()
first = streamFirst tidal

-- | See 'Sound.Tidal.Stream.nudgeAll'.
nudgeAll :: Tidally => Double -> IO ()
nudgeAll = streamNudgeAll tidal

-- | See 'Sound.Tidal.Stream.streamAll'.
all :: Tidally => (ControlSignal -> ControlSignal) -> IO ()
all = streamAll tidal

-- | See 'Sound.Tidal.Stream.resetCycles'.
resetCycles :: Tidally => IO ()
resetCycles = streamResetCycles tidal

-- | See 'Sound.Tidal.Stream.streamSetCycle'.
setCycle :: Tidally => Time -> IO ()
setCycle = streamSetCycle tidal

-- | See 'Sound.Tidal.Params.cps'.
setcps :: Tidally => Signal Double -> IO ()
setcps = once . cps

-- | See 'Sound.Tidal.Stream.streamGetcps'.
getcps :: Tidally => IO Double
getcps = streamGetcps tidal

-- | See 'Sound.Tidal.Stream.streamGetnow'.
getnow :: Tidally => IO Double
getnow = streamGetnow tidal

-- | Replace what's playing on the given orbit.
d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14, d15, d16 :: Tidally => ControlSignal -> IO ()
d1 = p 1 . (|. orbit 0)
d2 = p 2 . (|. orbit 1)
d3 = p 3 . (|. orbit 2)
d4 = p 4 . (|. orbit 3)
d5 = p 5 . (|. orbit 4)
d6 = p 6 . (|. orbit 5)
d7 = p 7 . (|. orbit 6)
d8 = p 8 . (|. orbit 7)
d9 = p 9 . (|. orbit 8)
d10 = p 10 . (|. orbit 9)
d11 = p 11 . (|. orbit 10)
d12 = p 12 . (|. orbit 11)
d13 = p 13
d14 = p 14
d15 = p 15
d16 = p 16

-- | See 'Sound.Tidal.Stream.streamGet'.
getState :: Tidally => String -> IO (Maybe Value)
getState = streamGet tidal

-- | See 'Sound.Tidal.Stream.streamSetI'.
setI :: Tidally => String -> Signal Int -> IO ()
setI = streamSetI tidal

-- | See 'Sound.Tidal.Stream.streamSetF'.
setF :: Tidally => String -> Signal Double -> IO ()
setF = streamSetF tidal

-- | See 'Sound.Tidal.Stream.streamSetS'.
setS :: Tidally => String -> Signal String -> IO ()
setS = streamSetS tidal

-- | See 'Sound.Tidal.Stream.streamSetR'.
setR :: Tidally => String -> Signal Rational -> IO ()
setR = streamSetR tidal

-- | See 'Sound.Tidal.Stream.streamSetB'.
setB :: Tidally => String -> Signal Bool -> IO ()
setB = streamSetB tidal

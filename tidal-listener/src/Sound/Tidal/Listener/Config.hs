
module Sound.Tidal.Listener.Config where

import Data.List (intercalate)
import Language.Haskell.Interpreter
import Sound.Tidal.Stream (Target(..), Stream)
import qualified Sound.Tidal.Context as T

data Config = Config {listenPort :: Int
                     ,replyPort :: Int
                     ,dirtPort :: Int
                     ,noGHC :: Bool
                     } deriving (Eq,Show)

editorTarget :: Int -> Target
editorTarget rPort = Target {oName = "editor"
                            ,oAddress = "127.0.0.1"
                            ,oPort = rPort
                            ,oBusPort = Nothing
                            ,oLatency = 0.1
                            ,oWindow = Nothing
                            ,oSchedule = T.Live
                            ,oHandshake = False
                            }

startListenerStream :: Int -> Int -> IO Stream
startListenerStream rPort dPort = T.startStream T.defaultConfig
                                        [(T.superdirtTarget {oPort = dPort, oLatency = 0.1},[T.superdirtShape])
                                        ,(editorTarget rPort,[T.OSCContext "/code/highlight"])
                                        ]

libsU :: [String]
libsU = [
    "Sound.Tidal.Transition"
  , "Sound.Tidal.Context"
  , "Sound.Tidal.ID"
  , "Sound.Tidal.Simple"
  , "Control.Applicative"
  , "Data.Bifunctor"
  , "Data.Bits"
  , "Data.Bool"
  , "Data.Char"
  , "Data.Either"
  , "Data.Foldable"
  , "Data.Function"
  , "Data.Functor"
  , "Data.Int"
  , "Data.List"
  , "Data.Maybe"
  , "Data.Monoid"
  , "Data.Ord"
  , "Data.Ratio"
  , "Data.Semigroup"
  , "Data.String"
  , "Data.Traversable"
  , "Data.Tuple"
  , "Data.Typeable"
  , "Data.IORef"
  , "GHC.Float"
  , "GHC.Real"
  , "System.IO"
  , "System.Directory"
  ]

libsU' :: [ModuleImport]
libsU' = [ModuleImport x NotQualified NoImportList | x <- libsU]

libs :: [ModuleImport]
libs = [ModuleImport "Data.Map" (QualifiedAs $ Just "Map") NoImportList
       ,ModuleImport "System.IO.Silently" NotQualified (HidingList ["silence"])
       ] ++ libsU'

exts :: [Extension]
exts = [OverloadedStrings, BangPatterns, MonadComprehensions]


bootTidal' :: [String]
bootTidal' = [ "p = streamReplace tidal"
              ,"d1 !pat = p 1 $ pat |< orbit 0"
              ,"d2 !pat = p 2 $ pat |< orbit 1"
              ,"d3 !pat = p 3 $ pat |< orbit 2"
              ,"d4 !pat = p 4 $ pat |< orbit 3"
              ,"d5 !pat = p 5 $ pat |< orbit 4"
              ,"d6 !pat = p 6 $ pat |< orbit 5"
              ,"d7 !pat = p 7 $ pat |< orbit 6"
              ,"d8 !pat = p 8 $ pat |< orbit 7"
              ,"d9 !pat = p 9 $ pat |< orbit 8"
              ,"d10 !pat = p 10 $ pat |< orbit 9"
              ,"d11 !pat = p 11 $ pat |< orbit 10"
              ,"d12 !pat = p 12 $ pat |< orbit 11"
              ,"d13 !pat = p 13 $ pat |< orbit 12"
              ,"d14 !pat = p 14 $ pat |< orbit 13"
              ,"d15 !pat = p 15 $ pat |< orbit 14"
              ,"d16 !pat = p 16 $ pat |< orbit 15"
              ,"hush = streamHush tidal"
              ,"panic = do hush; once $ sound \"superpanic\""
              ,"list = streamList tidal"
              -- ,"mute = streamMute tidal"
              --,"unmute = streamUnmute tidal :: Show a => a -> IO ()"
              ,"unmuteAll = streamUnmuteAll tidal"
              ,"unsoloAll = streamUnsoloAll tidal"
              --,"solo = streamSolo tidal :: Show a => a -> IO ()"
              --,"unsolo = streamUnsolo tidal :: Show a => a -> IO ()"
              ,"once = streamOnce tidal"
              ,"first = streamFirst tidal"
              ,"asap = once"
              ,"nudgeAll = streamNudgeAll tidal"
              ,"all = streamAll tidal"
              ,"resetCycles = streamResetCycles tidal"
              ,"setcps = asap . cps"
              ,"getcps = streamGetcps tidal"
              ,"getnow = streamGetnow tidal"
              ,"xfade i = transition tidal True (Sound.Tidal.Transition.xfadeIn 4) i"
              ,"xfadeIn i t = transition tidal True (Sound.Tidal.Transition.xfadeIn t) i"
              ,"histpan i t = transition tidal True (Sound.Tidal.Transition.histpan t) i"
              ,"wait i t = transition tidal True (Sound.Tidal.Transition.wait t) i"
              ,"waitT i f t = transition tidal True (Sound.Tidal.Transition.waitT f t) i"
              ,"jump i = transition tidal True (Sound.Tidal.Transition.jump) i"
              ,"jumpIn i t = transition tidal True (Sound.Tidal.Transition.jumpIn t) i"
              ,"jumpIn' i t = transition tidal True (Sound.Tidal.Transition.jumpIn' t) i"
              ,"jumpMod i t = transition tidal True (Sound.Tidal.Transition.jumpMod t) i"
              ,"mortal i lifespan release = transition tidal True (Sound.Tidal.Transition.mortal lifespan release) i"
              ,"interpolate i = transition tidal True (Sound.Tidal.Transition.interpolate) i"
              ,"interpolateIn i t = transition tidal True (Sound.Tidal.Transition.interpolateIn t) i"
              ,"clutch i = transition tidal True (Sound.Tidal.Transition.clutch) i"
              ,"clutchIn i t = transition tidal True (Sound.Tidal.Transition.clutchIn t) i"
              ,"anticipate i = transition tidal True (Sound.Tidal.Transition.anticipate) i"
              ,"anticipateIn i t = transition tidal True (Sound.Tidal.Transition.anticipateIn t) i"
              ,"forId i t = transition tidal False (Sound.Tidal.Transition.mortalOverlay t) i"
             ]

bootTidal :: String
bootTidal = "let \n" ++ (intercalate "\n" bootTidal')

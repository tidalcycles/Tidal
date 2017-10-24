{-# LANGUAGE NoMonomorphismRestriction #-}
module Video.Tidal.VideoDirt where

import Sound.OSC.FD (Datum)
import qualified Data.Map as Map
import Control.Applicative
import Control.Concurrent.MVar
--import Visual
import Data.Colour.SRGB
import Data.Colour.Names
import Data.Hashable
import Data.Bits
import Data.Maybe
import Data.Fixed
import Data.Ratio
import Data.List (elemIndex)

import Sound.Tidal.Stream
import Sound.Tidal.OscStream
import Sound.Tidal.Pattern
import Sound.Tidal.Parse
import Sound.Tidal.Params
import Sound.Tidal.Time
import Sound.Tidal.Tempo
import Sound.Tidal.Transition (transition, wash)
import Sound.Tidal.Utils (enumerate)

videoDirt :: Shape
videodirt = Shape {   params = [ s_p,
                            n_p,
                            begin_p,
                            end_p,
                            speed_p,
                            xpos_p,
                            ypos_p,
                            zpos_p,
                            mirror_p,
                            opacity_p,
                            blendmode_p
                          ],
                 cpsStamp = True,
                 latency = 0.3
                }

videoDirtSlang = OscSlang {
  path = "/playVideo",
  timestamp = MessageStamp,
  namedParams = False,
  preamble = []
  }

videoDirtBackend port = do
  s <- makeConnection "127.0.0.1" port videoDirtSlang
  return $ Backend s (\_ _ _ -> return ())

videoDirtState port = do
  backend <- videoDirtBackend port
  Sound.Tidal.Stream.state backend videodirt


videoDirtSetters :: IO Time -> IO (ParamPattern -> IO (), (Time -> [ParamPattern] -> ParamPattern) -> ParamPattern -> IO ())
videoDirtSetters getNow = do ds <- videoDirtState 57120
                             return (setter ds, transition getNow ds)


videoDirts :: [Int]  -> IO [(ParamPattern -> IO (), (Time -> [ParamPattern] -> ParamPattern) -> ParamPattern -> IO ())]
videoDirts ports = do (_, getNow) <- cpsUtils
                      states <- mapM (videoDirtState) ports
                      return $ map (\state -> (setter state, transition getNow state)) states

-- -- disused parameter..
dirtstream _ = dirtStream

videoDirtToColour :: ParamPattern -> Pattern ColourD
videoDirtToColour p = s
  where s = fmap (\x -> maybe black (datumToColour) (Map.lookup (param videodirt "s") x)) p

showToColour :: Show a => a -> ColourD
showToColour = stringToColour . show

datumToColour :: Value -> ColourD
datumToColour = showToColour

stringToColour :: String -> ColourD
stringToColour s = sRGB (r/256) (g/256) (b/256)
  where i = (hash s) `mod` 16777216
        r = fromIntegral $ (i .&. 0xFF0000) `shiftR` 16;
        g = fromIntegral $ (i .&. 0x00FF00) `shiftR` 8;
        b = fromIntegral $ (i .&. 0x0000FF);

pick :: String -> Int -> String
pick name n = name ++ ":" ++ (show n)

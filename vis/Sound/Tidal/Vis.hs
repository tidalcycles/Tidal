module Sound.Tidal.Vis where

import qualified Graphics.Rendering.Cairo as C 
import Data.Colour
import Data.Colour.Names
import Data.Colour.SRGB
import Control.Applicative
import Sound.Tidal.Parse
import Sound.Tidal.Pattern
import Sound.Tidal.Utils
import Data.Ratio

vPDF = v C.withPDFSurface
vSVG = v C.withSVGSurface

v sf fn (x,y) pat = 
  sf fn x y $ \surf -> do
    C.renderWith surf $ do  
      C.save 
      C.scale x y
      C.setOperator C.OperatorOver
      C.setSourceRGB 0 0 0 
      C.rectangle 0 0 1 1
      C.fill
      mapM_ renderEvent (events pat)
      C.restore 


vLines sf fn (x,y) pat cyclesPerLine nLines = 
  sf fn x y $ \surf -> do
    C.renderWith surf $ do 
      C.save 
      C.scale x (y / (fromIntegral nLines))
      C.setOperator C.OperatorOver
      C.setSourceRGB 0 0 0 
      C.rectangle 0 0 1 1
      C.fill
      mapM_ (\x -> do C.save
                      C.translate 0 (fromIntegral x)
                      drawLine ((cyclesPerLine * (fromIntegral x)) ~> pat)
                      C.restore
            ) [0 .. (nLines - 1)]
      C.restore 
  where drawLine p = mapM_ renderEvent (events (_density cyclesPerLine p))


renderEvent (_, (s,e), (cs)) = do C.save
                                  drawBlocks cs 0
                                  C.restore
   where height = 1/(fromIntegral $ length cs)
         drawBlocks [] _ = return ()
         drawBlocks (c:cs) n = do let (RGB r g b) = toSRGB c
                                  C.setSourceRGBA r g b 1
                                  C.rectangle x y w h
                                  C.fill
                                  C.stroke
                                  drawBlocks cs (n+1)
           where x = (fromRational s)
                 y = (fromIntegral n) * height
                 w = (fromRational (e-s))
                 h = height


events pat = (map (mapSnd' (\(s,e) -> ((s - (ticks/2))/speed,(e - (ticks/2))/speed))) $ arc (segment pat) ((ticks/2), (ticks/2)+speed))
  where speed = 1
ticks = 0
--pat = p "[red blue green,orange purple]" :: Sequence ColourD

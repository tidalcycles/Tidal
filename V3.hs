module V3 where

import qualified Graphics.Rendering.Cairo as C 
import Data.Colour
import Data.Colour.Names
import Data.Colour.SRGB
import Control.Applicative
import Parse
import Pattern
import Utils

run f pat = 
  C.withSVGSurface
          f 1000 150 $ \surf -> do
            C.renderWith surf $ do  
              C.save 
              C.scale 1000 150
              C.setOperator C.OperatorOver
              C.setSourceRGB 0 0 0 
              C.rectangle 0 0 1 1
              C.fill
              mapM_ renderEvent (events pat)
              C.restore 

renderEvent ((s,e), (cs)) = do C.save
                               drawBlocks cs 0
                               C.restore
   where height = 1/(fromIntegral $ length cs)
         drawBlocks [] _ = return ()
         drawBlocks (c:cs) n = do let (RGB r g b) = toSRGB c
                                  C.setSourceRGBA r g b 1
                                  -- width hack (for rounding errors?)
                                  C.rectangle x y (w +0.1) h
                                  C.fill
                                  C.stroke
                                  drawBlocks cs (n+1)
           where x = (fromRational s)
                 y = (fromIntegral n) * height
                 w = (fromRational (e-s))
                 h = height


events pat = (map (mapFst (\(s,e) -> ((s - (ticks/2))/speed,(e - (ticks/2))/speed))) $ arc (segment pat) ((ticks/2), (ticks/2)+speed))
  where speed = 1
ticks = 0
--pat = p "[red blue green,orange purple]" :: Sequence ColourD

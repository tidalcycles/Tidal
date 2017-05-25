module Sound.Tidal.Vis2 where

import qualified Graphics.Rendering.Cairo as C 
import Data.Colour
import Data.Colour.Names
import Data.Colour.SRGB
import Control.Applicative
import Sound.Tidal.Parse
import Sound.Tidal.Pattern
import Sound.Tidal.Time
import Sound.Tidal.Utils
import Data.Ratio
import Data.Maybe
import System.Cmd
import Data.List
import Data.Ord         ( comparing )

totalWidth = 600 :: Double
ratio = 1/40
levelHeight = totalWidth * ratio

arrangeEvents [] = []
arrangeEvents (e:es) = addEvent e (arrangeEvents es)
fits e es = null $ filter (id) $ map (\e' -> isJust $ subArc (snd' e) (snd' e')) es
addEvent e [] = [[e]]
addEvent e (level:levels) | fits e level = (e:level):levels
                          | otherwise = level:(addEvent e levels)

v sf fn (x,y) levels =
      sf fn x y $ \surf -> do
        C.renderWith surf $ do
          C.save
          -- C.scale x (y / (fromIntegral $ length levels))
          C.setOperator C.OperatorOver
          -- C.setSourceRGB 0 0 0
          -- C.rectangle 0 0 1 1
          --C.fill
          mapM_ (renderLevel (length levels)) $ enumerate levels
          C.restore

renderLevel total (n, level) = do C.save
                                  mapM_ drawEvent $ level
                                  C.restore
      where drawEvent ((sWhole, eWhole), (s,e), c) = 
              do let (RGB r g b) = toSRGB c
                 -- C.setSourceRGBA 0.6 0.6 0.6 1
                 -- C.rectangle x y lineW levelHeight
                 C.withLinearPattern xWhole 0 (wholeLineW+xWhole) 0 $ \pattern ->
                   do --C.patternAddColorStopRGB pattern 0 0 0 0
                      --C.patternAddColorStopRGB pattern 0.5 1 1 1
                      C.save
                      C.patternAddColorStopRGBA pattern 0 r g b 1
                      C.patternAddColorStopRGBA pattern 1 r g b 0.5
                      C.patternSetFilter pattern C.FilterFast
                      C.setSource pattern
                      -- C.setSourceRGBA r g b 1
                      --C.arc (x+half) (y+half) (w/2) 0 (2 * pi)
                      C.rectangle x y lineW levelHeight
                      C.fill
                      C.restore
                      -- C.stroke
                      --C.fill
                      -- C.stroke
                   where x = (fromRational s) * totalWidth
                         y = (fromIntegral n) * levelHeight
                         xWhole = (fromRational sWhole) * totalWidth
                         w = levelHeight
                         lineW = ((fromRational $ e-s) * totalWidth)
                         wholeLineW = ((fromRational $ eWhole-sWhole) * totalWidth)
                         lineH = 2
                         lgap = 3
                         rgap = 3
                         border = 3
                         half = levelHeight / 2
                         quarter = levelHeight / 4
            vPDF = v C.withPDFSurface

vis name pat = do v (C.withSVGSurface) (name ++ ".svg") (totalWidth, levelHeight*(fromIntegral $ length levels)) levels
                  rawSystem "/home/alex/Dropbox/bin/fixsvg.pl" [name ++ ".svg"]
                  -- rawSystem "convert" [name ++ ".svg", name ++ ".pdf"]
                  return ()
                    where levels = arrangeEvents $ sortOn ((\x -> snd x - fst x) . snd') (arc pat (0,1))
                          sortOn f = map snd . sortBy (comparing fst) . map (\x -> let y = f x in y `seq` (y, x))

visAsString pat = do vis "/tmp/vis2-tmp" pat
                     svg <- readFile "/tmp/vis2-tmp.svg"
                     return svg


magicallyMakeEverythingFaster = splitArcs 16
  where splitArcs n p = concatMap (\i -> arc p (i,i+(1/n))) [0, (1/n) .. (1-(1/n))]

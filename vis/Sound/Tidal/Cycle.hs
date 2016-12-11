module Sound.Tidal.Cycle where

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Concurrent.MVar
import Data.Array.IArray

import Graphics.UI.SDL
import Graphics.UI.SDL.Image
import qualified Graphics.UI.SDL.Framerate as FR
import qualified Graphics.UI.SDL.Primitives as SDLP
import qualified Graphics.UI.SDL.TTF.General as TTFG
import Graphics.UI.SDL.TTF.Management
import Graphics.UI.SDL.TTF.Render
import Graphics.UI.SDL.TTF.Types
import Data.Maybe (listToMaybe, fromMaybe, fromJust, isJust, catMaybes)
import GHC.Int (Int16)
import Data.List (intercalate, tails, nub, sortBy)
import Data.Colour
import Data.Colour.Names
import Data.Colour.SRGB
import Data.Colour.RGBSpace.HSV (hsv)
import qualified GHC.Word 
import Data.Bits
import Data.Ratio
import Debug.Trace (trace)
import Data.Fixed (mod')
import Control.Concurrent
import System.Exit

import Sound.OSC.FD

import Sound.Tidal.Stream (ParamPattern)
import Sound.Tidal.Pattern
import Sound.Tidal.Parse
import Sound.Tidal.Tempo
import qualified Sound.Tidal.Time as Time
import Sound.Tidal.Utils


--enumerate :: [a] -> [(Int, a)]
--enumerate = zip [0..]

maybeHead [] = Nothing
maybeHead (x:_) = Just x

sortByFst :: Ord a => [(a, b)] -> [(a, b)]
sortByFst = sortBy (\a b -> compare (fst a) (fst b))

parenthesise :: String -> String
parenthesise x = "(" ++ x ++ ")"
                                          
spaces :: Int -> String
spaces n = take n $ repeat ' '

single :: [a] -> Maybe a
single (x:[]) = Just x
single _ = Nothing

fromJust' (Just x) = x
fromJust' Nothing = error "nothing is just"

data Scene = Scene {mouseXY :: (Float, Float),
                    cursor :: (Float, Float)
                   }

data AppConfig = AppConfig {
  screen       :: Surface,
  font         :: Font,
  tempoMV      :: MVar (Tempo),
  fr           :: FR.FPSManager,
  mpat         :: MVar (Pattern ColourD)
}

type AppState = StateT Scene IO
type AppEnv = ReaderT AppConfig AppState



screenWidth  = 1024
screenHeight = 768
screenBpp    = 32
middle = (fromIntegral $ screenWidth`div`2,fromIntegral $ screenHeight`div`2)

toScreen :: (Float, Float) -> (Int, Int)
toScreen (x, y) = (floor (x * (fromIntegral screenWidth)),
                   floor (y * (fromIntegral screenHeight))
                  )

toScreen16 :: (Float, Float) -> (Int16, Int16)
toScreen16 (x, y) = (fromIntegral $ floor (x * (fromIntegral screenWidth)),
                     fromIntegral $ floor (y * (fromIntegral screenHeight))
                    )

fromScreen :: (Int, Int) -> (Float, Float)
fromScreen (x, y) = ((fromIntegral x) / (fromIntegral screenWidth),
                     (fromIntegral y) / (fromIntegral screenHeight)
                    )

isInside :: Integral a => Rect -> a -> a -> Bool
isInside (Rect rx ry rw rh) x y = (x' > rx) && (x' < rx + rw) && (y' > ry) && (y' < ry + rh)
 where (x', y') = (fromIntegral x, fromIntegral y)

ctrlDown mods = or $ map (\x -> elem x [KeyModLeftCtrl, 
                                        KeyModRightCtrl
                                       ]
                         ) mods

shiftDown mods = or $ map (\x -> elem x [KeyModLeftShift, 
                                         KeyModRightShift,
                                         KeyModShift
                                        ]
                          ) mods

handleEvent :: Scene -> Event -> AppEnv (Scene)
handleEvent scene (KeyDown k) =
     handleKey scene (symKey k) (symUnicode k) (symModifiers k)

handleEvent scene _ = return scene

handleKey :: Scene -> SDLKey -> Char -> [Modifier] -> AppEnv Scene
handleKey scene SDLK_SPACE _ _ = return scene
handleKey scene _ _ _ = return scene


applySurface :: Int -> Int -> Surface -> Surface -> Maybe Rect -> IO Bool
applySurface x y src dst clip = blitSurface src clip dst offset
 where offset = Just Rect { rectX = x, rectY = y, rectW = 0, rectH = 0 }

initEnv :: MVar (Pattern ColourD) -> IO AppConfig
initEnv mp = do    
    screen <- setVideoMode screenWidth screenHeight screenBpp [SWSurface]
    font <- openFont "futura.ttf" 22
    setCaption "Cycle" []
    tempoMV <- tempoMVar
    fps <- FR.new
    FR.init fps
    FR.set fps 15
    return $ AppConfig screen font tempoMV fps mp

blankWidth = 0.015

drawArc' :: Surface -> ColourD -> (Double, Double) -> (Double, Double) -> Double -> Double -> Double -> IO ()
drawArc' screen c (x,y) (r,r') t o step | o <= 0 = return ()
                                       | otherwise = 
               do let pix = (colourToPixel c)
                      steps = [t, (t + step) .. (t + o)]
                      coords = map (\s -> (floor $ x + (r*cos(s)),floor $ y + (r*sin(s)))) steps
                               ++ map (\s -> (floor $ x + (r'*cos(s)),floor $ y + (r'*sin(s)))) (reverse steps)
                  SDLP.filledPolygon screen coords pix
        --drawArc screen c (x,y) (r,r') t (o-step) step
                  return ()
    where a = max t (t + o - step)
          b = t + o


drawArc :: Surface -> ColourD -> (Double, Double) -> (Double, Double) -> Double -> Double -> Double -> IO ()
drawArc screen c (x,y) (r,r') t o step | o <= 0 = return ()
                                       | otherwise = 
               do let pix = (colourToPixel c)
                  SDLP.filledPolygon screen coords pix
                  drawArc screen c (x,y) (r,r') t (o-step) step
                  return ()
    where a = max t (t + o - step)
          b = t + o
          coords = map ((\(x',y') -> (floor $ x + x', floor $ y + y')))
                   [(r * cos(a), r * sin(a)),
                    (r' * cos(a), r' * sin(a)),
                    (r' * cos(b), r' * sin(b)),
                    (r * cos(b), r * sin(b))
                   ]

loop :: AppEnv ()
loop = do
    quit <- whileEvents $ act
    screen <- screen `liftM` ask
    font <- font `liftM` ask
    tempoM <- tempoMV `liftM` ask
    fps <- fr `liftM` ask
    scene <- get
    mp <- mpat `liftM` ask
    liftIO $ do
        pat <- readMVar mp
        tempo <- readMVar tempoM
        beat <- beatNow tempo
        bgColor  <- (mapRGB . surfaceGetPixelFormat) screen 0x00 0x00 0x00  
        clipRect <- Just `liftM` getClipRect screen
        fillRect screen clipRect bgColor
        --drawArc screen middle (100,110) ((beat) * pi) (pi/2) (pi/32)
        drawPat middle (100,(fi screenHeight)/2) pat screen beat
        Graphics.UI.SDL.flip screen
        FR.delay fps
    unless quit loop
      where act e = do scene <- get 
                       scene' <- handleEvent scene e
                       put $ scene'

drawPat :: (Double, Double) -> (Double, Double) -> Pattern ColourD -> Surface -> Double -> IO ()
drawPat (x, y) (r,r') p screen beat = mapM_ drawEvents es
  where es = map (\(_, (s,e), evs) -> ((max s pos, min e (pos + 1)), evs)) $ arc (segment p) (pos, pos + 1)
        pos = toRational $ beat / 8
        drawEvents ((s,e), cs) = 
          mapM_ (\(n', c) -> drawEvent (s,e) c n' (length cs)) (enumerate $ reverse cs)
        drawEvent (s,e) c n' len =
          do let thickness = (1 / fromIntegral len) * (r' - r)
             let start = r + thickness * (fromIntegral n')
             drawArc screen c middle (start,start+thickness) ((pi*2) * (fromRational (s-pos))) ((pi*2) * fromRational (e-s)) (pi/16)
{-          (thickLine h (n*scale+n') (linesz/ (fromIntegral scale))
           (x1 + (xd * fromRational (e-pos)))
           (y1 + (yd * fromRational (e-pos)))
           (x1 + (xd * fromRational (s-pos))) 
           (y1 + (yd * fromRational (s-pos)))
          ) 
          screen (colourToPixel c)-}

segment2 :: Pattern a -> Pattern [(Bool, a)]
segment2 p = Pattern $ \(s,e) -> filter (\(_, (s',e'),_) -> s' < e && e' > s) $ groupByTime (segment2' (arc (fmap (\x -> (True, x)) p) (s,e)))


segment2' :: [Time.Event (Bool, a)] -> [Time.Event (Bool, a)]
segment2' es = foldr splitEs es pts
  where pts = nub $ points es

splitEs :: Time.Time -> [Time.Event (Bool, a)] -> [Time.Event (Bool, a)]
splitEs _ [] = []
splitEs t ((ev@(a, (s,e), (h,v))):es) | t > s && t < e = (a, (s,t),(h,v)):(a, (t,e),(False,v)):(splitEs t es)
                                  | otherwise = ev:splitEs t es

whileEvents :: MonadIO m => (Event -> m ()) -> m Bool
whileEvents act = do
    event <- liftIO pollEvent
    case event of
        Quit -> return True
        NoEvent -> return False
        _       ->  do
            act event
            whileEvents act

runLoop :: AppConfig -> Scene -> IO ()
runLoop = evalStateT . runReaderT loop

textSize :: String -> Font -> IO ((Float,Float))
textSize text font = 
  do message <- renderTextSolid font text (Color 0 0 0)
     return (fromScreen (surfaceGetWidth message, surfaceGetHeight message))

run = do mp <- newMVar silence
         forkIO $ run' mp
         return mp
         

run' mp = withInit [InitEverything] $ 
        do result <- TTFG.init
           if not result
             then putStrLn "Failed to init ttf"
             else do enableUnicode True
                     env <- initEnv mp
                     --ws <- wordMenu (font env) things
                     let scene = Scene (0,0) (0.5,0.5)
                     --putStrLn $ show scene
                     runLoop env scene


-- colourToPixel :: Colour Double -> Pixel
-- colourToPixel c =  rgbColor (floor $ 256*r) (floor $ 256* g) (floor $ 256*b)
--  where (RGB r g b) = toSRGB c

colourToPixel :: Colour Double -> Pixel
colourToPixel c = rgbColor (floor $ r*255) (floor $ g*255) (floor $ b *255) -- mapRGB (surfaceGetPixelFormat screen) 255 255 255
   where (RGB r g b) = toSRGB c

--colourToPixel :: Surface -> Colour Double -> IO Pixel
--colourToPixel s c = (mapRGB . surfaceGetPixelFormat) s (floor $ r*255) (floor $ g*255) (floor $ b*255)


fi a = fromIntegral a

rgbColor :: GHC.Word.Word8 -> GHC.Word.Word8 -> GHC.Word.Word8 -> Pixel
rgbColor r g b = Pixel (shiftL (fi r) 24 .|. shiftL (fi g) 16 .|. shiftL (fi b) 8 .|. (fi 255))

pixel :: Surface -> (GHC.Word.Word8,GHC.Word.Word8,GHC.Word.Word8) -> IO Pixel
pixel surface (r,g,b) = mapRGB (surfaceGetPixelFormat surface) r g b

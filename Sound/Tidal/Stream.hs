{-# LANGUAGE OverloadedStrings, FlexibleInstances, RankNTypes, NoMonomorphismRestriction #-}

module Sound.Tidal.Stream where

import Data.Maybe
import Sound.OSC.FD
import Sound.OSC.Datum
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception as E
import Data.Time (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Ratio
import GHC.Float (float2Double, double2Float)
import Sound.Tidal.Pattern
import qualified Sound.Tidal.Parse as P
import Sound.Tidal.Tempo (Tempo, logicalTime, clocked,clockedTick,cps)
import Sound.Tidal.Utils
import qualified Sound.Tidal.Time as T

import qualified Data.Map as Map

data Param = S {name :: String, sDefault :: Maybe String}
           | F {name :: String, fDefault :: Maybe Double}
           | I {name :: String, iDefault :: Maybe Int}

instance Eq Param where
  a == b = name a == name b

instance Ord Param where
  compare a b = compare (name a) (name b)
instance Show Param where
  show p = name p

data TimeStamp = BundleStamp | MessageStamp | NoStamp
 deriving Eq

data OscShape = OscShape {path :: String,
                          params :: [Param],
                          cpsStamp :: Bool,
                          timestamp :: TimeStamp,
                          latency :: Double,
                          namedParams :: Bool,
                          preamble :: [Datum]
                         }
type OscMap = Map.Map Param (Maybe Datum)

type OscPattern = Pattern OscMap

ticksPerCycle = 8

defaultDatum :: Param -> Maybe Datum
defaultDatum (S _ (Just x)) = Just $ string x
defaultDatum (I _ (Just x)) = Just $ int32 x
defaultDatum (F _ (Just x)) = Just $ float x
defaultDatum _ = Nothing

hasDefault :: Param -> Bool
hasDefault (S _ Nothing) = False
hasDefault (I _ Nothing) = False
hasDefault (F _ Nothing) = False
hasDefault _ = True

defaulted :: OscShape -> [Param]
defaulted = filter hasDefault . params

defaultMap :: OscShape -> OscMap
defaultMap s
  = Map.fromList $ map (\x -> (x, defaultDatum x)) (defaulted s)

required :: OscShape -> [Param]
required = filter (not . hasDefault) . params

hasRequired :: OscShape -> OscMap -> Bool
hasRequired s m = isSubset (required s) (Map.keys (Map.filter (\x -> x /= Nothing) m))

isSubset :: (Eq a) => [a] -> [a] -> Bool
isSubset xs ys = all (\x -> elem x ys) xs

toMessage :: UDP -> OscShape -> Tempo -> Int -> (Double, OscMap) -> Maybe (IO ())
toMessage s shape change tick (o, m) =
  do m' <- applyShape' shape m
     let cycleD = ((fromIntegral tick) / (fromIntegral ticksPerCycle)) :: Double
         logicalNow = (logicalTime change cycleD)
         logicalPeriod = (logicalTime change (cycleD + (1/(fromIntegral ticksPerCycle)))) - logicalNow
         logicalOnset = logicalNow + (logicalPeriod * o) + (latency shape) + nudge
         sec = floor logicalOnset
         usec = floor $ 1000000 * (logicalOnset - (fromIntegral sec))
         oscdata = cpsPrefix ++ preamble shape ++ (parameterise $ catMaybes $ mapMaybe (\x -> Map.lookup x m') (params shape))
         oscdata' = ((int32 sec):(int32 usec):oscdata)
         osc | timestamp shape == BundleStamp = sendOSC s $ Bundle (ut_to_ntpr logicalOnset) [Message (path shape) oscdata]
             | timestamp shape == MessageStamp = sendOSC s $ Message (path shape) oscdata'
             | otherwise = doAt logicalOnset $ sendOSC s $ Message (path shape) oscdata
     return osc
     where
       parameterise :: [Datum] -> [Datum]
       parameterise ds | namedParams shape =
                               mergelists (map (string . name) (params shape)) ds
                       | otherwise = ds
       cpsPrefix | cpsStamp shape = [float (cps change)]
                 | otherwise = []
       nudge = maybe 0 (toF) (Map.lookup (F "nudge" (Just 0)) m)
       toF (Just (Float f)) = float2Double f
       toF _ = 0

doAt t action = do forkIO $ do now <- getCurrentTime
                               let nowf = realToFrac $ utcTimeToPOSIXSeconds now
                               threadDelay $ floor $ (t - nowf) * 1000000
                               action
                   return ()
                       
applyShape' :: OscShape -> OscMap -> Maybe OscMap
applyShape' s m | hasRequired s m = Just $ Map.union m (defaultMap s)
                | otherwise = Nothing

start :: String -> Int -> OscShape -> IO (MVar (OscPattern))
start address port shape
  = do patternM <- newMVar silence
       s <- openUDP address port
       let ot = (onTick s shape patternM) :: Tempo -> Int -> IO ()
       forkIO $ clockedTick ticksPerCycle ot
       return patternM

-- variant of start where history of patterns is available
state :: String -> Int -> OscShape -> IO (MVar (OscPattern, [OscPattern]))
state address port shape
  = do patternsM <- newMVar (silence, [])
       s <- openUDP address port
       let ot = (onTick' s shape patternsM) :: Tempo -> Int -> IO ()
       forkIO $ clockedTick ticksPerCycle ot
       return patternsM

stream :: String -> Int -> OscShape -> IO (OscPattern -> IO ())
stream address port shape 
  = do patternM <- start address port shape
       return $ \p -> do swapMVar patternM p
                         return ()

streamcallback :: (OscPattern -> IO ()) -> String -> Int -> OscShape -> IO (OscPattern -> IO ())
streamcallback callback server port shape 
  = do f <- stream server port shape
       let f' p = do callback p
                     f p
       return f'

onTick :: UDP -> OscShape -> MVar (OscPattern) -> Tempo -> Int -> IO ()
onTick s shape patternM change ticks
  = do p <- readMVar patternM
       let ticks' = (fromIntegral ticks) :: Integer
           a = ticks' % ticksPerCycle
           b = (ticks' + 1) % ticksPerCycle
           messages = mapMaybe 
                      (toMessage s shape change ticks) 
                      (seqToRelOnsets (a, b) p)
       E.catch (sequence_ messages) (\msg -> putStrLn $ "oops " ++ show (msg :: E.SomeException))
       return ()

-- Variant where mutable variable contains list as history of the patterns
onTick' :: UDP -> OscShape -> MVar (OscPattern, [OscPattern]) -> Tempo -> Int -> IO ()
onTick' s shape patternsM change ticks
  = do ps <- readMVar patternsM
       let ticks' = (fromIntegral ticks) :: Integer
           a = ticks' % ticksPerCycle
           b = (ticks' + 1) % ticksPerCycle
           messages = mapMaybe 
                      (toMessage s shape change ticks) 
                      (seqToRelOnsets (a, b) $ fst ps)
       E.catch (sequence_ messages) (\msg -> putStrLn $ "oops " ++ show (msg :: E.SomeException))
       return ()

make :: (a -> Datum) -> OscShape -> String -> Pattern a -> OscPattern
make toOsc s nm p = fmap (\x -> Map.singleton nParam (defaultV x)) p
  where nParam = param s nm
        defaultV a = Just $ toOsc a
        --defaultV Nothing = defaultDatum nParam

nudge :: Pattern Double -> OscPattern
nudge p = fmap (\x -> Map.singleton (F "nudge" (Just 0)) (Just $ float x)) p
 
makeS = make string

makeF :: OscShape -> String -> Pattern Double -> OscPattern
makeF = make float

makeI :: OscShape -> String -> Pattern Int -> OscPattern
makeI = make int32

param :: OscShape -> String -> Param
param shape n = head $ filter (\x -> name x == n) (params shape)

merge :: OscPattern -> OscPattern -> OscPattern
merge x y = (flip Map.union) <$> x <*> y

infixl 1 |=|
(|=|) :: OscPattern -> OscPattern -> OscPattern
(|=|) = merge

(#) = (|=|)

mergeWith op x y = (Map.unionWithKey f) <$> x <*> y
  where f (F _ _) (Just a) (Just b) = do a' <- datum_float a
                                         b' <- datum_float b
                                         return $ float (op a' b')
        f _ a b = b

infixl 1 |*|
(|*|) :: OscPattern -> OscPattern -> OscPattern
(|*|) = mergeWith (*)

infixl 1 |+|
(|+|) :: OscPattern -> OscPattern -> OscPattern
(|+|) = mergeWith (+)

infixl 1 |-|
(|-|) :: OscPattern -> OscPattern -> OscPattern
(|-|) = mergeWith (-)

infixl 1 |/|
(|/|) :: OscPattern -> OscPattern -> OscPattern
(|/|) = mergeWith (/)

setter :: MVar (a, [a]) -> a -> IO ()
setter ds p = do ps <- takeMVar ds
                 putMVar ds $ (p, p:snd ps)
                 return ()

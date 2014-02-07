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

import Sound.Tidal.Pattern
import qualified Sound.Tidal.Parse as P
import Sound.Tidal.Tempo (Tempo, logicalTime, clocked,clockedTick)

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

data OscShape = OscShape {path :: String,
                          params :: [Param],
                          timestamp :: Bool
                         }
type OscMap = Map.Map Param (Maybe Datum)

type OscPattern = Pattern OscMap

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

tpb = 1
bpb = 2

toMessage :: UDP -> OscShape -> Tempo -> Int -> (Double, OscMap) -> Maybe (IO ())
toMessage s shape change ticks (o, m) =
  do m' <- applyShape' shape m
     let beat = fromIntegral ticks / fromIntegral tpb
         latency = 0.02
         logicalNow = (logicalTime change beat)
         beat' = (fromIntegral ticks + 1) / fromIntegral tpb
         logicalPeriod = (logicalTime change (beat + 1)) - logicalNow
         --logicalOnset = ntpr_to_ut $ logicalNow + (logicalPeriod * o) + latency
         logicalOnset = logicalNow + (logicalPeriod * o) + latency
         sec = floor logicalOnset
         usec = floor $ 1000000 * (logicalOnset - (fromIntegral sec))
         oscdata = catMaybes $ mapMaybe (\x -> Map.lookup x m') (params shape)
         oscdata' = ((int32 sec):(int32 usec):oscdata)
         osc | timestamp shape = sendOSC s $ Message (path shape) oscdata'
             | otherwise = doAt logicalOnset $ sendOSC s $ Message (path shape) oscdata
     return osc

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
       --putStrLn $ "connecting " ++ (show address) ++ ":" ++ (show port)
       s <- openUDP address port
       --putStrLn $ "connected "
       let ot = (onTick s shape patternM) :: Tempo -> Int -> IO ()
       forkIO $ clocked $ ot
       return patternM

stream :: String -> Int -> OscShape -> IO (OscPattern -> IO ())
stream address port shape 
  = do patternM <- start address port shape
       return $ \p -> do swapMVar patternM (slow bpb p)
                         return ()

streamcallback :: (OscPattern -> IO ()) -> String -> Int -> OscShape -> IO (OscPattern -> IO ())
streamcallback callback server port shape 
  = do f <- stream server port shape
       let f' p = do callback p
                     f p
       return f'

onTick :: UDP -> OscShape -> MVar (OscPattern) -> Tempo -> Int -> IO ()
onTick s shape patternM change beats
  = do p <- readMVar patternM
       let --tpb' = 2 :: Integer
           tpb' = 1
           beats' = (fromIntegral beats) :: Integer
           a = beats' % tpb'
           b = (beats' + 1) % tpb'
           messages = mapMaybe 
                      (toMessage s shape change beats) 
                      (seqToRelOnsets (a, b) p)
       --putStrLn $ (show a) ++ ", " ++ (show b)
       --putStrLn $ "tick " ++ show ticks ++ " = " ++ show messages
       E.catch (sequence_ messages) (\msg -> putStrLn $ "oops " ++ show (msg :: E.SomeException))
       return ()

{-ticker :: IO (MVar Rational)
ticker = do mv <- newMVar 0
            --forkIO $ clocked "ticker" "127.0.0.1" "127.0.0.1" tpb (f mv)
            forkIO $ clocked (f mv)
            return mv
  where f mv change ticks = do swapMVar mv ((fromIntegral ticks) / (fromIntegral tpb))
  where f mv ticks = do swapMVar mv (fromIntegral ticks)
                        return ()
        tpb = 32
-}

make :: (a -> Datum) -> OscShape -> String -> Pattern a -> OscPattern
make toOsc s nm p = fmap (\x -> Map.singleton nParam (defaultV x)) p
  where nParam = param s nm
        defaultV a = Just $ toOsc a
        --defaultV Nothing = defaultDatum nParam

makeS = make string
makeF = make float
makeI = make int32

param :: OscShape -> String -> Param
param shape n = head $ filter (\x -> name x == n) (params shape)
                
merge :: OscPattern -> OscPattern -> OscPattern
merge x y = Map.union <$> x <*> y

infixl 1 |+|
(|+|) :: OscPattern -> OscPattern -> OscPattern
(|+|) = merge



weave :: Rational -> OscPattern -> [OscPattern] -> OscPattern
weave t p ps | l == 0 = silence
             | otherwise = slow t $ stack $ map (\(i, p') -> ((density t p') |+| (((fromIntegral i) % l) <~ p))) (zip [0 ..] ps)
  where l = fromIntegral $ length ps



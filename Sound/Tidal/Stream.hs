{-# LANGUAGE OverloadedStrings, FlexibleInstances, RankNTypes, NoMonomorphismRestriction, DeriveDataTypeable #-}

module Sound.Tidal.Stream where

import Data.Maybe
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception as E
import Data.Time (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Ratio
import Data.Typeable
import Sound.Tidal.Pattern
import qualified Sound.Tidal.Parse as P
import Sound.Tidal.Tempo (Tempo, logicalTime, clockedTick,cps)
import Sound.Tidal.Utils
import qualified Sound.Tidal.Time as T

import qualified Data.Map.Strict as Map

type ToMessageFunc = Shape -> Tempo -> Int -> (Double, Double, ParamMap) -> Maybe (IO ())

data Backend a = Backend {
  toMessage :: ToMessageFunc,
  flush :: Shape -> Tempo -> Int -> IO ()
  }

data Param = S {name :: String, sDefault :: Maybe String}
           | F {name :: String, fDefault :: Maybe Double}
           | I {name :: String, iDefault :: Maybe Int}
  deriving Typeable

instance Eq Param where
  a == b = name a == name b

instance Ord Param where
  compare a b = compare (name a) (name b)
instance Show Param where
  show p = name p


data Shape = Shape {params :: [Param],
                    latency :: Double,
                    cpsStamp :: Bool}


data Value = VS { svalue :: String } | VF { fvalue :: Double } | VI { ivalue :: Int }
           deriving (Eq,Ord,Typeable)

instance Show Value where
  show (VS s) = s
  show (VF f) = show f
  show (VI i) = show i

class ParamType a where
  fromV :: Value -> Maybe a
  toV :: a -> Value

instance ParamType String where
  fromV (VS s) = Just s
  fromV _ = Nothing
  toV s = VS s

instance ParamType Double where
  fromV (VF f) = Just f
  fromV _ = Nothing
  toV f = VF f

instance ParamType Int where
  fromV (VI i) = Just i
  fromV _ = Nothing
  toV i = VI i

type ParamMap = Map.Map Param Value

type ParamPattern = Pattern ParamMap

ticksPerCycle = 8

defaultValue :: Param -> Value
defaultValue (S _ (Just x)) = VS x
defaultValue (I _ (Just x)) = VI x
defaultValue (F _ (Just x)) = VF x

hasDefault :: Param -> Bool
hasDefault (S _ Nothing) = False
hasDefault (I _ Nothing) = False
hasDefault (F _ Nothing) = False
hasDefault _ = True

defaulted :: Shape -> [Param]
defaulted = filter hasDefault . params

defaultMap :: Shape -> ParamMap
defaultMap s
  = Map.fromList $ map (\x -> (x, defaultValue x)) (defaulted s)

required :: Shape -> [Param]
required = filter (not . hasDefault) . params

hasRequired :: Shape -> ParamMap -> Bool
hasRequired s m = isSubset (required s) (Map.keys m)

isSubset :: (Eq a) => [a] -> [a] -> Bool
isSubset xs ys = all (\x -> elem x ys) xs


doAt t action = do _ <- forkIO $ do
                               now <- getCurrentTime
                               let nowf = realToFrac $ utcTimeToPOSIXSeconds now
                               threadDelay $ floor $ (t - nowf) * 1000000
                               action
                   return ()

logicalOnset' change tick o offset = logicalNow + (logicalPeriod * o) + offset
  where
    tpc = fromIntegral ticksPerCycle
    cycleD = ((fromIntegral tick) / tpc) :: Double
    logicalNow = logicalTime change cycleD
    logicalPeriod = (logicalTime change (cycleD + (1/tpc))) - logicalNow


applyShape' :: Shape -> ParamMap -> Maybe ParamMap
applyShape' s m | hasRequired s m = Just $ Map.union m (defaultMap s)
                | otherwise = Nothing

start :: Backend a -> Shape -> IO (MVar (ParamPattern))
start backend shape
  = do patternM <- newMVar silence
       let ot = (onTick backend shape patternM) :: Tempo -> Int -> IO ()
       forkIO $ clockedTick ticksPerCycle ot
       return patternM

-- variant of start where history of patterns is available
state :: Backend a -> Shape -> IO (MVar (ParamPattern, [ParamPattern]))
state backend shape
  = do patternsM <- newMVar (silence, [])
       let ot = (onTick' backend shape patternsM) :: Tempo -> Int -> IO ()
       forkIO $ clockedTick ticksPerCycle ot
       return patternsM

stream :: Backend a -> Shape -> IO (ParamPattern -> IO ())
stream backend shape
  = do patternM <- start backend shape
       return $ \p -> do swapMVar patternM p
                         return ()

streamcallback :: (ParamPattern -> IO ()) -> Backend a -> Shape -> IO (ParamPattern -> IO ())
streamcallback callback backend shape
  = do f <- stream backend shape
       let f' p = do callback p
                     f p
       return f'

onTick :: Backend a -> Shape -> MVar (ParamPattern) -> Tempo -> Int -> IO ()
onTick backend shape patternM change ticks
  = do p <- readMVar patternM
       let ticks' = (fromIntegral ticks) :: Integer
           a = ticks' % ticksPerCycle
           b = (ticks' + 1) % ticksPerCycle
           messages = mapMaybe
                      (toMessage backend shape change ticks)
                      (seqToRelOnsetDeltas (a, b) p)
       E.catch (sequence_ messages) (\msg -> putStrLn $ "oops " ++ show (msg :: E.SomeException))
       flush backend shape change ticks
       return ()

-- Variant where mutable variable contains list as history of the patterns
onTick' :: Backend a -> Shape -> MVar (ParamPattern, [ParamPattern]) -> Tempo -> Int -> IO ()
onTick' backend shape patternsM change ticks
  = do ps <- readMVar patternsM
       let ticks' = (fromIntegral ticks) :: Integer
           toM = (toMessage backend)
           a = ticks' % ticksPerCycle
           b = (ticks' + 1) % ticksPerCycle
           messages = mapMaybe
                      (toM shape change ticks)
                      (seqToRelOnsetDeltas (a, b) $ fst ps)
       E.catch (sequence_ messages) (\msg -> putStrLn $ "oops " ++ show (msg :: E.SomeException))
       flush backend shape change ticks
       return ()

make :: (a -> Value) -> Shape -> String -> Pattern a -> ParamPattern
make toValue s nm p = fmap (\x -> Map.singleton nParam (defaultV x)) p
  where nParam = param s nm
        defaultV a = toValue a
        --defaultV Nothing = defaultValue nParam

make' :: ParamType a => (a -> Value) -> Param -> Pattern a -> ParamPattern
make' toValue par p = fmap (\x -> Map.singleton par (toValue x)) p

makeP :: ParamType a => Param -> Pattern a -> ParamPattern
makeP par p = coerce par $ fmap (\x -> Map.singleton par (toV x)) p

makeS = make VS

makeF :: Shape -> String -> Pattern Double -> ParamPattern
makeF = make VF

makeI :: Shape -> String -> Pattern Int -> ParamPattern
makeI = make VI

param :: Shape -> String -> Param
param shape n = head $ filter (\x -> name x == n) (params shape)

merge :: ParamPattern -> ParamPattern -> ParamPattern
merge x y = (flip Map.union) <$> x <*> y

infixl 1 |=|
(|=|) :: ParamPattern -> ParamPattern -> ParamPattern
(|=|) = merge

infixl 1 #
(#) = (|=|)

mergeWith op x y = (Map.unionWithKey op) <$> x <*> y

mergeWith
  :: (Ord k, Applicative f) =>
     (k -> a -> a -> a)
     -> f (Map.Map k a) -> f (Map.Map k a) -> f (Map.Map k a)

mergeNumWith intOp floatOp = mergeWith f
  where f (F _ _) (VF a) (VF b) = VF $ floatOp a b
        f (I _ _) (VI a) (VI b) = VI $ intOp a b
        f _ _ b = b

mergePlus = mergeWith f
  where f (F _ _) (VF a) (VF b) = VF $ a + b
        f (I _ _) (VI a) (VI b) = VI $ a + b
        f (S _ _) (VS a) (VS b) = VS $ a ++ b
        f _ _ b = b

infixl 1 |*|
(|*|) :: ParamPattern -> ParamPattern -> ParamPattern
(|*|) = mergeNumWith (*) (*)

infixl 1 |+|
(|+|) :: ParamPattern -> ParamPattern -> ParamPattern
(|+|) = mergePlus

infixl 1 |-|
(|-|) :: ParamPattern -> ParamPattern -> ParamPattern
(|-|) = mergeNumWith (-) (-)

infixl 1 |/|
(|/|) :: ParamPattern -> ParamPattern -> ParamPattern
(|/|) = mergeNumWith (div) (/)

{- | These are shorthand for merging lists of patterns with @#@, @|*|@, @|+|@,
or @|/|@.  Sometimes this saves a little typing and can improve readability
when passing things into other functions.  As an example, instead of writing
@
d1 $ sometimes ((|*| speed "2") . (|*| cutoff "2") . (|*| shape "1.5")) $ sound "arpy*4" # cutoff "350" # shape "0.3"
@
you can write
@
d1 $ sometimes (*** [speed "2", cutoff "2", shape "1.5"]) $ sound "arpy*4" ### [cutoff "350", shape "0.3"]
@
-}
(###) = foldl (#)
(***) = foldl (|*|)
(+++) = foldl (|+|)
(///) = foldl (|/|)

setter :: MVar (a, [a]) -> a -> IO ()
setter ds p = do ps <- takeMVar ds
                 putMVar ds $ (p, p:snd ps)
                 return ()

{- | Copies values from one parameter to another. Used by @nToOrbit@ in @Sound.Tidal.Dirt@. -}

copyParam:: Param -> Param -> ParamPattern -> ParamPattern
copyParam fromParam toParam pat = f <$> pat
  where f m = maybe m (updateValue m) (Map.lookup fromParam m)
        updateValue m v = Map.union m (Map.fromList [(toParam,v)])

get :: ParamType a => Param -> ParamPattern -> Pattern a
get param p = filterJust $ fromV <$> (filterJust $ Map.lookup param <$> p)

getI :: Param -> ParamPattern -> Pattern Int
getI = get
getF :: Param -> ParamPattern -> Pattern Double
getF = get
getS :: Param -> ParamPattern -> Pattern String
getS = get

with :: (ParamType a) => Param -> (Pattern a -> Pattern a) -> ParamPattern -> ParamPattern
with param f p = p # (makeP param) ((\x -> f (get param x)) p)
withI :: Param -> (Pattern Int -> Pattern Int) -> ParamPattern -> ParamPattern
withI = with
withF :: Param -> (Pattern Double -> Pattern Double) -> ParamPattern -> ParamPattern
withF = with
withS :: Param -> (Pattern String -> Pattern String) -> ParamPattern -> ParamPattern
withS = with

follow :: (ParamType a, ParamType b) => Param -> Param -> (Pattern a -> Pattern b) -> ParamPattern -> ParamPattern
follow source dest f p = p # (makeP dest $ f (get source p))

-- follow :: ParamType a => Param -> (Pattern a -> ParamPattern) -> ParamPattern -> ParamPattern
-- follow source dest p = p # (dest $ get source p)

follow' :: ParamType a => Param -> Param -> (Pattern a -> Pattern a) -> ParamPattern -> ParamPattern
follow' source dest f p = p # (makeP dest $ f (get source p))

followI :: Param -> Param -> (Pattern Int -> Pattern Int) -> ParamPattern -> ParamPattern
followI = follow'
followF :: Param -> Param -> (Pattern Double -> Pattern Double) -> ParamPattern -> ParamPattern
followF = follow'
followS :: Param -> Param -> (Pattern String -> Pattern String) -> ParamPattern -> ParamPattern
followS = follow'

-- with :: ParamType a => Param -> (Pattern a -> Pattern a) -> ParamPattern -> ParamPattern
-- with source f p = p # (makeP source $ f (get source p))
coerce :: Param -> ParamPattern -> ParamPattern
coerce par@(S _ _) p = (Map.update f par) <$> p
  where f (VS s) = Just (VS s)
        f (VI i) = Just (VS $ show i)
        f (VF f) = Just (VS $ show f)
coerce par@(I _ _) p = (Map.update f par) <$> p
  where f (VS s) = Just (VI $ read s)
        f (VI i) = Just (VI i)
        f (VF f) = Just (VI $ floor f)
coerce par@(F _ _) p = (Map.update f par) <$> p
  where f (VS s) = Just (VF $ read s)
        f (VI i) = Just (VF $ fromIntegral i)
        f (VF f) = Just (VF f)

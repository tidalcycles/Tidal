{-# LANGUAGE DeriveDataTypeable #-}

module Pattern where

import Control.Applicative
import Data.Monoid
--import Control.Monad
import Data.Fixed
import Data.List
import Data.Maybe
import Data.Ratio
import Debug.Trace
import Data.Typeable
import Data.Function

import Time
import Utils

type Event a = (Arc, a)

data Sequence a = Sequence {arc :: Arc -> [Event a]}

data Signal a = Signal {at :: Time -> [a]}

instance (Show a) => Show (Sequence a) where
  show p@(Sequence _) = show $ arc p (0, 1)

instance (Show a) => Show (Signal a) where
  show p@(Signal _) = "~signal~"

--class (Functor p, Applicative p) => Pattern p where
class (Functor p, Applicative p, Monad p) => Pattern p where
  pt :: (p a) -> Time -> [a]
  atom :: a -> p a
  silence :: p a
  toSignal :: p a -> Signal a
  toSignal p = Signal $ \t -> pt p t
  mapQueryTime :: (Time -> Time) -> p a -> p a
  mapResultTime :: (Time -> Time) -> p a -> p a
  squash :: Int -> (Int, p a) -> p a
  overlay :: p a -> p a -> p a
  
{-
  mapTime = mapOnset
  mapOnset :: (Time -> Time) -> p a -> p a
  mapTimeOut :: (Time -> Time) -> p a -> p a
-}  
  
instance Pattern Signal where
  pt = at
  atom x = Signal $ const [x]
  silence = Signal $ const []
  toSignal = id
  mapQueryTime f p = Signal $ \t -> at p (f t)
  mapResultTime _ p = p
  squash = squashSignal
  overlay p p' = Signal $ \t -> (at p t) ++ (at p' t)

instance Functor Sequence where
  fmap f (Sequence a) = Sequence $ fmap (fmap (mapSnd f)) a

instance Functor Signal where
  fmap f (Signal a) = Signal $ fmap (fmap f) a

instance Pattern Sequence where
  pt p t = map snd $ arc p (t, t)
  atom x = Sequence f
    where f (s, e) = map 
                     (\t -> ((t%1, (t+1)%1), x))
                     [floor s .. ((ceiling e) - 1)]
  silence = Sequence $ const []
  mapQueryTime f p = Sequence $ \a -> arc p (mapArc f a)
  mapResultTime f p = Sequence $ \a -> mapFsts (mapArc f) $ arc p a
  squash = squashSequence
  overlay p p' = Sequence $ \a -> (arc p a) ++ (arc p' a)

instance Applicative Signal where
  pure = atom
  (Signal fs) <*> (Signal xs) = Signal $ \t -> (fs t) <*> (xs t)

instance Applicative Sequence where
  pure = atom
  (Sequence fs) <*> (Sequence xs) = 
    Sequence $ \a ->
      concatMap
      (\((s,e),x) -> map 
                     (mapSnd ($ x))
                     (filter
                      (isIn (s,e) . eventStart)
                      (fs a)
                     )
      )
      (xs a)

eventStart :: Event a -> Time
eventStart = fst . fst


instance Monad Signal where
  return = pure
  p >>= f = Signal (\t -> concat $ map (\x -> at (f x) t) (at p t))

instance Monad Sequence where
  return = pure
  p >>= f = 
    Sequence (\a -> concatMap 
                    (\(a', x) -> mapFsts (cutArc a) $ arc (f x) a')
                    (arc p a)
             )

instance Monoid (Signal a) where
    mempty = silence
    mappend x y = Signal $ \t -> (at x t) ++ (at y t)

instance Monoid (Sequence a) where
    mempty = silence
    mappend x y = Sequence $ \a -> (arc x a) ++ (arc y a)

cat :: (Pattern p) => [p b] -> p b
cat ps = stack $ map (squash l) (zip [0..] ps)
  where l = length ps

listToPat :: Pattern p => [a] -> p a
listToPat = cat . map atom

slowcat :: (Pattern p) => [p b] -> p b
slowcat ps = slow (fromIntegral $ length ps) $ cat ps

squashSignal :: Int -> (Int, Signal a) -> Signal a
squashSignal parts (part, p) = Signal f
  where f t | cyclePos t >= start && cyclePos t < end = at p $ scale t
            | otherwise = []
        start = fromIntegral part % fromIntegral parts
        end = start + (1 % fromIntegral parts)
        scale t = (sam t) + ((cyclePos t - start) * fromIntegral parts)

squashSequence :: Int -> (Int, Sequence a) -> Sequence a
squashSequence parts (part, p) = Sequence f 
  where f a = concat $ catMaybes $ map doCycle (arcCycles a)
        start = fromIntegral part % fromIntegral parts
        end = start + (1 % fromIntegral parts)
        doCycle (s,e) | s > end' || e <= start' = Nothing
                      | otherwise = do (s',e') <- subArc (s,e) 
                                                  (start', end')
                                       let es = arc p (scale s', 
                                                       scale e'
                                                      )
                                       return $ mapFsts scaleOutArc es
          where cycleStart = sam s
                scale t = 
                  cycleStart + (((t - cycleStart) - start)
                                * fromIntegral parts)
                scaleOut t = 
                  start' + ((t - cycleStart) 
                                        / fromIntegral parts)
                scaleOutArc a = mapArc scaleOut a
                start' = start + cycleStart
                end' = end + cycleStart

stack :: (Pattern p) => [p a] -> p a
stack ps = foldr overlay silence ps

density :: Pattern p => Time -> p a -> p a
density 1 p = p
density r p = mapResultTime (/ r) $ mapQueryTime (* r) p

slow :: Pattern p => Time -> p a -> p a
slow t = density (1/t) 


(<~) :: Pattern p => Time -> p a -> p a
(<~) t p = mapResultTime (+ t) $ mapQueryTime (subtract t) p

(~>) :: Pattern p => Time -> p a -> p a
(~>) = (<~) . (0-)

sig :: (Time -> a) -> Sequence a
sig f = Sequence f'
  where f' (s,e) | s > e = []
                 | otherwise = [((s,e), f s)]

sinewave :: Sequence Double
sinewave = sig $ \t -> sin $ pi * 2 * (fromRational t)

sinewave1 :: Sequence Double
sinewave1 = fmap ((/ 2) . (+ 1)) sinewave

sinePhase1 :: Double -> Sequence Double
sinePhase1 offset = (+ offset) <$> sinewave1

triwave1 :: Sequence Double
triwave1 = sig $ \t -> mod' (fromRational t) 1

triwave :: Sequence Double
triwave = ((subtract 1) . (* 2)) <$> triwave1


squarewave1 :: Sequence Double
squarewave1 = sig $ 
              \t -> fromIntegral $ floor $ (mod' (fromRational t) 1) * 2

squarewave :: Sequence Double
squarewave = ((subtract 1) . (* 2)) <$> squarewave1

infixl 4 <~>
(<~>) :: Pattern p => Sequence (a -> b) -> p a -> Sequence b
(Sequence fs) <~> xs = 
  Sequence $ \r -> concatMap (\((s,e), f) -> 
                               map 
                               (\x -> ((s,e), f x))
                               (at (toSignal xs) s)
                             ) (fs r)
                   

-- Filter out events that start before range
filterOffsets :: Sequence a -> Sequence a
filterOffsets (Sequence f) = 
  Sequence $ \(s, e) -> filter ((>= s) . eventStart) $ f (s, e)

seqToRelOnsets :: Arc -> Sequence a -> [(Double, a)]
seqToRelOnsets (s, e) p = mapFsts (fromRational . (/ (e-s)) . (subtract s) . fst) $ arc (filterOffsets p) (s, e)

sample :: Int -> Signal a -> Sequence a
sample n p = listToPat (take n $ repeat id) <~> p


{-
-- Normalise range to positive duration
normaliseRange :: Range -> Range
normaliseRange r@(_, Nothing) = r
normaliseRange r@(s, Just d) | d < 0 = (s + d, Just $ 0 - d)
                             | otherwise = r

normaliseRange' :: (Time, Time) -> (Time, Time)
normaliseRange' r@(s, d) | d < 0 = (s + d, 0 - d)
                         | otherwise = r

--  (Signal fs) <*> px@(Sequence _) = 
--    Signal $ \t -> concatMap (\(_, x) -> map (\f -> f x) (fs t)) (range px (t,Nothing))

filterEvents :: (Event a -> Bool) -> Sequence a -> Sequence a
filterEvents f (Sequence a) = Sequence $ \r -> filter f $ a r

mapEvents :: (Event a -> Event a) -> Sequence a -> Sequence a
mapEvents f (Sequence a) = Sequence $ \r -> map f (a r)


revT :: (Time, Time) -> (Time, Time)
revT (s, d) = (s', d)
  where sam' = sam s
        x = s - sam'
        y = sam' - x - d
        z = y + d
        s' = y + (z - s)

--rev :: Pattern p => p a -> p a
--rev p = mapTimeOut revT $ mapOnset revT p

--revT :: Time -> Time
--revT = \t -> sam t + ((fromIntegral $ ceiling t) - t)

every :: Pattern p => Int -> (p a -> p a) -> p a -> p a
every 0 _ p = p
every n f p = slow (fromIntegral n %1) $ cat $ (take (n-1) $ repeat p) ++ [f p]


segment :: Sequence a -> Sequence [a]
segment p = Sequence $ \r -> groupByTime (segment' (range p r))

segment' :: [Event a] -> [Event a]
segment' es = foldr split es pts
  where pts = nub $ points es

groupByTime :: [Event a] -> [Event [a]]
groupByTime es = map mrg $ groupBy ((==) `on` fst) $ sortBy (compare `on` fst) es
  where mrg es@((a, _):_) = (a, map snd es)

split :: Time -> [Event a] -> [Event a]
split _ [] = []
split t ((e@((s,d), v)):es) | t > s && t < s+d = ((s,t-s),v):((t,(s+d)-t),v):(split t es)
                            | otherwise = e:split t es

points :: [Event a] -> [Time]
points [] = []
points (((s,d), _):es) = s:(s+d):(points es)

--nubSeq :: Eq a => Sequence a -> Sequence a
--nubSeq p = Sequence $ \r -> nub (range p r)
-}
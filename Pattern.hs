{-# LANGUAGE DeriveDataTypeable #-}

module Pattern where

import Control.Applicative
import Data.Monoid
import Data.Fixed
import Data.List
import Data.Maybe
import Data.Ratio
import Debug.Trace
import Data.Typeable
import Data.Function

import Time
import Utils

data Pattern a = Pattern {arc :: Arc -> [Event a]}

instance (Show a) => Show (Pattern a) where
  show p@(Pattern _) = show $ arc p (0, 1)
  
instance Functor Pattern where
  fmap f (Pattern a) = Pattern $ fmap (fmap (mapSnd f)) a

atom :: a -> Pattern a
atom x = Pattern f
  where f (s, e) = map 
                   (\t -> ((t%1, (t+1)%1), x))
                   [floor s .. ((ceiling e) - 1)]
  
silence :: Pattern a
silence = Pattern $ const []

mapQueryArc :: (Arc -> Arc) -> Pattern a -> Pattern a
mapQueryArc f p = Pattern $ \a -> arc p (f a)

mapQueryTime :: (Time -> Time) -> Pattern a -> Pattern a
mapQueryTime = mapQueryArc . mapArc

mapResultArc :: (Arc -> Arc) -> Pattern a -> Pattern a
mapResultArc f p = Pattern $ \a -> mapFsts f $ arc p a

mapResultTime :: (Time -> Time) -> Pattern a -> Pattern a
mapResultTime = mapResultArc . mapArc

overlay :: Pattern a -> Pattern a -> Pattern a
overlay p p' = Pattern $ \a -> (arc p a) ++ (arc p' a)

instance Applicative Pattern where
  pure = atom
  (Pattern fs) <*> (Pattern xs) = 
    Pattern $ \a ->
      concatMap
      (\((s,e),x) -> map 
                     (mapSnd ($ x))
                     (filter
                      (isIn (s,e) . eventStart)
                      (fs a)
                     )
      )
      (xs a)

instance Monad Pattern where
  return = pure
  p >>= f = 
    Pattern (\a -> concatMap 
                    (\(a', x) -> mapFsts (cutArc a) $ arc (f x) a')
                    (arc p a)
             )

instance Monoid (Pattern a) where
    mempty = silence
    mappend x y = Pattern $ \a -> (arc x a) ++ (arc y a)

cat :: [Pattern b] -> Pattern b
cat ps = stack $ map (squash l) (zip [0..] ps)
  where l = length ps

slowcat :: [Pattern b] -> Pattern b
slowcat ps = slow (fromIntegral $ length ps) $ cat ps

listToPat :: [a] -> Pattern a
listToPat = cat . map atom

squash :: Int -> (Int, Pattern a) -> Pattern a
squash parts (part, p) = Pattern f 
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

stack :: [Pattern a] -> Pattern a
stack ps = foldr overlay silence ps

density :: Time -> Pattern a -> Pattern a
density 1 p = p
density r p = mapResultTime (/ r) $ mapQueryTime (* r) p

slow :: Time -> Pattern a -> Pattern a
slow t = density (1/t) 

(<~) :: Time -> Pattern a -> Pattern a
(<~) t p = mapResultTime (+ t) $ mapQueryTime (subtract t) p

(~>) :: Time -> Pattern a -> Pattern a
(~>) = (<~) . (0-)

rev :: Pattern a -> Pattern a
rev p = Pattern $ \a -> concatMap 
                        (\a' -> mapFsts mirrorArc $ 
                                (arc p (mirrorArc a')))
                        (arcCycles a)

sig :: (Time -> a) -> Pattern a
sig f = Pattern f'
  where f' (s,e) | s > e = []
                 | otherwise = [((s,e), f s)]

sinewave :: Pattern Double
sinewave = sig $ \t -> sin $ pi * 2 * (fromRational t)

sinewave1 :: Pattern Double
sinewave1 = fmap ((/ 2) . (+ 1)) sinewave

sinePhase1 :: Double -> Pattern Double
sinePhase1 offset = (+ offset) <$> sinewave1

triwave1 :: Pattern Double
triwave1 = sig $ \t -> mod' (fromRational t) 1

triwave :: Pattern Double
triwave = ((subtract 1) . (* 2)) <$> triwave1


squarewave1 :: Pattern Double
squarewave1 = sig $ 
              \t -> fromIntegral $ floor $ (mod' (fromRational t) 1) * 2

squarewave :: Pattern Double
squarewave = ((subtract 1) . (* 2)) <$> squarewave1

-- Filter out events that start before range
filterOffsets :: Pattern a -> Pattern a
filterOffsets (Pattern f) = 
  Pattern $ \(s, e) -> filter ((>= s) . eventStart) $ f (s, e)

seqToRelOnsets :: Arc -> Pattern a -> [(Double, a)]
seqToRelOnsets (s, e) p = mapFsts (fromRational . (/ (e-s)) . (subtract s) . fst) $ arc (filterOffsets p) (s, e)

every :: Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
every 0 _ p = p
every n f p = slow (fromIntegral n %1) $ cat $ (take (n-1) $ repeat p) ++ [f p]


{-
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
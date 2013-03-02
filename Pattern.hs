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

instance Applicative Pattern where
  pure = atom
  (Pattern fs) <*> (Pattern xs) = 
    Pattern $ \a ->
      concatMap
      (\((s,e),x) -> map (mapSnd ($ x)) (filter
                                         (isIn (s,e) . eventStart)
                                         (fs a)
                                        )
      )
      (xs a)


instance Monoid (Pattern a) where
    mempty = silence
    mappend x y = Pattern $ \a -> (arc x a) ++ (arc y a)

instance Monad Pattern where
  return = pure
  p >>= f = 
    Pattern (\a -> concatMap 
                    (\(a', x) -> 
                      mapFsts (fromJust . (subArc a)) $
                      filter 
                      (isIn a . eventStart)
                      (arc (f x) a')
                    )
                    (arc p a)
             )

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

stack :: [Pattern a] -> Pattern a
stack ps = foldr overlay silence ps

cat :: [Pattern b] -> Pattern b
cat ps = density (fromIntegral $ length ps) $ slowcat ps

slowcat :: [Pattern a] -> Pattern a
slowcat [] = silence
slowcat ps = Pattern $ \a -> concatMap f (arcCycles a)
  where l = length ps
        f (s,e) = mapFsts arcF $ arc p (s', s' + (e - s))
          where p = ps !! n
                n = (floor s) `mod` l
                cyc = (floor s) - n
                s' = fromIntegral (cyc `div` l) + cyclePos s
                arcF (s'',e'') = (s''', s''' + (e'' - s''))
                  where s''' = (fromIntegral $ cyc + n) + (cyclePos s'')

listToPat :: [a] -> Pattern a
listToPat = cat . map atom

density :: Time -> Pattern a -> Pattern a
density 0 = id
density 1 p = p
density r p = mapResultTime (/ r) $ mapQueryTime (* r) p

slow :: Time -> Pattern a -> Pattern a
slow 0 = id
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



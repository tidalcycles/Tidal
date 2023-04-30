{-# LANGUAGE RankNTypes #-}

-- (c) Alex McLean, Aravind Mohandas and contributors 2022
-- Shared under the terms of the GNU Public License v3.0

module Sound.Tidal.Sequence2 where

import           Sound.Tidal.Time
-- import Sound.Tidal.Value
import           Data.List               (intersperse)
import           Data.Maybe              (fromMaybe)
import           Data.Ratio
import           Prelude                 hiding (span)
import           Sound.Tidal.Pattern
import           Sound.Tidal.Signal.Base ()
import           Sound.Tidal.Types

-- | Instances

instance Functor Sequence where
  fmap f (Atom d v) = Atom d (f v)
  fmap _ (Gap d)    = Gap d
  fmap f (Cat xs)   = Cat $ map (fmap f) xs
  fmap f (Stack xs) = Stack $ map (fmap f) xs

-- | Utils

seqTake :: Time -> Sequence a -> Maybe (Sequence a)
seqTake t (a@(Atom d v)) | t > d = Nothing
                         | otherwise = Just $ Atom t v
seqTake t (Gap d) | t > d = Nothing
                  | otherwise = Just (Gap t)
-- Return nothing if you ask for too much
seqTake t (Stack ss) = Stack <$> (sequence $ map (seqTake t) ss)
seqTake t (Cat ss) = Cat <$> (sequence $ step t ss)
  where step :: Time -> [Sequence a] -> [Maybe (Sequence a)]
        step 0 []  = []
        -- Return nothing if you ask for too much
        step t []  = [Nothing]
        step t (s:ss) | t <= 0 = []
                      | t <= stepDur = [seqTake t s]
                      | otherwise = (seqTake stepDur s):(step (t - stepDur) ss)
          where stepDur = seqDuration s

seqTake' :: Time -> Sequence a -> Sequence a
seqTake' t s = fromMaybe (Gap 0) $ seqTake t s -- TODO - error handling..

seqDrop :: Time -> Sequence a -> Maybe (Sequence a)
seqDrop t (a@(Atom d v)) | t > d = Nothing
                         | t == d = Just $ Gap 0
                         | otherwise = Just $ Atom (d - t) v
seqDrop t (Gap d) | t > d = Nothing
                  | otherwise = Just (Gap $ d - t)
-- Return nothing if you ask for too much
seqDrop t (Stack ss) = Stack <$> (sequence $ map (seqDrop t) ss)
seqDrop t (Cat ss) = Cat <$> (sequence $ step t ss)
  where step :: Time -> [Sequence a] -> [Maybe (Sequence a)]
        step 0 []  = []
        -- Return nothing if you ask for too much
        step t []  = [Nothing]
        step t (s:ss) | t <= 0 = []
                      | t <= stepDur = seqDrop t s:(map Just ss)
                      | otherwise = step (t - stepDur) ss
          where stepDur = seqDuration s

seqDrop' :: Time -> Sequence a -> Sequence a
seqDrop' t s = fromMaybe (Gap 0) $ seqDrop t s -- TODO - error handling..

seqDuration (Atom t _)    = t
seqDuration (Gap t)       = t
seqDuration (Cat xs)      = sum $ map seqDuration xs
seqDuration (Stack [])    = 0
seqDuration (Stack (x:_)) = seqDuration x

seqCount :: Sequence a -> Int
seqCount (Cat xs) = length xs
seqCount _        = 1

-- | Removes duplication, zero-width steps etc.
normalise :: Sequence a -> Sequence a
normalise (Cat [x]) = normalise x
normalise (Cat xs) = listToCat $ step $ map normalise xs
  where listToCat [x] = x
        listToCat xs  = Cat xs
        step []                = []
        step (Atom 0 _:xs)     = step xs
        step (Gap 0:xs)        = step xs
        step (Gap t:Gap t':xs) = step $ (Gap $ t + t'):xs
        step (Cat xs':xs)      = step $ xs' ++ xs
        step (x:xs)            = normalise x:step xs
normalise (Stack [x]) = normalise x
normalise (Stack xs) = listToStack $ step xs
  where listToStack [x] = x
        listToStack xs  = Stack xs
        step (Stack xs':xs) = step $ xs' ++ xs
        step (x:xs)         = normalise x:step xs
        step []             = []
normalise x = x

-- | Transformation

_seqFast :: Time -> Sequence a -> Sequence a
_seqFast t (Atom x s) = Atom (x/t) s
_seqFast t (Gap x)    = Gap (x/t)
_seqFast t (Cat s)    = Cat $ map (_seqFast t) s
_seqFast t (Stack x)  = Stack $ map(_seqFast t) x

_seqSlow :: Time -> Sequence a -> Sequence a
_seqSlow t = _seqFast (1/t)

-- TODO - more general version that takes rational
seqReplicate :: Int -> Sequence a -> Sequence a
seqReplicate n (Cat xs) = Cat $ concat $ replicate n xs
seqReplicate n x        = Cat $ replicate n x

-- | Alignment

seqPadBy :: ([Sequence a] -> Sequence a -> [Sequence a]) -> Time -> Sequence a -> Sequence a
seqPadBy by t x = f x
  where f (Cat xs) | t < 0 = error "Can't do negative pad"
                   | t == 0 = x
                   | otherwise = Cat $ by xs $ Gap $ t
        -- wrap in Cat for padding
        f x = seqPadBy by t $ Cat [x]

seqPadRightBy :: Time -> Sequence a -> Sequence a
seqPadRightBy = seqPadBy $ \xs x -> xs ++ [x]

seqPadLeftBy :: Time -> Sequence a -> Sequence a
seqPadLeftBy = seqPadBy $ flip (:)

seqPadBothBy :: Time -> Sequence a -> Sequence a
seqPadBothBy = seqPadBy (\xs x -> (x:xs) ++ [x])

seqSpaceOutBy :: Time -> Sequence a -> Sequence a
seqSpaceOutBy t (Cat ss) | t < 0 = error "Can't do negative pad"
                         | t == 0 = Cat ss
                         | otherwise = Cat $ intersperse gap ss
  where gap = Gap $ t / (toRational $ (length ss) - 1)
seqSpaceOutBy t s = seqSpaceOutBy t $ Cat [s]

seqRepeatTo :: Time -> Sequence a -> Sequence a
seqRepeatTo t (Cat ss) = seqTake' t $ Cat $ cycle ss
seqRepeatTo t s        = seqRepeatTo t $ Cat [s]

-- requires RankNTypes
withSmallest :: (forall x. Sequence x -> Sequence x) -> Sequence a -> Sequence b -> (Sequence a, Sequence b)
withSmallest f a b | o == LT = (f a, b)
                   | o == GT = (a, f b)
                   | otherwise = (a, b)
  where o = compare (seqDuration a) (seqDuration b)

withLargest :: (forall x. Sequence x -> Sequence x) -> Sequence a -> Sequence b -> (Sequence a, Sequence b)
withLargest f a b | o == LT = (a, f b)
                  | o == GT = (f a, b)
                  | otherwise = (a, b)
  where o = compare (seqDuration a) (seqDuration b)

-- Align two sequences so that they are the same overall duration,
-- according to a given strategy.

align :: Strategy -> Sequence a -> Sequence b -> (Sequence a, Sequence b)
align RepeatLCM a b = (rep a, rep b)
  where duration = lcmTime (seqDuration a) (seqDuration b)
        rep x = seqReplicate (floor $ duration / seqDuration x) x

align JustifyLeft a b = withSmallest (seqPadRightBy by) a b
  where by = abs $ seqDuration a - seqDuration b

align JustifyRight a b = withSmallest (seqPadLeftBy by) a b
  where by = abs $ seqDuration a - seqDuration b

align JustifyBoth a b = withSmallest (seqSpaceOutBy by) a b
  where by = abs $ seqDuration a - seqDuration b

align Centre a b = withSmallest (seqPadBothBy by) a b
  where by = abs $ (seqDuration a - seqDuration b) / 2

align Expand a b = withSmallest (_seqFast by) a b
  where ratio = seqDuration a / seqDuration b
        by | ratio < 1 = ratio
           | otherwise = 1/ratio

align TruncateLeft a b = withLargest (seqTake' $ min (seqDuration a) (seqDuration b)) a b

align TruncateRight a b = withLargest (seqDrop' $ abs $ (seqDuration a) - (seqDuration b)) a b

align TruncateRepeat a b = withSmallest (seqRepeatTo to) a b
  where to = max (seqDuration a) (seqDuration b)

-- data Strategy = / JustifyLeft
--               | / JustifyRight
--               | / RepeatLCM
--               | / Centre
--               | / Expand
--               | / JustifyBoth
--               | / TruncateLeft
--               | / TruncateRight
--               | / TruncateRepeat
--               | Squeeze
--               | SqueezeOut
--               | CycleIn
--               | CycleOut
--               | CycleMix
--               | Trig
--               | TrigZero


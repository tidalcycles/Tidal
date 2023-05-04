{-# LANGUAGE RankNTypes #-}

-- (c) Alex McLean, Aravind Mohandas and contributors 2022
-- Shared under the terms of the GNU Public License v3.0

module Sound.Tidal.Sequence2 where

import           Sound.Tidal.Time
-- import Sound.Tidal.Value
import           Data.List               (intersperse)
import           Data.Maybe              (fromMaybe)
import           Data.Ratio
import           Data.Tuple              (swap)
import           Prelude                 hiding (span)
import           Sound.Tidal.Pattern
import           Sound.Tidal.Show
import           Sound.Tidal.Signal.Base (_zoomArc)
import           Sound.Tidal.Types

-- | Instances

instance Functor Sequence where
  fmap f (Atom d i o v) = Atom d i o (f <$> v)
  fmap f (Cat xs)       = Cat $ map (fmap f) xs
  fmap f (Stack xs)     = Stack $ map (fmap f) xs


instance Applicative Sequence where
  pure x = Atom 1 0 0 $ Just x
  fseq <*> vseq = (\(f,v) -> f v) <$> pairAlign RepeatLCM In fseq vseq

-- | TODO - does this need to be more complicated?
instance Monad Sequence where
  return = pure
  Atom _ _ _ (Just v) >>= f = f v
  Atom d i o Nothing >>= _  = Atom d i o Nothing
  Cat xs >>= f              = Cat $ map (>>= f) xs
  Stack xs >>= f            = Stack $ map (>>= f) xs

instance Pattern Sequence where
  toSignal = seqToSignal
  slowcat = seqCat
  fastcat = seqFastcat
--   slowcat :: [p a] -> p a
--   fastcat :: [p a] -> p a
--   fastcat pats = _fast (toRational $ length pats) $ slowcat pats
--   _fast :: Time -> p a -> p a
--   silence :: p a
--   atom :: a -> p a
--   stack :: [p a] -> p a
--   -- patternify the first parameter
--   _patternify :: (a -> p b -> p c) -> (p a -> p b -> p c)
--   -- patternify the first two parameters
--   _patternify_p_p :: (a -> b -> p c -> p d) -> (p a -> p b -> p c -> p d)
--   -- patternify the first but not the second parameters
--   _patternify_p_n :: (a -> b -> p c -> p d) -> (p a -> b -> p c -> p d)
--   -- patternify the first three parameters
--   _patternify_p_p_p :: (a -> b -> c -> p d -> p e) -> (p a -> p b -> p c -> p d -> p e)
--   _appAlign :: (a -> p b -> p c) -> Align (p a) (p b) -> p c
--   rev :: p a -> p a
--   _ply :: Time -> p a-> p a
--   euclid :: p Int -> p Int -> p a -> p a
--   _euclid :: Int -> Int -> p a -> p a
--   timeCat :: [(Time, p a)] -> p a
--   _run :: (Enum a, Num a) => a -> p a
--   _scan :: (Enum a, Num a) => a -> p a
--   -- every :: p Int -> (p b -> p b) -> p b -> p b
--   when :: p Bool -> (p b -> p b) -> p b -> p b
--   -- listToPat :: [a] -> p a
--   _iter :: Int -> p a -> p a
--   _iterBack :: Int -> p a -> p a
--   collect :: Eq a => p a -> p [a]
--   uncollect :: p [a] -> p a
--   _pressBy :: Time -> p a -> p a

-- | Pattern instance implementations

-- One beat per cycle
seqToSignal :: Sequence a -> Signal a
seqToSignal seq = _slow (seqDuration seq) $ seqToSignal' seq

-- One sequence per cycle
seqToSignal' :: Sequence a -> Signal a
seqToSignal' (Atom t i o (Just v)) = _zoomArc (Arc (i/t) (1 - (o/t))) $ pure v
seqToSignal' (Cat xs) = timeCat $ timeseqs
  where timeseqs = map (\x -> (seqDuration x, seqToSignal' x)) xs
seqToSignal' (Stack xs) = stack $ map seqToSignal' xs

seqAppend :: Sequence a -> Sequence a -> Sequence a
seqAppend (Cat as) (Cat bs) = Cat (as ++ bs)
seqAppend a (Cat bs)        = Cat (a:bs)
seqAppend (Cat as) b        = Cat (as ++ [b])
seqAppend a b               = Cat [a,b]

seqCat :: [Sequence a] -> Sequence a
seqCat ([])     = Cat []
seqCat (a:[])   = a
seqCat (a:b:[]) = seqAppend a b
seqCat (a:xs)   = seqAppend a $ seqCat xs

seqFastcat :: [Sequence a] -> Sequence a
seqFastcat xs = _slow (sum $ map seqDuration xs) $ seqCat xs

-- | Utils

gap :: Time -> Sequence a
gap t = Atom t 0 0 Nothing

step :: Time -> a -> Sequence a
step t v = Atom t 0 0 $ Just v

seqTake :: Time -> Sequence a -> Maybe (Sequence a)
seqTake t (a@(Atom d i o v)) | t > d = Nothing
                             | otherwise = Just $ Atom t i o v
-- Return nothing if you ask for too much
seqTake t (Stack ss) = Stack <$> (sequence $ map (seqTake t) ss)
seqTake t (Cat ss) = Cat <$> (sequence $ loop t ss)
  where loop :: Time -> [Sequence a] -> [Maybe (Sequence a)]
        loop 0 []  = []
        -- Return nothing if you ask for too much
        loop t []  = [Nothing]
        loop t (s:ss) | t <= 0 = []
                      | t <= stepDur = [seqTake t s]
                      | otherwise = (seqTake stepDur s):(loop (t - stepDur) ss)
          where stepDur = seqDuration s

seqTake' :: Time -> Sequence a -> Sequence a
seqTake' t s = fromMaybe (gap 0) $ seqTake t s -- TODO - error handling..

seqDrop :: Time -> Sequence a -> Maybe (Sequence a)
seqDrop t (a@(Atom d i o v)) | t > d = Nothing
                             | t == d = Just $ gap 0
                             | otherwise = Just $ Atom (d - t) (i + t) o v
-- Return nothing if you ask for too much
seqDrop t (Stack ss) = Stack <$> (sequence $ map (seqDrop t) ss)
seqDrop t (Cat ss) = Cat <$> (sequence $ loop t ss)
  where loop :: Time -> [Sequence a] -> [Maybe (Sequence a)]
        loop 0 []  = []
        -- Return nothing if you ask for too much
        loop t []  = [Nothing]
        loop t (s:ss) | t <= 0 = []
                      | t <= stepDur = seqDrop t s:(map Just ss)
                      | otherwise = loop (t - stepDur) ss
          where stepDur = seqDuration s

seqDrop' :: Time -> Sequence a -> Sequence a
seqDrop' t s = fromMaybe (gap 0) $ seqDrop t s -- TODO - error handling..

-- TODO - optimise
seqSplitAt :: Time -> Sequence a -> Maybe (Sequence a, Sequence a)
seqSplitAt t s = do a <- seqTake t s
                    b <- seqDrop t s
                    return (a,b)

seqSplitAt' :: Time -> Sequence a -> (Sequence a, Sequence a)
seqSplitAt' t s = fromMaybe (gap 0, gap 0) $ seqSplitAt t s -- TODO - error handling..

seqDuration (Atom d _ _ _) = d
seqDuration (Cat xs)       = sum $ map seqDuration xs
seqDuration (Stack [])     = 0
seqDuration (Stack (x:_))  = seqDuration x

seqCount :: Sequence a -> Int
seqCount (Cat xs) = length xs
seqCount _        = 1

-- | Removes duplication, zero-width steps etc.
normalise :: Sequence a -> Sequence a
normalise (Cat [x]) = normalise x
normalise (Cat xs) = listToCat $ loop $ map normalise xs
  where listToCat [x] = x
        listToCat xs  = Cat xs
        loop []                = []
        loop (Atom 0 _ _ _:xs) = loop xs
        loop (Atom t _ _ Nothing:Atom t' _ _ Nothing:xs) = loop $ (gap $ t + t'):xs
        loop (Cat xs':xs)      = loop $ xs' ++ xs
        loop (x:xs)            = normalise x:loop xs
normalise (Stack [x]) = normalise x
normalise (Stack xs) = listToStack $ loop xs
  where listToStack [x] = x
        listToStack xs  = Stack xs
        loop (Stack xs':xs) = loop $ xs' ++ xs
        loop (x:xs)         = normalise x:loop xs
        loop []             = []
normalise x = x

-- | Transformation

_seqFast :: Time -> Sequence a -> Sequence a
_seqFast t (Atom d i o s) = Atom (d/t) (i/t) (o/t) s
_seqFast t (Cat s)        = Cat $ map (_seqFast t) s
_seqFast t (Stack x)      = Stack $ map(_seqFast t) x

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
                   | otherwise = Cat $ by xs $ gap t
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
                         | otherwise = Cat $ intersperse g ss
  where g = gap (t / (toRational $ (length ss) - 1))
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

align SqueezeIn (Cat xs) b = (Cat xs, squeezed)
  where squeezed = Cat $ map (\x -> squash (seqDuration x) b) xs
        squash t x = _seqFast (seqDuration x / t) x
align SqueezeIn x b = align SqueezeIn (Cat [x]) b

align SqueezeOut a b = swap $ align SqueezeIn b a

align strategy _ _ = error $ show strategy ++ " not implemented for sequences."

-- pairAligned :: Strategy -> Direction -> Sequence a -> Sequence b -> [(Sequence a, Sequence b)]
-- pairAligned s In (Cat xs) b = step xs b
--   where step [] _ = []
--         step (x:xs) b' = (x, h):(step xs t)
--           where (h,t) = seqSplitAt' (seqDuration x) b'

pairAligned :: Direction -> Sequence a -> Sequence b -> Sequence (a, b)

-- TODO - vertical alignments
pairAligned strategy (Stack as) b = seqCat $ map (\a -> pairAligned strategy a b) as
pairAligned strategy a (Stack bs) = seqCat $ map (\b -> pairAligned strategy a b) bs

pairAligned In (Cat as) b = loop 0 as b
  where loop t ([]) _ = gap 0
        loop t (a:[]) b = pairAligned In a b
        loop t (a:as) b = seqAppend (pairAligned In a b') $ loop t' as b''
          where t' = t + seqDuration a
                (b', b'') = seqSplitAt' t' b

pairAligned In a (Cat bs) = loop 0 a bs
  where loop t _ ([]) = gap 0
        loop t a (b:[]) = pairAligned In a b
        loop t a (b:bs) = seqAppend (pairAligned In a' b) $ loop t' a'' bs
          where t' = t + seqDuration b
                (a', a'') = seqSplitAt' t' a


pairAligned Out a b = swap <$> pairAligned In b a

-- TODO - should the value be Nothing if one/both are nothings?
pairAligned In (Atom d i o v) (Atom d' i' o' v') = Atom d i o $ do a <- v
                                                                   b <- v'
                                                                   return (a,b)

pairAlign :: Strategy -> Direction -> Sequence a -> Sequence b -> Sequence (a, b)
pairAlign s d a b = pairAligned d a' b'
  where (a', b') = align s a b


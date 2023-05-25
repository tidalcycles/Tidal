{-# LANGUAGE RankNTypes #-}

-- (c) Alex McLean, Aravind Mohandas and contributors 2022
-- Shared under the terms of the GNU Public License v3.0

module Sound.Tidal.Sequence where

import           Sound.Tidal.Time
-- import Sound.Tidal.Value
import           Data.List               (intersperse)
import           Data.Maybe              (fromMaybe)
import           Data.Ratio
import           Data.Tuple              (swap)
import           Prelude                 hiding (seq, span)
import           Sound.Tidal.Bjorklund   (bjorklund)
import           Sound.Tidal.Pattern
import           Sound.Tidal.Show        ()
import           Sound.Tidal.Signal.Base (_zoomArc)
import           Sound.Tidal.Types

-- | Instances


seqNoOv :: String -> a
seqNoOv meth = error $ meth ++ ": not supported for sequences"

instance Num a => Num (Sequence a) where
  negate      = fmap negate
  (+)         = liftA2 (+)
  (*)         = liftA2 (*)
  fromInteger = pure . fromInteger
  abs         = fmap abs
  signum      = fmap signum

instance Enum a => Enum (Sequence a) where
  succ           = fmap succ
  pred           = fmap pred
  toEnum         = pure . toEnum
  fromEnum       = seqNoOv "fromEnum"
  enumFrom       = seqNoOv "enumFrom"
  enumFromThen   = seqNoOv "enumFromThen"
  enumFromTo     = seqNoOv "enumFromTo"
  enumFromThenTo = seqNoOv "enumFromThenTo"

instance (Num a, Ord a) => Real (Sequence a) where
  toRational = seqNoOv "toRational"

instance (Integral a) => Integral (Sequence a) where
  quot          = liftA2 quot
  rem           = liftA2 rem
  div           = liftA2 div
  mod           = liftA2 mod
  toInteger     = seqNoOv "toInteger"
  x `quotRem` y = (x `quot` y, x `rem` y)
  x `divMod`  y = (x `div`  y, x `mod` y)

instance (Fractional a) => Fractional (Sequence a) where
  recip        = fmap recip
  fromRational = pure . fromRational

instance (Floating a) => Floating (Sequence a) where
  pi    = pure pi
  sqrt  = fmap sqrt
  exp   = fmap exp
  log   = fmap log
  sin   = fmap sin
  cos   = fmap cos
  asin  = fmap asin
  atan  = fmap atan
  acos  = fmap acos
  sinh  = fmap sinh
  cosh  = fmap cosh
  asinh = fmap asinh
  atanh = fmap atanh
  acosh = fmap acosh

instance (RealFrac a) => RealFrac (Sequence a) where
  properFraction = seqNoOv "properFraction"
  truncate       = seqNoOv "truncate"
  round          = seqNoOv "round"
  ceiling        = seqNoOv "ceiling"
  floor          = seqNoOv "floor"

instance (RealFloat a) => RealFloat (Sequence a) where
  floatRadix     = seqNoOv "floatRadix"
  floatDigits    = seqNoOv "floatDigits"
  floatRange     = seqNoOv "floatRange"
  decodeFloat    = seqNoOv "decodeFloat"
  encodeFloat    = ((.).(.)) pure encodeFloat
  exponent       = seqNoOv "exponent"
  significand    = seqNoOv "significand"
  scaleFloat n   = fmap (scaleFloat n)
  isNaN          = seqNoOv "isNaN"
  isInfinite     = seqNoOv "isInfinite"
  isDenormalized = seqNoOv "isDenormalized"
  isNegativeZero = seqNoOv "isNegativeZero"
  isIEEE         = seqNoOv "isIEEE"
  atan2          = liftA2 atan2

instance Functor Sequence where
  fmap f (Atom d i o v) = Atom d i o (f <$> v)
  fmap f (Cat xs)       = Cat $ map (fmap f) xs
  fmap f (Stack xs)     = Stack $ map (fmap f) xs

instance Applicative Sequence where
  pure = step 1
  fseq <*> vseq = (\(f,v) -> f v) <$> pairAlign Repeat In fseq vseq

join :: Sequence (Sequence a) -> Sequence a
join (Atom d i o (Just seq)) = _fast (innerDur / d) $ actives
  where innerDur = seqDuration seq
        skipHead = (i/d) * innerDur
        skipTail = (o/d) * innerDur
        body = innerDur - (skipHead + skipTail)
        actives = seqTake' body $ seqDrop' skipHead seq
join (Atom d i o Nothing) = Atom d i o Nothing
join (Cat xs) = Cat $ map join xs
join (Stack xs) = Stack $ map join xs

-- | TODO - does this need to be more complicated?
instance Monad Sequence where
  return = pure
  seqv >>= f = join $ fmap f seqv

instance Pattern Sequence where
  toSignal = seqToSignal
  slowcat = seqCat
  fastcat = seqFastcat
  _fast = _seqFast
  silence = gap 1
  atom = step 1
  stack = Stack
  innerJoin = join -- TODO - is this right?
  (<*) = (<*>) -- TODO - are these right? Probably not..
  (*>) = (<*>)
  rev = seqRev
  _ply = _seqPly
  timeCat = seqTimeCat
  _iter = _seqIter
  _iterBack = _seqIterBack
  _pressBy = _seqPressBy
  when = seqWhenS Expand
  _euclid = _seqEuclid
  collect = seqCollect
  uncollect = seqUncollect
  filterOnsets = seqFilterOnsets
  filterValues = seqFilterValues

filterAtoms :: (Sequence a -> Bool) -> Sequence a -> Sequence a
filterAtoms f a@(Atom d i o _) | f a = a
                               | otherwise = Atom d i o Nothing
filterAtoms f (Cat xs)         = Cat $ map (filterAtoms f) xs
filterAtoms f (Stack xs)       = Stack $ map (filterAtoms f) xs

seqFilterOnsets :: Sequence a -> Sequence a
seqFilterOnsets = filterAtoms f
  where f (Atom _ 0 _ _) = True
        f _              = False

seqFilterValues :: (a -> Bool) -> Sequence a -> Sequence a
seqFilterValues f = filterAtoms f'
  where f' a@(Atom _ _ _ Nothing) = True
        f' (Atom _ _ _ (Just v))  = f v
        f' _                      = False

-- | Utils

gap :: Time -> Sequence a
gap t = Atom t 0 0 Nothing

step :: Time -> a -> Sequence a
step t v = Atom t 0 0 $ Just v

firstDuration :: Sequence a -> Time
firstDuration (Cat (x:_)) = seqDuration x
firstDuration x           = seqDuration x

takeFirst :: Sequence a -> Sequence a
takeFirst (Cat (x:_)) = x
takeFirst x           = x

seqTake :: Time -> Sequence a -> Maybe (Sequence a)
seqTake 0 _ = Just $ gap 0
seqTake t (Atom d i _ v) | t > d = Nothing
                         | otherwise = Just $ Atom t i (max 0 $ d - t) v
-- Return nothing if you ask for too much
seqTake t (Stack ss) = Stack <$> (sequence $ map (seqTake t) ss)
seqTake t (Cat ss) = Cat <$> (sequence $ loop t ss)
  where loop :: Time -> [Sequence a] -> [Maybe (Sequence a)]
        loop 0 []  = []
        -- Return nothing if you ask for too much
        loop _ []  = [Nothing]
        loop t' (s:ss') | t' <= 0 = []
                        | t' <= stepDur = [seqTake t' s]
                        | otherwise = (seqTake stepDur s):(loop (t' - stepDur) ss')
          where stepDur = seqDuration s

seqTake' :: Time -> Sequence a -> Sequence a
seqTake' t s = fromMaybe (gap 0) $ seqTake t s -- TODO - error handling..

seqDrop :: Time -> Sequence a -> Maybe (Sequence a)
seqDrop 0 s = Just s
seqDrop t (Atom d i o v) | t > d = Nothing
                         | t == d = Just $ gap 0
                         | otherwise = Just $ Atom (d - t) (i + t) o v

seqDrop t (Stack ss) = Stack <$> (sequence $ map (seqDrop t) ss)
seqDrop t (Cat ss) = Cat <$> (sequence $ loop t ss)
  where loop :: Time -> [Sequence a] -> [Maybe (Sequence a)]
        loop 0 []  = []
        -- Return nothing if you ask for too much
        loop _ []  = [Nothing]
        loop t' (s:ss') | t' <= 0 = []
                        | t' == stepDur = map Just ss'
                        | t' <= stepDur = seqDrop t' s:(map Just ss')
                        | otherwise = loop (t' - stepDur) ss'
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

seqDuration :: Sequence a -> Time
seqDuration (Atom d _ _ _) = d
seqDuration (Cat xs)       = sum $ map seqDuration xs
seqDuration (Stack [])     = 0
seqDuration (Stack (x:_))  = seqDuration x

seqCount :: Sequence a -> Int
seqCount (Cat xs) = length xs
seqCount _        = 1

-- | Removes duplication, zero-width steps etc.
-- TODO - do we really want to use this internally? E.g. a stack of
-- stacks might represent structure rather than being redundant.
normalise :: Sequence a -> Sequence a
normalise (Cat [x]) = normalise x
normalise (Cat xs) = listToCat $ loop $ map normalise xs
  where listToCat [x] = x
        listToCat xs' = Cat xs'
        loop []                 = []
        loop (Atom 0 _ _ _:xs') = loop xs'
        loop (Atom t _ _ Nothing:Atom t' _ _ Nothing:xs') = loop $ (gap $ t + t'):xs'
        loop (Cat xs':xs'')     = loop $ xs' ++ xs''
        loop (x:xs')             = normalise x:loop xs'
normalise (Stack [x]) = normalise x
normalise (Stack xs) = listToStack $ loop xs
  where listToStack [x] = x
        listToStack xs' = Stack xs'
        loop (Stack xs':xs'') = loop $ xs' ++ xs''
        loop (x:xs')          = normalise x:loop xs'
        loop []               = []
normalise x = x

withAtom :: (Sequence a -> Sequence a) -> Sequence a -> Sequence a
withAtom f a@(Atom _ _ _ _) = f a
withAtom f (Cat xs)         = Cat $ map (withAtom f) xs
withAtom f (Stack xs)       = Stack $ map (withAtom f) xs

-- | Pattern instance implementations

-- One beat per cycle
seqToSignal :: Sequence a -> Signal a
seqToSignal seq = _slow (seqDuration seq) $ seqToSignal' seq

-- One sequence per cycle
seqToSignal' :: Sequence a -> Signal a
seqToSignal' (Atom d i o (Just v)) | d == 0 = error "whoops"
                                   | otherwise = _zoomArc (Arc i (1-o)) $ pure v
seqToSignal' (Atom _ _ _ Nothing) = silence
seqToSignal' (Cat xs) = timeCat $ timeseqs
  where timeseqs = map (\x -> (seqDuration x, seqToSignal' x)) xs
seqToSignal' (Stack xs) = stack $ map seqToSignal' xs

toCycle :: Rational -> Sequence a -> Signal a
toCycle beats seq = _fast beats $ seqToSignal seq

beatMode :: Rational -> Sequence a -> Signal a
beatMode = toCycle

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
seqFastcat xs = _fast (sum $ map seqDuration xs) $ seqCat xs

seqRev :: Sequence a -> Sequence a
seqRev (Stack xs) = Stack $ map seqRev xs
seqRev (Cat xs)   = withAtom swapio $ Cat $ reverse $ map seqRev xs
  where swapio (Atom d i o v) = Atom d o i v
        swapio seq            = seq -- shouldn't happen
seqRev x          = x

_seqPly :: Time -> Sequence a -> Sequence a
_seqPly t (Stack xs)             = Stack $ map (_seqPly t) xs
-- TODO more efficient catmap that avoids cats within cats
_seqPly t (Cat xs)               = normalise $ Cat $ map (_seqPly t) xs
-- You can't ply nothing, just make it longer
_seqPly t a@(Atom _ _ _ Nothing) = _seqSlow t a
_seqPly t seq                    = seqTake' t $ Cat $ repeat seq

seqTimeCat :: [(Time, Sequence a)] -> Sequence a
seqTimeCat seqs = normalise $ join $ Cat $ map (uncurry step) seqs

_seqIter :: Int -> Sequence a -> Sequence a
_seqIter 0 seq = seq
_seqIter 1 seq = seq
_seqIter n seq = normalise $ Cat $ map fori [0 .. (n-1)]
  where d = seqDuration seq
        fori 0 = seq
        fori i = swapcat $ seqSplitAt' (d*((fromIntegral i)%(fromIntegral n))) seq
        swapcat (a,b) = Cat [b,a]

_seqIterBack :: Int -> Sequence a -> Sequence a
_seqIterBack 0 seq = seq
_seqIterBack 1 seq = seq
_seqIterBack n seq = normalise $ Cat $ map fori [0 .. (n-1)]
  where d = seqDuration seq
        fori 0 = seq
        fori i = swapcat $ seqSplitAt' (d-(d*((fromIntegral i)%(fromIntegral n)))) seq
        swapcat (a,b) = Cat [b,a]

_seqPressBy :: Time -> Sequence a -> Sequence a
_seqPressBy t seq = normalise $ join $ fmap (\v -> Cat [gap t, step (1-t) v]) seq

seqMosesS :: Strategy -> Sequence Bool -> (Sequence b -> Sequence b) -> (Sequence b -> Sequence b) -> Sequence b -> Sequence b
seqMosesS strategy bseq fyes fno seq = normalise $ loop bseq' seq'
  where (bseq', seq') = align strategy bseq seq
        loop (Stack as) b = Stack $ map (\a -> loop a b) as
        loop (Cat as) b = Cat $ subloop as b
          where subloop [] _ = []
                subloop (a:as') b' = loop a (seqTake' (seqDuration a) b'):(subloop as' $ seqDrop' (seqDuration a) b')

        loop (Atom _ _ _ (Just True)) b = fyes b -- TODO - double check a and b are equal in duration? they should be..
        loop (Atom _ _ _ _) b = fno b

seqWhenS :: Strategy -> Sequence Bool -> (Sequence b -> Sequence b) -> Sequence b -> Sequence b
seqWhenS strategy bseq fyes seq = seqMosesS strategy bseq fyes id seq

-- TODO - take structure from binary pattern..
_seqEuclid :: Int -> Int -> Sequence a -> Sequence a
_seqEuclid n k seq | n >= 0 = seqMosesS Expand bseq (id) (gap . seqDuration) seq
                   | otherwise = seqMosesS Expand bseq (gap . seqDuration) (id) seq
              where bseq = Cat $ map (step 1) $ bjorklund (abs n,k)

seqCollect :: Eq a => Sequence a -> Sequence [a]
seqCollect = error "collect is not yet defined"

seqUncollect :: Sequence [a] -> Sequence a
seqUncollect seq = loop seq
  where loop (Atom d i o (Just xs)) = Stack $ map (Atom d i o . Just) xs
        loop (Atom d i o Nothing)   = (Atom d i o Nothing)
        loop (Cat xs)               = Cat $ map loop xs
        loop (Stack xs)             = Stack $ map loop xs

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

-- | Combination

poly :: [Sequence a] -> Sequence a
poly xs = normalise $ poly' xs
  where poly' ([])   = silence
        poly' (x:[]) = x
        poly' (x:xs') = Stack [a,b]
          where (a, b) = align Repeat x $ poly xs'

-- | Alignment

seqPadBy :: ([Sequence a] -> Sequence a -> [Sequence a]) -> Time -> Sequence a -> Sequence a
seqPadBy by t x = f x
  where f (Cat xs) | t < 0 = error "Can't do negative pad"
                   | t == 0 = x
                   | otherwise = Cat $ by xs $ gap t
        -- wrap in Cat for padding
        f x' = seqPadBy by t $ Cat [x']

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
align Repeat a b = (rep a, rep b)
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

alignStack :: Strategy -> Sequence a -> Sequence a -> Sequence a
alignStack s a b = Stack $ (\(a',b') -> [a',b']) $ align s a b

pairAligned :: Direction -> (Sequence a, Sequence b) -> Sequence (a, b)

-- TODO - vertical alignments
-- TODO - 'Mixed' direction
pairAligned direction ((Stack as), b) = seqCat $ map (\a -> pairAligned direction (a, b)) as
pairAligned direction (a, (Stack bs)) = seqCat $ map (\b -> pairAligned direction (a, b)) bs

-- TODO - should the value be Nothing if one/both are nothings?
pairAligned In ((Atom d i o v), (Atom _ _ _ v')) = Atom d i o $ do a <- v
                                                                   b <- v'
                                                                   return (a,b)

pairAligned In (Cat xs, Cat ys) = Cat $ loop xs ys
  where loop ([]) _ = []
        loop _ ([]) = []
        loop (a:as) (b:bs) | cmp == LT = (pairAligned In (a, b')):(loop as (b'':bs))
                           | cmp == GT = (pairAligned In (a', b)):(loop (a'':as) bs)
                           | cmp == EQ = (pairAligned In (a, b)):(loop as bs)
          where adur = seqDuration a
                bdur = seqDuration b
                cmp = compare adur bdur
                (a', a'') = seqSplitAt' bdur a
                (b', b'') = seqSplitAt' adur b

pairAligned In ((Cat xs), y) = loop 0 xs y
  where loop _ ([]) _ = gap 0
        loop _ (a:[]) b = pairAligned In (a, b)
        loop t (a:as) b = seqAppend (pairAligned In (a, b')) $ loop t' as b''
          where t' = t + seqDuration a
                (b', b'') = seqSplitAt' t' b

pairAligned In (x, (Cat ys)) = loop 0 x ys
  where loop :: Time -> (Sequence a) -> [Sequence b] -> Sequence (a,b)
        loop _ _ ([]) = gap 0
        loop _ a (b:[]) = pairAligned In (a, b)
        loop t a (b:bs) = seqAppend (pairAligned In (a', b)) $ loop t' a'' bs
          where t' = t + seqDuration b
                (a', a'') = seqSplitAt' t' a


pairAligned Out (a, b) = swap <$> pairAligned In (b, a)

pairAlign :: Strategy -> Direction -> Sequence a -> Sequence b -> Sequence (a, b)
pairAlign s d a b = pairAligned d $ align s a b

alignF :: Strategy -> Direction -> (a -> b -> c) -> Sequence a -> Sequence b -> Sequence c
alignF s d f a b = (uncurry f) <$> pairAlign s d a b

-- expandIn :: (a -> b -> c) -> Sequence a -> Sequence b -> Sequence c
-- expandIn f a b = alignF Expand In


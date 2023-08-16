{-# LANGUAGE RankNTypes #-}

module Sound.Tidal.Sequence where

import           Data.Fixed          (mod')
import           Data.List           (intercalate, intersperse)
import           Data.Ratio
import           Data.Tuple          (swap)

import           Sound.Tidal.Pattern
import           Sound.Tidal.Signal  ()
import           Sound.Tidal.Time
import           Sound.Tidal.Types

instance Functor Sequence where
  fmap f (Atom m d i o v) = Atom m d i o (f <$> v)
  fmap f (Cat xs)         = Cat $ map (fmap f) xs
  fmap f (Stack xs)       = Stack $ map (fmap f) xs

-- instance Sequenceable (Sequence a) a where toSeq = id
-- instance Sequenceable a a where toSeq = pure

-- new approach to patternify - allows strategy and bind to be set per parameter
alignify :: Alignment x => (a -> Sequence b -> Sequence c) -> x a -> Sequence b -> Sequence c
alignify f alignSeq seqb = seqa' `aBind` \a -> f a seqb'
  where SeqStrategy strat dir seqa = toSeqStrategy alignSeq
        (seqa', seqb') = align strat seqa seqb
        aBind | dir == Inner = innerBind
              | dir == Outer = outerBind
              | otherwise = (>>=)

-- alignify parameters
afast, aslow, aearly, alate :: Alignment x => x Time -> Sequence a -> Sequence a
afast  = alignify _fast
aslow  = alignify _slow
aearly = alignify _early
alate  = alignify _late

prettyRatio :: Rational -> String
prettyRatio r | denominator r == 1 = show $ numerator r
              | otherwise = show (numerator r) ++ "/" ++ show (denominator r)

instance (Show a) => Show (Sequence a) where
  show (Atom _ d _ _ Nothing) = "~" ++ "×" ++ prettyRatio d
  show (Atom _ d i o (Just v)) = show v ++ "×" ++ prettyRatio d ++ showio
    where showio | i == 0 && o == 0 = ""
                 | otherwise = "(" ++ prettyRatio i ++ "," ++ prettyRatio o ++ ")"
  show (Cat xs) = "[" ++ unwords (map show xs) ++ "]"
  show (Stack xs) = "[\n" ++ intercalate ", \n" (map show xs) ++ "\n]"

gap :: Time -> Sequence a
gap t = Atom mempty t 0 0 Nothing

step :: Time -> a -> Sequence a
step t v = Atom mempty t 0 0 $ Just v

-- | Removes duplication, zero-width steps etc.
-- TODO - do we really want to use this internally? E.g. a stack of
-- stacks might represent structure rather than being redundant.
normalise :: Sequence a -> Sequence a
normalise (Cat [x]) = normalise x
normalise (Cat xs) = listToCat $ loop $ map normalise xs
  where listToCat [x] = x
        listToCat xs' = Cat xs'
        loop []                   = []
        loop (Atom m 0 _ _ _:xs') = map (addMetadata m) $ loop xs'
        loop (Atom m t _ _ Nothing:Atom m' t' _ _ Nothing:xs')
          = map (addMetadata (m <> m')) $ loop $ gap (t + t'):xs'
        loop (Cat xs':xs'')       = loop $ xs' ++ xs''
        loop (x:xs')              = normalise x:loop xs'
normalise (Stack [x]) = normalise x
normalise (Stack xs) = listToStack $ loop xs
  where listToStack [x] = x
        listToStack xs' = Stack xs'
        loop (Stack xs':xs'') = loop $ xs' ++ xs''
        loop (x:xs')          = normalise x:loop xs'
        loop []               = []
normalise x = x

seqJoinWith :: (Time -> Time -> Time -> Sequence a -> Sequence a) -> Sequence (Sequence a) -> Sequence a
seqJoinWith f (Atom m d i o (Just s)) = addMetadata m $ f d i o s
seqJoinWith _ (Atom m d i o Nothing)  = Atom m d i o Nothing
seqJoinWith f (Cat xs)                = Cat $ map (seqJoinWith f) xs
seqJoinWith f (Stack xs)              = Stack $ map (seqJoinWith f) xs

seqJoinWithTime :: (Time -> Sequence a -> Sequence a) -> Sequence (Sequence a) -> Sequence a
seqJoinWithTime f = seqJoinWith $ \d i o s -> f (d + i + o) s

seqJoinWithSpan :: (Span -> Time -> Sequence a -> Sequence a) -> Sequence (Sequence a) -> Sequence a
seqJoinWithSpan f pat = loop 0 pat
  where patd = duration pat
        -- Pass d rather than d + i + o ?
        loop pos (Atom _ d _ _ (Just s)) = f (Span (pos/patd) ((pos + d)/patd)) d s
        loop _ (Atom m d i o Nothing)    = Atom m d i o Nothing
        loop pos (Stack xs  )            = Stack $ map (loop pos) xs
        loop pos (Cat xs)                = Cat $ loop' pos xs
          where loop' _ []         = []
                loop' pos' (x:xs') = (loop pos' x):(loop' (pos' + duration x) xs')

-- Flatten, using outer duration as relative duration for inner
seqJoin :: Sequence (Sequence a) -> Sequence a
seqJoin = seqJoinWithTime _slow

-- Flatten, expanding inner to outer duration
seqExpandJoin :: Sequence (Sequence a) -> Sequence a
seqExpandJoin = seqJoinWithTime (\t s -> _fast (duration s / t) s)

-- Flatten, repeating inner to total duration of outer
seqLoopJoin :: Sequence (Sequence a) -> Sequence a
seqLoopJoin = seqJoinWithTime seqTakeLoop

-- Flatten, changing duration of outer to fit inner
seqInnerJoin :: Sequence (Sequence a) -> Sequence a
seqInnerJoin pat = seqJoinWithSpan f pat
  -- TODO: 'd' isn't used here..
  where f (Span b e) {- d -} _ pat' = seqTakeLoop ((e-b)*d') $ seqDrop (b*d') pat'
          where d' = duration pat'

-- Flatten, changing duration of inner to fit outer
seqOuterJoin :: Sequence (Sequence a) -> Sequence a
seqOuterJoin pat = _fast (duration inner / duration pat) inner
  where inner = seqInnerJoin pat

-- Flatten, set duration of inner sequence to fit outer atom durations
seqSqueezeJoin :: Sequence (Sequence a) -> Sequence a
seqSqueezeJoin = seqJoinWith $ \d i o pat -> seqTakeLoop d $ seqDrop i $ _fast (duration pat / d + i + o) pat

seqTakeLoop :: Time -> Sequence a -> Sequence a
seqTakeLoop 0 _ = gap 0
seqTakeLoop t pat@(Atom m d i _ v) | t > d = seqTakeLoop t $ Cat $ repeat pat
                                   | otherwise = Atom m t i (max 0 $ d - t) v
seqTakeLoop t (Stack ss) = Stack $ map (seqTakeLoop t) ss
-- TODO - raise an error?
seqTakeLoop _ (Cat []) = Cat []
seqTakeLoop t (Cat ss) = Cat $ loop t $ cycle ss
  where loop :: Time -> [Sequence a] -> [Sequence a]
        loop _ [] = [] -- can't happen
        loop t' (s:ss') | t' <= 0 = []
                        | t' <= stepDur = [seqTakeLoop t' s]
                        | otherwise = seqTakeLoop stepDur s : loop (t' - stepDur) ss'
          where stepDur = duration s

seqDrop :: Time -> Sequence a -> Sequence a
seqDrop 0 s = s
-- The mod makes this 'safe' but is probably a bad idea..
seqDrop t s | t > duration s = seqDrop' (t `mod'` duration s) s
            | otherwise = seqDrop' t s
  where seqDrop' :: Time -> Sequence a -> Sequence a
        seqDrop' t' (Atom m d i o v) | t' == d = gap 0
                                     | otherwise = Atom m (d - t') (i + t') o v
        seqDrop' t' (Stack ss) = Stack $ map (seqDrop' t') ss
        seqDrop' t' (Cat ss) = Cat $ loop t' ss
          where loop :: Time -> [Sequence a] -> [Sequence a]
                loop _ []  = []
                loop t'' (s':ss') | t'' <= 0 = []
                                  | t'' == stepDur = ss'
                                  | t'' <= stepDur = seqDrop' t'' s' : ss'
                                  | otherwise = loop (t'' - stepDur) ss'
                  where stepDur = duration s'

seqSplitAt :: Time -> Sequence a -> (Sequence a, Sequence a)
seqSplitAt t s = (seqTakeLoop t s, seqDrop t s)

withAtom :: (Metadata -> Time -> Time -> Time -> Maybe a -> Sequence a) -> Sequence a -> Sequence a
withAtom f (Atom m d i o v) = f m d i o v
withAtom f (Cat xs)         = Cat $ map (withAtom f) xs
withAtom f (Stack xs)       = Stack $ map (withAtom f) xs

withAtomTime :: (Time -> Time) -> Sequence a -> Sequence a
withAtomTime f = withAtom $ \m d i o v -> Atom m (f d) (f i) (f o) v

instance Monad Sequence where
  return = pure
  seqv >>= f = seqJoin $ fmap f seqv

instance Applicative Sequence where
  pure = step 1
  pf <*> px = pf >>= \f -> px >>= \x -> pure $ f x

instance Pattern Sequence where
  withTime f _ pat = withAtomTime f pat
  cat = Cat   -- TODO - shallow cat?
  stack = expands
  -- duration of 'part', not whole
  duration (Atom _ d _ _ _) = d
  duration (Cat xs)         = sum $ map duration xs
  duration (Stack [])       = 0
  duration (Stack (x:_))    = duration x
  timeCat seqs = seqJoin $ Cat $ map (uncurry step) seqs
  outerJoin = seqOuterJoin
  innerJoin = seqInnerJoin
  squeezeJoin = seqSqueezeJoin
  _early t = (\(a, b) -> cat [a,b]) . seqSplitAt t
  rev (Stack xs) = Stack $ map rev xs
  rev (Cat xs)   = withAtom swapio $ Cat $ reverse $ map rev xs
    where swapio m d i o v = Atom m d o i v
  rev x          = x
  -- One beat per cycle..
  toSignal pat = _slow (duration pat) $ toSignal' pat
    where
      -- One sequence per cycle
      toSignal' (Atom m d i o (Just v)) | d == 0 = error "whoops"
                                        | otherwise = addMetadata m $ _zoomSpan (Span (i/t) (1-(o/t))) $ pure v
        where t = d + i + o
      toSignal' (Atom _ _ _ _ Nothing) = silence
      toSignal' (Cat xs) = timeCat timeseqs
        where timeseqs = map (\x -> (duration x, toSignal' x)) xs
      toSignal' (Stack xs) = stack $ map toSignal' xs
  withMetadata f = withAtom $ \m d i o v -> Atom (f m) d i o v
  silence = gap 1
  _zoomSpan (Span s e) pat = _slow (d/(d * frac))
                           $ seqTakeLoop (d*frac) $ seqDrop (d*s) pat
    where d = duration pat
          frac = e-s

-- class Pattern p => Patternable p a b where toP :: a -> p b

-- **********************
-- | Sequence alignment *
-- **********************

instance Alignment Sequence where
  -- default strategy and direction
  toSeqStrategy a = SeqStrategy Expand Inner a

instance Alignment SeqStrategy where
  toSeqStrategy = id

setStrategy :: Alignment x => Strategy -> x a -> SeqStrategy a
setStrategy strat a = (toSeqStrategy a) {sStrategy = strat}

setDirection :: Alignment x => Direction -> x a -> SeqStrategy a
setDirection dir a = (toSeqStrategy a) {sDirection = dir}

justifyleft, justifyright, justifyboth, expand, truncateleft, truncateright, truncaterepeat, rep, centre, squeezein, squeezeout :: Alignment x => x a -> SeqStrategy a
justifyleft    = setStrategy JustifyLeft
justifyright   = setStrategy JustifyRight
justifyboth    = setStrategy JustifyBoth
expand         = setStrategy Expand
truncateleft   = setStrategy TruncateLeft
truncateright  = setStrategy TruncateRight
truncaterepeat = setStrategy TruncateRepeat
-- repeat is already taken by prelude
rep            = setStrategy Repeat
centre         = setStrategy Centre
squeezein      = setStrategy SqueezeIn
squeezeout     = setStrategy SqueezeOut

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
  where g = gap (t / toRational (length ss - 1))
seqSpaceOutBy t s = seqSpaceOutBy t $ Cat [s]

seqRepeatTo :: Time -> Sequence a -> Sequence a
seqRepeatTo t (Cat ss) = seqTakeLoop t $ Cat $ cycle ss
seqRepeatTo t s        = seqRepeatTo t $ Cat [s]

-- requires RankNTypes?
withSmallest :: (forall x. Sequence x -> Sequence x) -> Sequence a -> Sequence b -> (Sequence a, Sequence b)
withSmallest f a b | o == LT = (f a, b)
                   | o == GT = (a, f b)
                   | otherwise = (a, b)
  where o = compare (duration a) (duration b)

withLargest :: (forall x. Sequence x -> Sequence x) -> Sequence a -> Sequence b -> (Sequence a, Sequence b)
withLargest f a b | o == LT = (a, f b)
                  | o == GT = (f a, b)
                  | otherwise = (a, b)
  where o = compare (duration a) (duration b)

align :: Strategy -> Sequence a -> Sequence b -> (Sequence a, Sequence b)
align Repeat a b = (replic a, replic b)
  where d = lcmTime (duration a) (duration b)
        replic x = seqReplicate (floor $ d / duration x) x
        seqReplicate :: Int -> Sequence a -> Sequence a
        seqReplicate n (Cat xs) = Cat $ concat $ replicate n xs
        seqReplicate n x        = Cat $ replicate n x

align JustifyLeft a b = withSmallest (seqPadRightBy by) a b
  where by = abs $ duration a - duration b

align JustifyRight a b = withSmallest (seqPadLeftBy by) a b
  where by = abs $ duration a - duration b

align JustifyBoth a b = withSmallest (seqSpaceOutBy by) a b
  where by = abs $ duration a - duration b

align Centre a b = withSmallest (seqPadBothBy by) a b
  where by = abs $ (duration a - duration b) / 2

align Expand a b = withSmallest (_fast by) a b
  where ratio = duration a / duration b
        by | ratio < 1 = ratio
           | otherwise = 1/ratio

align TruncateLeft a b = withLargest (seqTakeLoop $ min (duration a) (duration b)) a b

-- align TruncateRight a b = withLargest (seqDrop' $ abs $ duration a - duration b) a b

align TruncateRepeat a b = withSmallest (seqRepeatTo to) a b
  where to = max (duration a) (duration b)

align SqueezeIn (Cat xs) b = (Cat xs, squeezed)
  where squeezed = Cat $ map (\x -> squash (duration x) b) xs
        squash t x = _fast (duration x / t) x
align SqueezeIn x b = align SqueezeIn (Cat [x]) b

align SqueezeOut a b = swap $ align SqueezeIn b a

align strategy _ _ = error $ show strategy ++ " not implemented for sequences."

-- **********************
-- | Sequence combining *
-- **********************

pairAligned :: Direction -> (Sequence a, Sequence b) -> Sequence (a, b)
-- TODO - vertical alignments
-- TODO - 'Mixed' direction
pairAligned Mix _ = error "TODO !!"

pairAligned direction (Stack as, b) = Stack $ map (\a -> pairAligned direction (a, b)) as
pairAligned direction (a, Stack bs) = Stack $ map (\b -> pairAligned direction (a, b)) bs

-- TODO - should the value be Nothing if one/both are nothings?
pairAligned Inner (Atom m d i o v, Atom m' _ _ _ v')
  = Atom (m <> m') d i o $ do a <- v
                              b <- v'
                              return (a,b)

pairAligned Inner (Cat xs, Cat ys) = Cat $ loop xs ys
  where loop :: [Sequence a] -> [Sequence b] -> [Sequence (a, b)]
        loop [] _ = []
        loop _ [] = []
        loop (a:as) (b:bs) = case cmp of
                               LT -> pairAligned Inner (a, b')
                                     : loop as (b'':bs)
                               GT -> pairAligned Inner (a', b)
                                     : loop (a'':as) bs
                               EQ -> pairAligned Inner (a, b)
                                     : loop as bs
          where adur = duration a
                bdur = duration b
                cmp = compare adur bdur
                (a', a'') = seqSplitAt bdur a
                (b', b'') = seqSplitAt adur b

pairAligned Inner (Cat xs, y) = loop 0 xs y
  where loop _ [] _     = gap 0
        loop _ [a] b    = pairAligned Inner (a, b)
        loop t (a:as) b = cat [pairAligned Inner (a, b'), loop t' as b'']
          where t' = t + duration a
                (b', b'') = seqSplitAt t' b

pairAligned Inner (x, Cat ys) = loop 0 x ys
  where loop :: Time -> Sequence a -> [Sequence b] -> Sequence (a,b)
        loop _ _ []     = gap 0
        loop _ a [b]    = pairAligned Inner (a, b)
        loop t a (b:bs) = cat [pairAligned Inner (a', b), loop t' a'' bs]
          where t' = t + duration b
                (a', a'') = seqSplitAt t' a

pairAligned Outer (a, b) = swap <$> pairAligned Inner (b, a)

pairAlign :: Strategy -> Direction -> Sequence a -> Sequence b -> Sequence (a, b)
pairAlign s d a b = pairAligned d $ align s a b

alignF :: Strategy -> Direction -> (a -> b -> c) -> Sequence a -> Sequence b -> Sequence c
alignF s d f a b = uncurry f <$> pairAlign s d a b

-- | Stacks

alignStack :: Strategy -> [Sequence a] -> Sequence a
alignStack strat xs = normalise $ loop xs
  where loop []      = silence
        loop [x]     = x
        loop (x:xs') = Stack [a,b]
          where (a, b) = align strat x $ alignStack strat xs'

polys :: [Sequence a] -> Sequence a
polys = alignStack Repeat

centres :: [Sequence a] -> Sequence a
centres = alignStack Centre

lefts :: [Sequence a] -> Sequence a
lefts = alignStack JustifyLeft

rights :: [Sequence a] -> Sequence a
rights = alignStack JustifyRight

truncs :: [Sequence a] -> Sequence a
truncs = alignStack TruncateRepeat

expands :: [Sequence a] -> Sequence a
expands = alignStack Expand

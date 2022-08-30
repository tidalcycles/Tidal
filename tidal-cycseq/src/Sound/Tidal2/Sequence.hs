-- (c) Aravind Mohandas, Alex McLean and contributors 2022
-- Shared under the terms of the GNU Public License v3.0

module Sound.Tidal2.Sequence where

import Sound.Tidal2.Time
import Sound.Tidal2.Value
import Sound.Tidal2.Pattern

import Data.List (inits)
import Prelude hiding (span)
import Data.Ratio

-- import Sound.Tidal.Bjorklund

data Strategy = JustifyLeft
              | JustifyRight
              | JustifyBoth
              | Expand
              | TruncateMax
              | TruncateMin
              | RepeatLCM
              | Centre
              | Squeeze
              deriving Show

data Sequence a = Atom Rational a
                | Gap Rational
                | Sequence [Sequence a]
                | Stack [Sequence a]
              deriving Show

instance Functor Sequence where
  fmap f (Atom x y) = Atom x (f y)
  fmap _ (Gap x) = Gap x
  fmap f (Sequence x) = Sequence $ map (fmap f) x
  fmap f (Stack y) = Stack (map (fmap f) y)

instance Applicative Sequence where
  pure x = Atom 1 x
  (Atom x f) <*> something =
    let (p,q) = forTwo (Atom x f) something
        c = reAlign p q
        d = unwrap $ Sequence $ map (fmap f) c
    in d
  Gap x <*> something =
    let (p,_) = forTwo (Gap x) something
    in unwrap $ Sequence  p
  Sequence f <*> something =
    let (p,q) = forTwo (Sequence f) something
        c = unwrapper $ reAlign p q
        d = unwrap $ Sequence  $ funcMap p c
    in d
  Stack f <*> something =
    let d = map (<*> something) f
    in Stack d

instance Monad Sequence where
  return = pure
  Atom _ y >>= f = f y
  Gap x >>= _ = Gap x
  Sequence x >>= f = Sequence $ map (>>= f) x
  Stack y >>= f = Stack (map (>>= f) y)

instance Pattern Sequence where
  slowcat = seqSlowcat
  fastcat = seqFastcat
  _fast   = _seqFast
  -- _early  = _seqEarly
  silence = Gap 1
  atom    = pure
  stack   = seqStack
  -- _patternify f x pat = joinInner $ (`f` pat) <$> x

-- -- | Takes sequence of functions and a sequence which have been aligned and applies the functions at the corresponding time points
funcMap :: [Sequence (a->b)] -> [Sequence a] -> [Sequence b]
funcMap [] _ = []
funcMap ((Atom x f):xs) y =
  let t = seqSpan (Atom x f)
      p = takeWhile (\r -> sum(map seqSpan r) <= t) $ inits y
      pTill = last p
      q = map (fmap f) pTill
  in q ++ funcMap xs (drop (length pTill) y)
funcMap (Gap x:xs) y=
  let t = seqSpan (Gap x)
      p = takeWhile (\r -> sum(map seqSpan r) <= t) $ inits y
      pTill = last p
      q = Gap (sum $ map seqSpan pTill)
  in q : funcMap xs (drop (length pTill) y)
funcMap (Sequence x:xs) y = funcMap (unwrapper x ++ xs) y
funcMap _ _ = error "funcMap cannot be called on stack"

-- | Mapping a function on entire sequence instead of just the values it takes
map' :: (Sequence a1 -> Sequence a2) -> Sequence a1 -> Sequence a2
map' f (Atom x y) = f (Atom x y)
map' f (Gap x) = f (Gap x)
map' f (Sequence x) = Sequence $ map (map' f) x
map' f (Stack y) = Stack (map (map' f) y)

-- | Function to map a seqeuence of a functions on another sequence
mapSeq::Sequence (Sequence a-> Sequence b) -> Sequence a -> Sequence b
mapSeq (Atom x f) something =
  let (p,q) = forTwo (Atom x f) something
      c = reAlign p q
      d = unwrap $ Sequence $ map (map' f) c
  in d
mapSeq (Gap x) something =
  let (p,_) = forTwo (Gap x) something
  in unwrap $ Sequence  p
mapSeq (Sequence f) something =
    let (p,q) = forTwo (Sequence f) something
        c = unwrapper $ reAlign p q
        d = unwrap $ Sequence  $ funcSeq p c
    in d
mapSeq (Stack f) something =
    let d = map (`mapSeq` something) f
    in Stack d

-- | Takes sequence of functions on sequences and a sequence which have been aligned and applies the functions at 
-- the corresponding time points
funcSeq :: [Sequence (Sequence a1 -> Sequence a2)]-> [Sequence a1] -> [Sequence a2]
funcSeq [] _ = []
funcSeq ((Atom x f):xs) y =
  let t = seqSpan (Atom x f)
      p = takeWhile (\r -> sum(map seqSpan r) <= t) $ inits y
      pTill = last p
      q = map (map' f) pTill
  in q ++ funcSeq xs (drop (length pTill) y)
funcSeq (Gap x:xs) y=
  let t = seqSpan (Gap x)
      p = takeWhile (\r -> sum(map seqSpan r) <= t) $ inits y
      pTill = last p
      q = Gap (sum $ map seqSpan pTill)
  in q : funcSeq xs (drop (length pTill) y)
funcSeq (Sequence x:xs) y = funcSeq (unwrapper x ++ xs) y
funcSeq _ _ = error "funcSeq cannot be called on Stack"

-- | Takes two sequences, which have been obtained using stratApply, or forTwo, and then splits the second sequence with respect
-- to the break points of the first
reAlign:: [Sequence a] -> [Sequence b] -> [Sequence b]
reAlign [] _ = []
reAlign ((Gap x):xs) y =
  let t = seqSpan (Gap x)
      p  = takeWhile (\q -> sum (map seqSpan q) <=t ) $ inits y
      pTill = last p
      l = length pTill
      tTill = sum (map seqSpan pTill)
      (a,b) = if tTill == t then (Gap 0, drop l y)  else
        let (m,n) =  getPartition (y!!l) (t - tTill)
        in (m, n :drop (l+1) y)
  in if tTill == t then pTill ++ reAlign xs b else (pTill ++ [a]) ++ reAlign xs b
reAlign ((Atom x s):xs) y =
  let t = seqSpan (Atom x s)
      p  = takeWhile (\q -> sum (map seqSpan q) <=t ) $ inits y
      pTill = last p
      l = length pTill
      tTill = sum (map seqSpan pTill)
      (a,b) = if tTill == t then (Gap 0, drop l y)  else
        let (m,n) =  getPartition (y!!l) (t - tTill)
        in (m, n :drop (l+1) y)
  in (pTill ++ [a]) ++ reAlign xs b
reAlign (Sequence x:xs) (Sequence y:ys) = reAlign (unwrapper x ++ xs) (unwrapper y ++ ys)
reAlign _ _ = error "reAlign not defined"


-- | Function to partition an event in a sequence
getPartition::Sequence a-> Rational -> (Sequence a, Sequence a)
getPartition (Atom x s) t = (Atom t s, Atom (x-t) s)
getPartition (Gap x) t = (Gap t, Gap (x - t))
getPartition (Sequence y) t =
  let p = takeWhile (\q -> sum (map seqSpan q) <=t ) $ inits y
      pTill = last p
      l = length pTill
      tTill = sum (map seqSpan pTill)
      (a,b) = if tTill == t then (Gap 0, drop l y)  else
        let (m,n) =  getPartition (y!!l) (t - tTill)
        in (m, n :drop (l+1) y)
  in (Sequence $ pTill ++ [a], Sequence b)
getPartition (Stack s) t = let a = map (`getPartition` t) s in (Stack (map fst a),Stack (map snd a))

-- | Given two sequences of different types, this function uses the LCM method to align those two sequences
forTwo :: Sequence a1 -> Sequence a2 -> ([Sequence a1], [Sequence a2])
forTwo a b =
  let p = lcmRational (seqSpan a) (seqSpan b)
  in (unwrapper $ replicate (fromIntegral  $ numerator $ p/seqSpan a) a, unwrapper $ replicate (fromIntegral $ numerator $ p/seqSpan b) b)

unwrap::Sequence a -> Sequence a
unwrap (Sequence x) = Sequence $ unwrapper x
unwrap (Stack x) = Stack $ map unwrap x
unwrap s = s

-- | Unwrapping a sequence referes to removing the redundancies that are present in the code
unwrapper:: [Sequence a] -> [Sequence a]
unwrapper [] = []
unwrapper [Sequence x] = unwrapper x
unwrapper (Sequence x :xs) = unwrapper x ++ unwrapper xs
unwrapper (Atom x s:xs) = Atom x s : unwrapper xs
unwrapper (Gap x:xs) = if x ==0 then unwrapper xs else Gap x: unwrapper xs
unwrapper (Stack x:xs) = Stack x : unwrapper xs

rev :: Sequence a -> Sequence a
rev (Sequence bs) = Sequence $ reverse $ map rev bs
rev (Stack bs) = Stack $ map rev bs
rev b = b

cat :: [Sequence a] -> Sequence a
cat [] = Gap 0
cat [b] = b
cat bs = Sequence bs

ply :: Sequence Rational -> Sequence a -> Sequence a
ply sr s =  mapSeq (applyfToSeq _ply sr) s

_ply :: Rational -> Sequence a -> Sequence a
_ply n (Atom d v) = Sequence $ reduce $  ((replicate (floor n) $ Atom (d / toRational n) v) ++ [Atom (d - ((floor n)%1) *d /n ) v])
_ply _ (Gap x) = Gap x
_ply n (Sequence x) = unwrap $ Sequence (map (_ply n) x)
_ply n (Stack x) =  Stack (map (_ply n) x)

seqSpan :: Sequence a -> Rational
seqSpan (Atom s _) = s
seqSpan (Gap s) = s
seqSpan (Sequence bs) = sum $ map seqSpan bs
seqSpan (Stack []) = 0
seqSpan (Stack x) = seqSpan $ head x

lcmRational :: Rational->Rational-> Rational
lcmRational a b = lcm (f a) (f b) % d
  where d = lcm (denominator a) (denominator b)
        f x = numerator x * (d `div` denominator x)

-- | stratApply takes a list of sequences, a strategy, and then aligns all those sequences according to these strategies
stratApply::Strategy -> [Sequence a] ->Sequence a
stratApply _ [] = Gap 0

stratApply JustifyLeft bs =
  let a = maximum $ map seqSpan bs
      b = map (\x -> Sequence (x: [Gap (a - seqSpan x)])) bs
  in Stack b

stratApply JustifyRight bs =
  let a = maximum $ map seqSpan bs
      b = map (\x -> Sequence (Gap (a - seqSpan x) : [x])) bs
  in Stack b

stratApply Centre bs =
  let a = maximum $ map seqSpan bs
      b = map( \x -> Sequence ([Gap ((a - seqSpan x)/2)] ++ [x] ++ [Gap ((a - seqSpan x)/2)])) bs
  in Stack b

stratApply RepeatLCM bs@(x:xs) =
  let a = foldr (lcmRational . seqSpan) (seqSpan x) xs
      b = map (\r ->  unwrap $ Sequence $  replicate (fromIntegral $ numerator $ a/seqSpan r) x) bs
  in Stack b

stratApply Expand bs =
  let a = maximum $ map seqSpan bs
      b = map (\x -> expand x $ a/seqSpan x) bs
  in Stack b

stratApply Squeeze bs =
  let a = minimum $ map seqSpan bs
      b = map (\x -> expand x $ a/seqSpan x) bs
  in Stack b

stratApply JustifyBoth bs =
  let a = maximum $ map seqSpan bs
      b = map (`expand'` a) bs
  in Stack b

stratApply TruncateMin bs =
  let a = minimum $ map seqSpan bs
      b = map (`cutShort` a) bs
  in Stack b

stratApply TruncateMax bs =
  let a = maximum $ map seqSpan bs
      b = map (\x -> unwrap $ Sequence $ replicate (floor $ a/seqSpan x) x ++ let Sequence p = cutShort x (realToFrac (a - floor (a/seqSpan x)%1 * seqSpan x)) in p) bs
  in Stack b

-- Return a segment of a sequence
cutShort::Sequence a->Rational->Sequence a
cutShort (Atom x s) r = if x>r then Atom r s else Atom x s
cutShort (Gap x) r = if x > r then Gap r else Gap x
cutShort (Sequence x) r =
  let p = takeWhile (\q -> sum (map seqSpan q) <=r ) $ inits x
      pTill = last p
      l = length pTill
      tTill = sum (map seqSpan pTill)
  in if tTill == r then Sequence pTill else Sequence $ pTill ++ [fst (getPartition (x!!l) (r - tTill))]
cutShort (Stack x) r = Stack $ map (`cutShort` r) x

-- | Expand a sequence to a particular length, while not modifying the length of that sequence
expand'::Sequence a -> Rational -> Sequence a
expand' (Atom x s) r = expand (Atom x s) $ r/seqSpan (Atom x s)
expand' (Gap x) r = expand (Gap x) $ r/seqSpan (Gap x)
expand' (Sequence [x]) r = Sequence [expand x (r/seqSpan x)]
expand' (Sequence x) r =
  let Sequence y = (expand (Sequence $ init x) $ (r- seqSpan (last x))/ seqSpan (Sequence $ init x))
  in Sequence (y++ [last x])
expand' (Stack x) r = Stack $ map (`expand'` r) x

-- | Expand a sequence to a particular length
expand::Sequence a-> Rational -> Sequence a
expand (Atom x s) r = Atom (x*r) s
expand (Gap x) r = Gap (x*r)
expand (Sequence x) r = Sequence $ map (`expand` r) x
expand (Stack x) r = Stack $ map (`expand` r) x


-- | Reduce a list of sequences by removing redundancies
reduce::[Sequence a] -> [Sequence a]
reduce (Atom 0 _:xs) = reduce xs
reduce (Gap x1:Gap x2:xs) =reduce $ Gap (x1+x2):xs
reduce (Sequence x:xs) = Sequence (reduce x):reduce xs
reduce (Stack x:xs) = Stack (reduce x):reduce xs
reduce (x:xs) = x:reduce xs
reduce [] = []

-- | Applies a function to within a sequence
-- TODO: Is this fmap?
applyfToSeq :: (t -> a) -> Sequence t -> Sequence a
applyfToSeq f (Atom x s) = Atom x (f s)
applyfToSeq _ (Gap x) = Gap x
applyfToSeq f (Sequence x) = Sequence $ map (applyfToSeq f) x
applyfToSeq f (Stack x) = Stack $ map (applyfToSeq f) x

-- | Speed up the sequence
fast :: Sequence Rational -> Sequence a -> Sequence a
fast sr s = mapSeq (applyfToSeq _fast sr) s

_seqFast :: Rational -> Sequence a -> Sequence a
_seqFast n (Atom x s) = Atom (x/n) s
_seqFast n (Gap x) = Gap (x/n)
_seqFast n (Sequence s) = Sequence $ map (_fast n) s
_seqFast n (Stack x) = Stack $ map(_fast n) x

-- | Slow down the sequence
-- slow :: Sequence Rational->Sequence a->Sequence a
-- slow sr s = mapSeq (applyfToSeq _slow sr) s

-- | Repeat the sequence a desired number of times without changing duration
rep::Int -> Sequence a-> Sequence a
rep n (Atom x s) = Atom (realToFrac n * x) s
rep n (Gap x) = Gap (realToFrac  n*x)
rep n (Sequence s) = Sequence $ reduce $  concat $ replicate n s
rep n (Stack s) = Stack $ map (rep n) s

-- | Repeat sequence desired number of times, and squeeze it into the duration of the original sequence
repSqueeze::Int -> Sequence a-> Sequence a
repSqueeze n s = _fast (realToFrac n) $ rep n s

-- | Takes a list of sequences, and if aligned returns a stack. Otherwise applies default method and returns
seqStack :: [Sequence a] -> Sequence a
seqStack s =
  let a = foldl (\acc x->if seqSpan x==acc then acc else -1) (seqSpan $ head s) s
  in if a == (-1) then stratApply Expand s else Stack s

-- euclid :: Sequence Int -> Sequence Int -> Sequence b -> Sequence b
-- euclid s1 s2 sa =
--   let b = fmap _euclid s1
--       c = b <*> s2
--       d = mapSeq c sa
--   in d

-- -- | Obtain a euclidean pattern 
-- _euclid:: Int-> Int-> Sequence a -> Sequence a
-- _euclid a b s  =
--   let x = bjorklund (a, b)
--       y = map (\t -> if t then s else Gap (seqSpan s)) x
--   in unwrap $ Sequence y

every :: Sequence Int -> (Sequence b -> Sequence b) -> Sequence b -> Sequence b
every si f sa = mapSeq (applyfToSeq (every' f) si) sa

every' :: (Sequence a -> Sequence a) -> Int -> Sequence a -> Sequence a
every' f s = _every s f

_every :: Int -> (Sequence a -> Sequence a) -> Sequence a -> Sequence a
_every n f s = unwrap $ Sequence $ replicate (n-1) s ++ [f s]



-- makeSeq:: [Syllable] -> Rational -> Sequence Syllable
-- makeSeq x r =
--   let a = lcmRational (realToFrac $ length x) r

--       b = concat $ replicate (div (fromIntegral $ numerator a) (length x)) x
--       c = map (\t -> if t == Gdot then Gap (1/r) else Atom (1/r) t) b
--   in unwrap $ Sequence c

fromList:: [a] -> Sequence a
fromList x=  unwrap $ Sequence $ reduce $  map (Atom 1) x

fastFromList ::[a] -> Sequence a
fastFromList x = unwrap $ Sequence $ reduce $ map (Atom (realToFrac $ (1%length x))) x

listToPat ::[a] -> Sequence a
listToPat = fastFromList

seqFastcat ::  [Sequence a]  -> Sequence a
seqFastcat x = _fast (sum $ map seqSpan x) $ Sequence (reduce x)

fromMaybes :: [Maybe a] -> Sequence a
fromMaybes = fastcat . map f
  where f Nothing = Gap 1
        f (Just x) = Atom 1 x

run :: (Enum a, Num a) => Sequence a -> Sequence a
run x = unwrap $ (>>= _run) x

_run :: (Enum a, Num a) => a -> Sequence a
_run n = unwrap $  fastFromList [0 .. n-1]

seqSlowcat :: [Sequence a] -> Sequence a
seqSlowcat [] = Gap 0
seqSlowcat [b] = b
seqSlowcat bs = Sequence bs

-- | From @1@ for the first cycle, successively adds a number until it gets up to @n@
scan :: (Enum a, Num a) => Sequence a -> Sequence a
scan x = unwrap $  (>>= _scan) x

_scan :: (Enum a, Num a) => a -> Sequence a
_scan n = unwrap $ slowcat $ map _run [1 .. n]

-- | Alias for 'append'
slowAppend :: Sequence a -> Sequence a -> Sequence a
slowAppend = append
slowappend :: Sequence a -> Sequence a -> Sequence a
slowappend = append

-- | Like 'append', but twice as fast
fastAppend :: Sequence a -> Sequence a -> Sequence a
fastAppend a b = _fast 2 $ append a b
fastappend :: Sequence a -> Sequence a -> Sequence a
fastappend = fastAppend

timeCat :: [(Rational, Sequence a)] -> Sequence a
timeCat x = cat $ map (\t -> _fast (seqSpan (snd t)/fst t) (snd t)) x

-- | Alias for @timeCat@
timecat :: [(Rational, Sequence a)] -> Sequence a
timecat = timeCat

-- | An alias for @fast@
density :: Sequence Rational -> Sequence a -> Sequence a
density = fast

rotL :: Rational -> Sequence a -> Sequence a
rotL t (Sequence x) = splitUp t x
rotL t (Stack x) = Stack $ map (t `rotL`) x
rotL _ x = x

rotR :: Rational -> Sequence a -> Sequence a
rotR t (Sequence x) = splitUp (seqSpan (Sequence x) - t) x
rotR t (Stack x) = Stack $ map (t `rotR`) x
rotR _ x = x

splitUp :: Rational -> [Sequence a] -> Sequence a
splitUp t x =
  let a = takeWhile (\m -> sum (map seqSpan m) <=t ) (inits x)
      pTill =if null a then [] else  last a
      l = length pTill
      tTill = sum (map seqSpan pTill)
      (p,q) = if tTill == t then (pTill, drop l x)  else
        let (m,n) =  getPartition (x!!l) (t - tTill)
        in (pTill ++ [m], n : drop (l+1) x)
  in Sequence $ q++p

-- -- | Shifts a sequence back in time by the given amount, expressed in cycles
-- (<~) :: Sequence Rational -> Sequence a -> Sequence a
-- (<~) sr s= mapSeq (applyfToSeq rotL sr) s

-- -- | Shifts a pattern forward in time by the given amount, expressed in cycles
-- (~>) :: Sequence Rational -> Sequence a -> Sequence a
-- (~>) sr s = mapSeq (applyfToSeq rotR sr) s

iter :: Sequence Int -> Sequence a ->  Sequence a
iter sr s = 
  let (a,b) = forTwo sr s
  in unwrap $  iterer (Sequence a) (Sequence b) 

iterer :: Sequence Int -> Sequence a -> Sequence a
iterer (Atom _ s) sr = _iter s sr
iterer (Gap x) _ = Gap x
iterer (Sequence x) sr = Sequence $ map (\t -> iterer t sr) x
iterer (Stack x) sr = stack $ map (\t -> iterer t sr) x

_iter :: Int -> Sequence a -> Sequence a
_iter n p = slowcat $ map (\i -> ((fromIntegral i) % (fromIntegral n)) `rotL` p) [0 .. (n-1)] 

-- | @iter'@ is the same as @iter@, but decrements the starting
-- subdivision instead of incrementing it.
iter' :: Sequence Int -> Sequence c ->Sequence c
iter' sr s= 
  let (a,b) = forTwo sr s
  in unwrap $  iterer' (Sequence a) (Sequence b) 

iterer' :: Sequence Int -> Sequence a -> Sequence a
iterer' (Atom _ s) sr = _iter' s sr
iterer' (Gap x) _ = Gap x
iterer' (Sequence x) sr = Sequence $ map (\t -> iterer' t sr) x
iterer' (Stack x) sr = stack $ map (\t -> iterer' t sr) x


_iter' :: Int -> Sequence a -> Sequence a
_iter' n p = slowcat $ map (\i -> (fromIntegral i % fromIntegral n) `rotR` p) [0 .. (n-1)]

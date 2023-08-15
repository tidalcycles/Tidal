{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Sound.Tidal.Pattern where

import           Data.Ratio
import           Prelude           hiding ((*>), (<*))
import           Sound.Tidal.Types

-- Patternification

-- Turns functions with non-patterned parameters into fully patternified ones

-- patternify the first of two parameters
patternify_p_n :: Pattern p => (a -> b -> p c) -> (p a -> b -> p c)
patternify_p_n f apat pat = apat `innerBind` \a -> f a pat

-- patternify two parameters
patternify_p_p :: (Pattern p) => (a -> b -> p c) -> (p a -> p b -> p c)
patternify_p_p f apat bpat = apat `innerBind` \a -> bpat `innerBind` \b -> f a b

-- patternify the first two of three parameters
patternify_p_p_n :: (Pattern p) => (a -> b -> c -> p d) -> p a -> p b -> c -> p d
patternify_p_p_n f apat bpat pat = apat `innerBind` \a -> bpat `innerBind` \b -> f a b pat

-- patternify three parameters
patternify_p_p_p :: (Pattern p) => (a -> b -> c -> p d) -> (p a -> p b -> p c -> p d)
patternify_p_p_p f apat bpat cpat = apat `innerBind` \a -> bpat `innerBind` \b -> cpat `innerBind` \c -> f a b c

-- and so on
patternify_p_n_n :: Pattern p => (a -> b -> c -> p d) -> p a -> b -> c -> p d
patternify_p_n_n f apat b pat = apat `innerBind` \a -> f a b pat

patternify_p_p_p_n :: Pattern p => (a -> b -> c -> d -> p e) -> p a -> p b -> p c -> d -> p e
patternify_p_p_p_n f apat bpat cpat pat = apat `innerBind` \a -> (bpat `innerBind` \b -> (cpat `innerBind` \c -> f a b c pat))

(<*), (*>) :: Pattern p => p (t -> b) -> p t -> p b
pf <* px = pf `innerBind` \f -> px `innerBind` \x -> pure $ f x
pf *> px = pf `outerBind` \f -> px `outerBind` \x -> pure $ f x
infixl 4 <*, *>

-- ************************************************************ --
-- Transformations common to Signals and Sequences

slowcat, fastcat :: Pattern p => [p a] -> p a
slowcat = cat
fastcat xs = _fast (toRational $ length xs) $ cat xs

append, fastAppend, slowAppend :: Pattern p => p a -> p a -> p a
append a b = cat [a,b]
fastAppend a b = fastcat [a,b]
slowAppend = append

overlay :: Pattern p => p a -> p a -> p a
overlay a b =  cat [a,b]

_fast, _slow, _late :: Pattern p => Time -> p a -> p a
_fast t = withTime (/ t) (* t)
_slow t = withTime (* t) (/ t)
_late = _early . (0-)

-- patternify parameters
fast, density, slow, sparsity, early, late, (<~), (~>) :: Pattern p => p Time -> p a -> p a
fast  = patternify_p_n _fast
density = fast
slow  = patternify_p_n _slow
sparsity = slow
early = patternify_p_n _early
late  = patternify_p_n _late
-- Infix operator for `early`
(<~) = early
-- Infix operator for `late`
(~>) = late

_inside, _outside :: Pattern p => Time -> (p a -> p b) -> p a -> p b
_inside n f p = _fast n $ f (_slow n p)
_outside n = _inside (1/n)

inside, outside :: Pattern p => p Time -> (p a -> p b) -> p a -> p b
inside = patternify_p_n_n _inside
outside = patternify_p_n_n _outside

_superimpose :: Pattern p => (p x -> p x) -> p x -> p x
_superimpose f pat = cat [pat, f pat]

superimpose :: Pattern p => p (p x -> p x) -> p x -> p x
superimpose = patternify_p_n _superimpose

xsuperimpose :: forall x p a. (Pattern p, Applicable p a (p x -> p x)) => a -> p x -> p x
xsuperimpose pf pat = superimpose (toA pf) pat

_off :: Pattern p => Time -> (p a -> p a) -> p a -> p a
_off t f p = _superimpose (f . (t `_late`)) p

off :: Pattern p => p Time -> p (p a -> p a) -> p a -> p a
off  = patternify_p_p_n _off


when :: Pattern p => p Bool -> (p b -> p b) -> p b -> p b
when boolpat f pat = innerJoin $ (\b -> if b then f pat else pat) <$> boolpat

_firstOf, _lastOf, _every :: Pattern p => Int -> (p a -> p a) -> p a -> p a
_firstOf n f pat | n <= 0 = silence
                 | otherwise = when (fromList
                                     (True : replicate (n - 1) False)
                                    ) f pat
_lastOf n f pat | n <= 0 = silence
                | otherwise = when (fromList
                                    (replicate (n - 1) False ++ [True])
                                   ) f pat
_every = _lastOf

firstOf, lastOf, every :: Pattern p => p Int -> (p a -> p a) -> p a -> p a
firstOf = patternify_p_n_n _firstOf
lastOf = patternify_p_n_n _lastOf
every = lastOf

-- | @foldEvery ns f p@ applies the function @f@ to @p@, and is
-- applied for each cycle in @ns@.
foldEvery :: Pattern p => [Int] -> (p a -> p a) -> p a -> p a
foldEvery ns f p = foldr (`_every` f) p ns

_iter, _iterBack :: Pattern p => Int -> p a -> p a
_iter n p | n == 0 = p
          | n == 1 = p
          | n < 0 = _iterBack (abs n) p
          | otherwise = slowcat $ p:map (\t -> _early ((fromIntegral t)%(fromIntegral n)) p) [1 .. n]

_iterBack n p | n == 0 = p
              | n == 1 = p
              | n < 0 = _iter (abs n) p
              | otherwise = slowcat $ p:map (\t -> _early ((fromIntegral t)%(fromIntegral n)) p) [n .. 1]

iter, iterBack :: Pattern p => p Int -> p a -> p a
iter = patternify_p_n _iter
iterBack = patternify_p_n _iterBack

-- | @palindrome p@ applies @rev@ to @p@ every other cycle, so that
-- the pattern alternates between forwards and backwards.
palindrome :: Pattern p => p a -> p a
palindrome p = slowcat [p, rev p]

-- | Repeats each event @n@ times within its arc
ply :: Pattern p => p Int -> p a -> p a
ply = patternify_p_n _ply

_ply :: Pattern p => Int -> p a -> p a
_ply n pat = squeezeJoin $ fastcat . replicate n . pure <$> pat

-- | Syncopates a rhythm, shifting each event halfway into its arc (aka timespan), e.g. @"a b [c d] e"@ becomes the equivalent of @"[~ a] [~ b] [[~ c] [~ d]] [~ e]"@
press :: Pattern p => p a -> p a
press = _pressBy 0.5

pressBy :: Pattern p => p Time -> p a -> p a
-- | Like @press@, but allows you to specify the amount in which each event is shifted. @pressBy 0.5@ is the same as @press@, while @pressBy (1/3)@ shifts each event by a third of its duration.
pressBy = patternify_p_n _pressBy

_pressBy :: Pattern p => Time -> p a -> p a
_pressBy t pat = squeezeJoin $ (\v -> timeCat [(t, silence), (1-t, pure v)]) <$> pat

-- | chooses between a list of patterns, using a pattern of floats (from 0-1)
select :: Pattern p => p Double -> [p a] -> p a
select = patternify_p_n _select

_select :: Pattern p => Double -> [p a] -> p a
_select f ps =  ps !! floor (max 0 (min 1 f) * fromIntegral (length ps - 1))

-- | chooses between a list of functions, using a pattern of floats (from 0-1)
selectF :: Pattern p => p Double -> [p a -> p a] -> p a -> p a
selectF pf ps p = innerJoin $ (\f -> _selectF f ps p) <$> pf

_selectF :: Double -> [p a -> p a] -> p a -> p a
_selectF f ps p =  (ps !! floor (max 0 (min 0.999999 f) * fromIntegral (length ps))) p

_repeatCycles, _fastRepeatCycles, _slowRepeatCycles
  :: Pattern p => Int -> p a -> p a
_repeatCycles n p = slowcat $ replicate n p
_fastRepeatCycles n p = fastcat $ replicate n p
_slowRepeatCycles = _repeatCycles

repeatCycles, fastRepeatCycles, slowRepeatCycles
  :: Pattern p => p Int -> p a -> p a
repeatCycles = patternify_p_n _repeatCycles
fastRepeatCycles = patternify_p_n _fastRepeatCycles
slowRepeatCycles = repeatCycles

-- ************************************************************ --
-- Numerical manipulations

{- | `range` will take a pattern which goes from 0 to 1 (like `sine`),
   and scale it to a different range - between the first and second
   arguments. In the below example, `range 1 1.5` shifts the range of
   `sine1` from 0 - 1 to 1 - 1.5.

@
d1 $ jux (iter 4) $ sound "arpy arpy:2*2"
  |+ speed (slow 4 $ range 1 1.5 sine1)
@
-}
range :: (Pattern p, Num a) => p a -> p a -> p a -> p a
range = patternify_p_p_n _range

_range :: (Functor f, Num b) => b -> b -> f b -> f b
_range from to pat = (+ from) . (* (to-from)) <$> pat

{- | `range2` does the same as `range`, but assumes a starting range
   from -1 to 1 rather than from 0 to 1.
-}
range2 :: (Pattern p, Fractional a) => p a -> p a -> p a -> p a
range2 from to pat = range from to $ fromBipolar pat

-- | Converts from a range from 0 to 1, to a range from -1 to 1
toBipolar :: (Pattern p, Fractional x) => p x -> p x
toBipolar pat = fmap (\v -> (v*2)-1) pat

-- | Converts from a range from -1 to 1, to a range from 0 to 1
fromBipolar :: (Pattern p, Fractional x) => p x -> p x
fromBipolar pat = fmap (\v -> (v+1)/2) pat

-- | limit values in a Pattern (or other Functor) to n equally spaced
-- divisions of 1.
quantise :: (Functor f, RealFrac b) => b -> f b -> f b
quantise n = fmap ((/n) . (fromIntegral :: RealFrac b => Int -> b) . round . (*n))

-- quantise but with floor
qfloor :: (Functor f, RealFrac b) => b -> f b -> f b
qfloor n = fmap ((/n) . (fromIntegral :: RealFrac b => Int -> b) . floor . (*n))

qceiling :: (Functor f, RealFrac b) => b -> f b -> f b
qceiling n = fmap ((/n) . (fromIntegral :: RealFrac b => Int -> b) . ceiling . (*n))

qround :: (Functor f, RealFrac b) => b -> f b -> f b
qround = quantise

-- ************************************************************ --
-- Simple pattern generators

-- | Turns a list of values into a pattern
fromList, fastFromList, slowFromList :: Pattern p => [a] -> p a
fromList = slowcat . map pure
slowFromList = fromList
fastFromList = fastcat . map pure

-- | 'fromMaybes; is similar to 'fromList', but allows values to
-- be optional using the 'Maybe' type, so that 'Nothing' results in
-- gaps in the pattern.
fromMaybes :: Pattern t => [Maybe a] -> t a
fromMaybes = fastcat . map f
  where f Nothing  = silence
        f (Just x) = pure x

_run :: (Pattern p, Enum a, Num a) => a -> p a
_run n = fastFromList [0 .. n-1]

-- | A pattern of whole numbers from 0 up to (and not including) the
-- given number, in a single cycle.
run :: (Pattern p, Enum a, Num a) => p a -> p a
run = (>>= _run)

_scan :: (Pattern p, Enum a, Num a) => a -> p a
_scan n = slowcat $ map _run [1 .. n]

-- | From @1@ for the first cycle, successively adds a number until it
-- gets up to @n@
scan :: (Pattern p, Enum a, Num a) => p a -> p a
scan = (>>= _run)

-- ************************************************************ --
-- Metadata utils

addMetadata :: Pattern p => Metadata -> p a -> p a
addMetadata m = withMetadata (m <>)

setMetadata :: Pattern p => Metadata -> p a -> p a
setMetadata m = withMetadata (const m)

withSrcPos :: Pattern p => ([((Int, Int), (Int, Int))] -> [((Int, Int), (Int, Int))]) -> p a -> p a
withSrcPos f = withMetadata (\m -> m {metaSrcPos = f $ metaSrcPos m})

addSrcPos :: Pattern p => [((Int, Int), (Int, Int))] -> p a -> p a
addSrcPos xs = withSrcPos (++ xs)

stripMetadata :: Pattern p => p a -> p a
stripMetadata = withMetadata $ const mempty

patDeltaMetadata :: Pattern p => Int -> Int -> p a -> p a
patDeltaMetadata column line pat
    = withSrcPos (map (\((bx,by), (ex,ey)) ->
                         ((bx+column,by+line), (ex+column,ey+line)))) pat

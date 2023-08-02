{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Sound.Tidal.Pattern where

import           Prelude           hiding ((*>), (<*))
import           Sound.Tidal.Types

silence :: (Monoid (p a), Pattern p) => p a
silence = mempty

innerJoin, outerJoin :: Pattern p => p (p b) -> p b
innerJoin s = innerBind s id
outerJoin s = outerBind s id

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

_fast, _slow, _late :: Pattern p => Time -> p a -> p a
_fast t = withTime (/ t) (* t)
_slow t = withTime (* t) (/ t)
_late = _early . (0-)

-- patternify parameters
fast, slow, early, late :: Pattern p => p Time -> p a -> p a
fast  = patternify_p_n _fast
slow  = patternify_p_n _slow
early = patternify_p_n _early
late  = patternify_p_n _late

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

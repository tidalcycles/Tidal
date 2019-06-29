{-# LANGUAGE FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}

module Sound.Tidal.MiniTidal (miniTidal) where

import           Language.Haskell.Exts
import           Control.Monad (forever)
import           Control.Applicative (liftA2,(<|>))
import           Data.Bifunctor

import           Sound.Tidal.Context (Pattern,ControlMap,ControlPattern,Enumerable,Parseable,Time,Arc,TPat,Stream)
import qualified Sound.Tidal.Context as T

-- This is depended upon by Estuary, and changes to its type will cause problems downstream for Estuary.
miniTidal :: String -> Either String ControlPattern
miniTidal = f . parseExp
  where
    f (ParseOk x) = value x
    f (ParseFailed l "Parse error: EOF") = Right $ T.silence
    f (ParseFailed l s) = Left $ show l ++ ": " ++ show s

-- Throughout this module, we leverage the fact that the type Either a is an instance of MonadPlus/Alternative
-- The type synonym Result that many of our definitions return may be thought of as a reminder that we are
-- operating in that monadic context (as well simplify later changes to that context).

type Result = Either String

-- The class MiniTidal is a class for all of the types that we know how to parse.
-- For each type that is an instance of the class we define functions that go from
-- the Haskell abstract syntax trees (ASTs) provided by the haskell-src-exts library to
-- types in the TidalCycles universe (wrapped in the aforementioned monadic Result context).

class MiniTidal a where
  valueByApplication :: Exp SrcSpanInfo -> Exp SrcSpanInfo -> Result a
  valueByApplication _ _ = Left "expected something else"
  valueByOperator :: Exp SrcSpanInfo -> String -> Exp SrcSpanInfo -> Result a
  valueByOperator _ _ _ = Left "expected something else"
  valueByIdentifier :: String -> Result a
  valueByIdentifier _ = Left "expected something else"
  valueByStringLiteral :: String -> Result a
  valueByStringLiteral _ = Left "expected something else"
  valueByIntegerLiteral :: Integer -> Result a
  valueByIntegerLiteral _ = Left "expected something else"
  valueByRationalLiteral :: Rational -> Result a
  valueByRationalLiteral _ = Left "expected something else"
  valueByList :: [Exp SrcSpanInfo] -> Result a
  valueByList _ = Left "expected something else"
  valueByTuple :: [Exp SrcSpanInfo] -> Result a
  valueByTuple _ = Left "expected something else"

-- The function value provides a standard method of parsing from Haskell ASTs to any type
-- that is an instance of the MiniTidal class. We will be able to use this in, for example,
-- applicative expressions to provide arguments of particular types.

value :: MiniTidal a => Exp SrcSpanInfo -> Result a
value (Paren _ exp) = value exp
value (InfixApp _ exp1 (QVarOp _ (UnQual _ (Symbol _ "$"))) exp2) = valueByApplication exp1 exp2
value (InfixApp _ exp1 (QVarOp _ (UnQual _ (Symbol _ op))) exp2) = valueByOperator exp1 op exp2
value (App _ exp1 exp2) = valueByApplication exp1 exp2
value (Var _ (UnQual _ (Ident _ x))) = valueByIdentifier x
value (Lit _ (String _ x _)) = valueByStringLiteral x
value (NegApp _ (Lit _ (Int _ x _))) = valueByIntegerLiteral (x * (-1))
value (NegApp _ (Lit _ (Frac _ x _))) = valueByRationalLiteral (x * (-1))
value (Lit _ (Int _ x _)) = valueByIntegerLiteral x
value (Lit _ (Frac _ x _)) = valueByRationalLiteral x
value (List _ xs) = valueByList xs
value (Tuple _ Boxed xs) = valueByTuple xs
value _ = Left "unhandled structure in Sound.Tidal.MiniTidal.value"


instance MiniTidal Int where
  valueByIntegerLiteral = Right . fromIntegral

instance MiniTidal Time where
  valueByRationalLiteral = Right

instance MiniTidal Double where
  valueByIntegerLiteral = Right . fromIntegral
  valueByRationalLiteral = Right . realToFrac

instance {-# INCOHERENT #-} MiniTidal String where -- don't really think incoherent is necessary here but compiler claims about overlap with MiniTidal [a] instance...
  valueByStringLiteral = Right

instance MiniTidal a => MiniTidal (a,a) where
  valueByTuple (a:b:[]) = (\x y -> (x,y)) <$> value a <*> value b
  valueByTuple _ = Left "unable to parse as (,)"

instance MiniTidal a => MiniTidal [a] where
  valueByList = mapM value

list :: MiniTidal a => Exp SrcSpanInfo -> Result [a]
list (Paren _ exp) = list exp
list (List _ xs) = mapM value xs
list _ = Left "unhandled structure in Sound.Tidal.MiniTidal.list"

instance MiniTidal ControlMap -- empty instance for the moment...

-- Next, we provide MiniTidal instance declarations for as many as possible of the types
-- that occur in typical TidalCycles usage patterns, beginning with ControlPattern, which
-- represents a patterning of different types of parameters to the Dirt/SuperDirt/WebDirt/etc
-- sample engines, and is thus close to the top of TidalCycles' type hierarchy. Note how
-- the use of value to parse expression components sometimes requires type annotations given
-- the extreme polymorphism of this function (ie. how many instances of MiniTidal we have).

instance MiniTidal ControlPattern where
  valueByApplication e1 e2 =
    (value e1 :: Result (ControlPattern -> ControlPattern)) <*> value e2 <|>
    (value e1 :: Result (Pattern String -> ControlPattern)) <*> value e2 <|>
    (value e1 :: Result (Pattern Double -> ControlPattern)) <*> value e2 <|>
    (value e1 :: Result (Pattern Int -> ControlPattern)) <*> value e2 <|>
    genericPatternExpressions e1 e2
  valueByOperator e1 op e2 =
    (unionableMergeOperator op <|>
     numMergeOperator op <|>
     fractionalMergeOperator op <|>
     Left "ControlPattern merge operator expected")
    <*> value e1 <*> value e2
  valueByIdentifier "silence" = Right $ T.silence


instance MiniTidal (Pattern String) where
  valueByApplication e1 e2 = genericPatternExpressions e1 e2
  valueByIdentifier "silence" = Right $ T.silence
  valueByStringLiteral x = parseBP x


instance MiniTidal (Pattern Int) where
  valueByApplication e1 e2 = genericPatternExpressions e1 e2
  valueByOperator e1 op e2 =
    (unionableMergeOperator op <|>
     numMergeOperator op <|>
     Left "Pattern Int merge operator expected")
    <*> value e1 <*> value e2
  valueByIdentifier "silence" = Right $ T.silence
  valueByStringLiteral x = parseBP x
  valueByIntegerLiteral x = pure (fromIntegral x)


instance MiniTidal (Pattern Double) where
  valueByApplication e1 e2 = genericPatternExpressions e1 e2
  valueByOperator e1 op e2 =
    (unionableMergeOperator op <|>
     numMergeOperator op <|>
     realMergeOperator op <|>
     fractionalMergeOperator op <|>
     Left "Double merge operator expected")
    <*> value e1 <*> value e2
  valueByIdentifier "silence" = Right $ T.silence
  valueByIdentifier "rand" = Right $ T.rand
  valueByIdentifier "sine" = Right $ T.sine
  valueByIdentifier "saw" = Right $ T.saw
  valueByIdentifier "isaw" = Right $ T.isaw
  valueByIdentifier "tri" = Right $ T.tri
  valueByIdentifier "square" = Right $ T.square
  valueByIdentifier "cosine" = Right $ T.cosine
  valueByIdentifier _ = Left "unexpected identifier where Pattern Double expected"
  valueByStringLiteral x = parseBP x
  valueByIntegerLiteral x = pure (fromIntegral x)
  valueByRationalLiteral x = pure (realToFrac x)


instance MiniTidal (Pattern Time) where
  valueByApplication e1 e2 = genericPatternExpressions e1 e2
  valueByOperator e1 op e2 =
    (unionableMergeOperator op <|>
     numMergeOperator op <|>
     realMergeOperator op <|>
     fractionalMergeOperator op <|>
     Left "Pattern Time merge operator expected")
    <*> value e1 <*> value e2
  valueByIdentifier "silence" = Right $ T.silence
  valueByStringLiteral x = parseBP x
  valueByIntegerLiteral x = pure (fromIntegral x)
  valueByRationalLiteral x = pure (pure x)


instance MiniTidal (Pattern String -> ControlPattern) where
  valueByIdentifier "s" = Right T.s
  valueByIdentifier "sound" = Right T.sound
  valueByIdentifier "vowel" = Right T.vowel
  valueByIdentifier _ = Left "expected Pattern String -> ControlPattern"


instance MiniTidal (Pattern Int -> ControlPattern) where
  valueByIdentifier "coarse" = Right T.coarse
  valueByIdentifier "cut" = Right T.cut
  valueByIdentifier _ = Left "expected Pattern Int -> ControlPattern"


instance MiniTidal (Pattern Double -> ControlPattern) where
  valueByIdentifier "n" = Right T.n
  valueByIdentifier "up" = Right T.up
  valueByIdentifier "speed" = Right T.speed
  valueByIdentifier "pan" = Right T.pan
  valueByIdentifier "shape" = Right T.shape
  valueByIdentifier "gain" = Right T.gain
  valueByIdentifier "accelerate" = Right T.accelerate
  valueByIdentifier "bandf" = Right T.bandf
  valueByIdentifier "bandq" = Right T.bandq
  valueByIdentifier "begin" = Right T.begin
  valueByIdentifier "crush" = Right T.crush
  valueByIdentifier "cutoff" = Right T.cutoff
  valueByIdentifier "delayfeedback" = Right T.delayfeedback
  valueByIdentifier "delaytime" = Right T.delaytime
  valueByIdentifier "delay" = Right T.delay
  valueByIdentifier "end" = Right T.end
  valueByIdentifier "hcutoff" = Right T.hcutoff
  valueByIdentifier "hresonance" = Right T.hresonance
  valueByIdentifier "resonance" = Right T.resonance
  valueByIdentifier "loop" = Right T.loop
  valueByIdentifier "note" = Right T.note
  valueByIdentifier _ = Left "expected Pattern Double -> ControlPattern"


instance (MiniTidal a, MiniTidal (Pattern a)) => MiniTidal (Pattern a -> Pattern a) where
  valueByApplication e1 e2 = genericTransformationsByApplication e1 e2
  valueByIdentifier x = oneWordTransformations x

oneWordTransformations :: String -> Result (Pattern a -> Pattern a)
oneWordTransformations "brak" = Right T.brak
oneWordTransformations "rev" = Right T.rev
oneWordTransformations "palindrome" = Right T.palindrome
oneWordTransformations "stretch" = Right T.stretch
oneWordTransformations "loopFirst" = Right T.loopFirst
oneWordTransformations "degrade" = Right T.degrade
oneWordTransformations _ = Left "expected 'oneWordTransformation', eg. rev, brak, etc"

genericTransformationsByApplication :: (MiniTidal a, MiniTidal (Pattern a)) => Exp SrcSpanInfo -> Exp SrcSpanInfo -> Result (Pattern a -> Pattern a)
genericTransformationsByApplication e1 e2 =
  (value e1 :: Result (Time -> Pattern a -> Pattern a)) <*> value e2 <|>
  (value e1 :: Result (Int -> Pattern a -> Pattern a)) <*> value e2 <|>
  (value e1 :: Result ((Time,Time) -> Pattern a -> Pattern a)) <*> value e2 <|>
  --
  (value e1 :: Result (Pattern a -> Pattern a -> Pattern a)) <*> value e2 <|>
  (value e1 :: Result (Pattern Time -> Pattern a -> Pattern a)) <*> value e2 <|>
  (value e1 :: Result (Pattern Int -> Pattern a -> Pattern a)) <*> value e2 <|>
  (value e1 :: Result (Pattern String -> Pattern a -> Pattern a)) <*> value e2 <|>
  (value e1 :: Result (Pattern Double -> Pattern a -> Pattern a)) <*> value e2 <|>
  (value e1 :: Result ([Pattern a] -> Pattern a -> Pattern a)) <*> value e2 <|>
--  (value e1 :: Result ([Time] -> Pattern a -> Pattern a)) <*> value e2 <|>
--  (value e1 :: Result ([Pattern Int] -> Pattern a -> Pattern a)) <*> value e2 <|>
--  (value e1 :: Result ([Pattern a -> Pattern a] -> Pattern a -> Pattern a)) <*> value e2 <|>
  (value e1 :: Result ((Pattern a -> Pattern a) -> Pattern a -> Pattern a)) <*> value e2

instance {-# INCOHERENT #-} MiniTidal (Time -> Pattern a -> Pattern a) where
  valueByIdentifier "rotL" = Right T.rotL
  valueByIdentifier "rotR" = Right T.rotR
  valueByIdentifier _ = Left "expected Time -> Pattern a -> Pattern a"
  valueByApplication e1 e2 = (value e1 :: Result (Time -> Time -> Pattern a -> Pattern a)) <*> value e2

instance MiniTidal (Time -> Time -> Pattern a -> Pattern a) where
  valueByIdentifier "playFor" = Right T.playFor
  valueByIdentifier _ = Left "expected Time -> Time -> Pattern a -> Pattern a"

instance {-# INCOHERENT #-} MiniTidal (Int -> Pattern a -> Pattern a) where
  valueByIdentifier "repeatCycles" = Right T.repeatCycles
  valueByIdentifier _ = Left "expected Pattern Int -> Pattern a -> Pattern a"

instance {-# INCOHERENT #-} MiniTidal ((Time,Time) -> Pattern a -> Pattern a) where
  valueByIdentifier "compress" = Right T.compress
  valueByIdentifier "zoom" = Right T.zoom
  valueByIdentifier "compressTo" = Right T.compressTo
  valueByIdentifier _ = Left "expected Pattern (Time,Time) -> Pattern a -> Pattern a"

instance MiniTidal (Pattern a -> Pattern a -> Pattern a) where -- *** many binary functions, eg. Num functions, missing from this currently
  valueByIdentifier "overlay" = Right T.overlay
  valueByIdentifier "append" = Right T.append
  valueByIdentifier _ = Left "expected Pattern a -> Pattern a -> Pattern a"

instance {-# INCOHERENT #-} MiniTidal (Pattern Time -> Pattern a -> Pattern a) where
  valueByIdentifier "fast" = Right T.fast
  valueByIdentifier "fastGap" = Right T.fast
  valueByIdentifier "density" = Right T.fast
  valueByIdentifier "slow" = Right T.fast
  valueByIdentifier "trunc" = Right T.fast
  valueByIdentifier "densityGap" = Right T.fast
  valueByIdentifier "sparsity" = Right T.fast
  valueByIdentifier "linger" = Right T.fast
  valueByIdentifier "segment" = Right T.fast
  valueByIdentifier "discretise" = Right T.fast
  valueByIdentifier "timeLoop" = Right T.fast
  valueByIdentifier "swing" = Right T.fast
  valueByIdentifier _ = Left "expected Pattern Time -> Pattern a -> Pattern a"
  valueByApplication e1 e2 =
    (value e1 :: Result (Pattern Time -> Pattern Time -> Pattern a -> Pattern a)) <*> value e2

instance MiniTidal (Pattern Time -> Pattern Time -> Pattern a -> Pattern a) where
  valueByIdentifier "swingBy" = Right T.swingBy
  valueByIdentifier _ = Left "expected Pattern Time -> Pattern Time -> Pattern a -> Pattern a"

instance {-# INCOHERENT #-} MiniTidal (Pattern Int -> Pattern a -> Pattern a) where
  valueByIdentifier "iter" = Right T.iter
  valueByIdentifier "iter'" = Right T.iter'
  valueByIdentifier "ply" = Right T.ply
  valueByIdentifier "substruct'" = Right T.substruct'
  valueByIdentifier "slowstripe" = Right T.slowstripe
  valueByIdentifier "shuffle" = Right T.shuffle
  valueByIdentifier "scramble" = Right T.scramble
  valueByIdentifier _ = Left "expected Pattern Int -> Pattern a -> Pattern a"
  valueByApplication e1 e2 =
    (value e1 :: Result (Pattern Int -> Pattern Int -> Pattern a -> Pattern a)) <*> value e2

instance MiniTidal (Pattern Int -> Pattern Int -> Pattern a -> Pattern a) where
  valueByIdentifier "euclid" = Right T.euclid
  valueByIdentifier "euclidInv" = Right T.euclidInv
  valueByIdentifier _ = Left "expected Pattern Int -> Pattern Int -> Pattern a -> Pattern a"
  valueByApplication e1 e2 =
    (value e1 :: Result (Int -> Pattern Int -> Pattern Int -> Pattern a -> Pattern a)) <*> value e2

instance MiniTidal (Int -> Pattern Int -> Pattern Int -> Pattern a -> Pattern a) where
  valueByApplication e1 e2 =
    (value e1 :: Result (Pattern Time -> Int -> Pattern Int -> Pattern Int -> Pattern a -> Pattern a)) <*> value e2

instance MiniTidal (Pattern Time -> Int -> Pattern Int -> Pattern Int -> Pattern a -> Pattern a) where
  valueByIdentifier "fit'" = Right T.fit'
  valueByIdentifier _ = Left "expected Pattern Time -> Int -> Pattern Int -> Pattern Int -> Pattern a -> Pattern a"

instance {-# INCOHERENT #-} MiniTidal (Pattern String -> Pattern a -> Pattern a) where
  valueByIdentifier "substruct" = Right T.substruct
  valueByIdentifier _ = Left "expected Pattern String -> Pattern a -> Pattern a"

instance {-# INCOHERENT #-} MiniTidal (Pattern Double -> Pattern a -> Pattern a) where
  valueByIdentifier "degradeBy" = Right T.degradeBy
  valueByIdentifier "unDegradeBy" = Right T.unDegradeBy
  valueByIdentifier _ = Left "expected Pattern Double -> Pattern a -> Pattern a"
  valueByApplication e1 e2 =
    (value e1 :: Result (Int -> Pattern Double -> Pattern a -> Pattern a)) <*> value e2

instance MiniTidal (Int -> Pattern Double -> Pattern a -> Pattern a) where
  valueByIdentifier "degradeOverBy" = Right T.degradeOverBy
  valueByIdentifier _ = Left "expected Int -> Pattern Double -> Pattern a -> Pattern a"

instance MiniTidal ([Pattern a] -> Pattern a -> Pattern a) where
  valueByApplication e1 e2 =
    (value e1 :: Result ((Pattern a -> Pattern a -> Pattern a) -> [Pattern a] -> Pattern a -> Pattern a)) <*> value e2

instance MiniTidal ((a -> b -> Pattern c) -> [a] -> b -> Pattern c) where
  valueByIdentifier "spread" = Right T.spread
  valueByIdentifier "slowspread" = Right T.slowspread
  valueByIdentifier "fastspread" = Right T.fastspread
  valueByIdentifier _ = Left "expected (a -> b -> Pattern c) -> [a] -> b -> Pattern c"



instance {-# INCOHERENT #-} MiniTidal (ControlPattern -> ControlPattern) where
  valueByApplication e1 e2 =
    genericTransformationsByApplication e1 e2 <|>
    (value e1 :: Result ((ControlPattern -> ControlPattern) -> ControlPattern -> ControlPattern)) <*> value e2
  valueByIdentifier x = oneWordTransformations x

instance MiniTidal ((Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  valueByApplication e1 e2 =
    (value e1 :: Result (Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)) <*> value e2 <|>
    (value e1 :: Result (Pattern Time -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)) <*> value e2

instance {-# INCOHERENT #-} MiniTidal ((ControlPattern -> ControlPattern) -> ControlPattern -> ControlPattern) where
  valueByApplication e1 e2 =
    value e1 <*> (value e2 :: Result (Pattern Int)) <|>
    value e1 <*> (value e2 :: Result (Pattern Time))
  valueByIdentifier "jux" = Right T.jux
  valueByIdentifier _ = Left "expected (ControlPattern -> ControlPattern) -> ControlPattern -> ControlPattern"

instance MiniTidal (Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  valueByIdentifier "every" = Right T.every
  valueByIdentifier _ = Left "expected Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a"

instance MiniTidal (Pattern Time -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  valueByIdentifier "off" = Right T.off
  valueByIdentifier _ = Left "expected Pattern Time -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a"


genericPatternExpressions :: forall a. (MiniTidal a, MiniTidal (Pattern a)) => Exp SrcSpanInfo -> Exp SrcSpanInfo -> Result (Pattern a)
genericPatternExpressions e1 e2 =
  (value e1 :: Result (Pattern a -> Pattern a)) <*> value e2 <|>
  (value e1 :: Result ([a] -> Pattern a)) <*> value e2 <|>
  (value e1 :: Result ([Pattern a] -> Pattern a)) <*> value e2 <|>
  (value e1 :: Result (Pattern Int -> Pattern a)) <*> value e2

instance MiniTidal ([a] -> Pattern a) where
  valueByIdentifier "listToPat" = Right T.listToPat
  valueByIdentifier "choose" = Right T.choose
  valueByIdentifier "cycleChoose" = Right T.cycleChoose
  valueByIdentifier _ = Left "expected [a] -> Pattern a"

instance MiniTidal ([Pattern a] -> Pattern a) where
  valueByIdentifier "stack" = Right T.stack
  valueByIdentifier "fastcat" = Right T.fastcat
  valueByIdentifier "slowcat" = Right T.slowcat
  valueByIdentifier "cat" = Right T.cat
  valueByIdentifier "randcat" = Right T.randcat
  valueByIdentifier _ = Left "expected [Pattern a] -> Pattern a"

instance {-# INCOHERENT #-} MiniTidal [a] => MiniTidal (Pattern Int -> Pattern a) where
  valueByApplication e1 e2 = (value e1 :: Result ([a] -> Pattern Int -> Pattern a)) <*> value e2

instance MiniTidal ([a] -> Pattern Int -> Pattern a) where
  valueByApplication e1 e2 = (value e1 :: Result (Int -> [a] -> Pattern Int -> Pattern a)) <*> value e2

instance MiniTidal (Int -> [a] -> Pattern Int -> Pattern a) where
  valueByIdentifier "fit" = Right T.fit
  valueByIdentifier _ = Left "expected Int -> [a] -> Pattern Int -> Pattern a"


unionableMergeOperator :: T.Unionable a => String -> Result (Pattern a -> Pattern a -> Pattern a)
unionableMergeOperator "#" = Right (T.#)
unionableMergeOperator "|>|" = Right (T.|>|)
unionableMergeOperator "|>" = Right (T.|>)
unionableMergeOperator ">|" = Right (T.>|)
unionableMergeOperator "|<|" = Right (T.|<|)
unionableMergeOperator "|<" = Right (T.|<)
unionableMergeOperator "<|" = Right (T.<|)
unionableMergeOperator _ = Left "Unionable merge operator expected"

numMergeOperator :: Num a => String -> Result (Pattern a -> Pattern a -> Pattern a)
numMergeOperator "|+|" = Right (T.|+|)
numMergeOperator "|+" = Right (T.|+)
numMergeOperator "+|" = Right (T.+|)
numMergeOperator "|-|" = Right (T.|-|)
numMergeOperator "|-" = Right (T.|-)
numMergeOperator "-|" = Right (T.-|)
numMergeOperator "|*|" = Right (T.|*|)
numMergeOperator "|*" = Right (T.|*)
numMergeOperator "*|" = Right (T.*|)
numMergeOperator "+" = Right (+)
numMergeOperator "*" = Right (*)
numMergeOperator "-" = Right (-)
numMergeOperator _ = Left "Num merge operator expected"

realMergeOperator :: Real a => String -> Result (Pattern a -> Pattern a -> Pattern a)
realMergeOperator "|%|" = Right (T.|%|)
realMergeOperator "|%" = Right (T.|%)
realMergeOperator "%|" = Right (T.%|)
realMergeOperator _ = Left "Real merge operator expected"

fractionalMergeOperator :: Fractional a => String -> Result (Pattern a -> Pattern a -> Pattern a)
fractionalMergeOperator "|/|" = Right (T.|/|)
fractionalMergeOperator "|/" = Right (T.|/)
fractionalMergeOperator "/|" = Right (T./|)
fractionalMergeOperator _ = Left "Fractional merge operator expected"

parseBP :: (Enumerable a, T.Parseable a) => String -> Either String (Pattern a)
parseBP x = first show $ T.parseBP x

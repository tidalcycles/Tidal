{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

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
value _ = Left "internal error: unhandled structure in Sound.Tidal.MiniTidal.value"

-- Next, we provide MiniTidal instance declarations for as many as possible of the types
-- that occur in typical TidalCycles usage patterns, beginning with ControlPattern, which
-- represents a patterning of different types of parameters to the Dirt/SuperDirt/WebDirt/etc
-- sample engines, and is thus close to the top of TidalCycles' type hierarchy. Note how
-- the use of value to parse expression components sometimes requires type annotations given
-- the extreme polymorphism of this function (ie. how many instances of MiniTidal we have).

instance MiniTidal ControlPattern where
  valueByApplication e1 e2 =
    value e1 <*> (value e2 :: Result ControlPattern) <|>
    value e1 <*> (value e2 :: Result (Pattern String)) <|>
    value e1 <*> (value e2 :: Result (Pattern Double)) <|>
    value e1 <*> (value e2 :: Result (Pattern Int))
  valueByOperator e1 op e2 =
    (unionableMergeOperator op <|> numMergeOperator op <|> fractionalMergeOperator op <|> Left "ControlPattern merge operator expected")
    <*> value e1 <*> value e2
  valueByIdentifier "silence" = Right $ T.silence


instance MiniTidal (Pattern String) where
  valueByApplication e1 e2 = value e1 <*> (value e2 :: Result (Pattern String))
  valueByIdentifier "silence" = Right $ T.silence
  valueByStringLiteral x = parseBP x


instance MiniTidal (Pattern Int) where
  valueByApplication e1 e2 = value e1 <*> (value e2 :: Result (Pattern Int))
  valueByOperator e1 op e2 =
    (unionableMergeOperator op <|> numMergeOperator op <|> Left "Pattern Int merge operator expected")
    <*> value e1 <*> value e2
  valueByIdentifier "silence" = Right $ T.silence
  valueByStringLiteral x = parseBP x
  valueByIntegerLiteral x = pure (fromIntegral x)


instance MiniTidal (Pattern Double) where
  valueByApplication e1 e2 = value e1 <*> (value e2 :: Result (Pattern Double))
  valueByOperator e1 op e2 =
    (unionableMergeOperator op <|> numMergeOperator op <|> realMergeOperator op <|> fractionalMergeOperator op <|> Left "Double merge operator expected")
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
  valueByOperator e1 op e2 =
    (unionableMergeOperator op <|> numMergeOperator op <|> realMergeOperator op <|> fractionalMergeOperator op <|> Left "Pattern Time merge operator expected")
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


instance MiniTidal (Pattern a -> Pattern a) where
  valueByApplication e1 e2 = genericTransformationsByApplication e1 e2
  valueByIdentifier x = oneWordTransformations x

instance {-# OVERLAPPING #-} MiniTidal (ControlPattern -> ControlPattern) where
  valueByApplication e1 e2 = genericTransformationsByApplication e1 e2 <|> controlPatternTransformationsByApplication e1 e2
  valueByIdentifier x = oneWordTransformations x

instance MiniTidal ((Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  valueByApplication e1 e2 =
    value e1 <*> (value e2 :: Result (Pattern Int))

instance {-# OVERLAPPING #-} MiniTidal ((ControlPattern -> ControlPattern) -> ControlPattern -> ControlPattern) where
  valueByIdentifier "jux" = Right T.jux
  valueByIdentifier _ = Left "expected (ControlPattern -> ControlPattern) -> ControlPattern -> ControlPattern"




oneWordTransformations :: String -> Result (Pattern a -> Pattern a)
oneWordTransformations "rev" = Right T.rev
oneWordTransformations "brak" = Right T.brak
oneWordTransformations _ = Left "expected 'oneWordTransformation', eg. rev, brak, etc"

genericTransformationsByApplication :: Exp SrcSpanInfo -> Exp SrcSpanInfo -> Result (Pattern a -> Pattern a)
genericTransformationsByApplication e1 e2 =
  (value e1 :: Result (Pattern Time -> Pattern a -> Pattern a)) <*> value e2 -- <|>
--  (value e1 :: Result ((Pattern a -> Pattern a) -> Pattern a -> Pattern a)) <*> (value e2 :: Result (Pattern a -> Pattern a))


controlPatternTransformationsByApplication :: Exp SrcSpanInfo -> Exp SrcSpanInfo -> Result (ControlPattern -> ControlPattern)
controlPatternTransformationsByApplication e1 e2 =
  value e1 <*> (value e2 :: Result (ControlPattern -> ControlPattern))

instance MiniTidal (Pattern Time -> Pattern a -> Pattern a) where
  valueByIdentifier "fast" = Right T.fast
  valueByIdentifier _ = Left "expected Pattern Time -> Pattern a -> Pattern a"

instance MiniTidal (Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  valueByIdentifier "every" = Right T.every
  valueByIdentifier _ = Left "expected Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a"


-- working on every
--  :: Pattern Int
--     -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a


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

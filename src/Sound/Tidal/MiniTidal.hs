{-# LANGUAGE TemplateHaskell, FlexibleInstances, FlexibleContexts, ScopedTypeVariables, UndecidableInstances #-}

module Sound.Tidal.MiniTidal (miniTidal) where

import           Language.Haskell.Exts
import           Control.Applicative
import           Data.Bifunctor

import           Sound.Tidal.Context (Pattern,ControlMap,ControlPattern,Enumerable,Time)
import qualified Sound.Tidal.Context as T
import           Sound.Tidal.MiniTidal.TH
import           Sound.Tidal.MiniTidal.ExpParser


-- This is depended upon by Estuary, and changes to its type will cause problems downstream for Estuary.
miniTidal :: String -> Either String ControlPattern
miniTidal = f . parseExp
  where
    f (ParseOk x) = runExpParser parser x
    f (ParseFailed l "Parse error: EOF") = Right $ T.silence
    f (ParseFailed l s) = Left $ show l ++ ": " ++ show s

{- test :: MiniTidal a => String -> Either String a
test = f. parseExp
  where
    f (ParseOk x) = value x
    f (ParseFailed l s) = Left $ show l ++ ": " ++ show s -}



-- The class MiniTidal is a class for all of the types that we know how to parse.
-- For each type, we provide all the ways we can think of producing that type
-- via expressions in MiniTidal.

class MiniTidal a where
  parser :: ExpParser a

instance MiniTidal Int where
  parser = fromIntegral <$> integer

instance MiniTidal Integer where
  parser = integer

instance MiniTidal Time where
  parser = rational

instance MiniTidal Double where
  parser = (fromIntegral <$> integer) <|> (realToFrac <$> rational)

instance {-# INCOHERENT #-} MiniTidal String where
  parser = string

instance MiniTidal a => MiniTidal (a,a) where
  parser = tupleOf parser

instance MiniTidal a => MiniTidal [a] where
  parser = listOf parser

instance MiniTidal ControlMap where
  parser = empty


instance MiniTidal ControlPattern where
  parser =
    (parser :: ExpParser (Pattern String -> ControlPattern)) <*> parser <|>
    (parser :: ExpParser (Pattern Double -> ControlPattern)) <*> parser <|>
    (parser :: ExpParser (Pattern Int -> ControlPattern)) <*> parser <|>
    genericPatternExpressions


genericPatternExpressions :: forall a. (MiniTidal a, MiniTidal (Pattern a),MiniTidal (Pattern a -> Pattern a)) => ExpParser (Pattern a)
genericPatternExpressions =
  (parser :: ExpParser (Pattern a -> Pattern a)) <*> parser <|>
  (parser :: ExpParser ([a] -> Pattern a)) <*> parser <|>
  (parser :: ExpParser ([Pattern a] -> Pattern a)) <*> parser <|>
  pInt_p <*> parser <|>
  silence

silence :: ExpParser (Pattern a)
silence = $(fromTidal "silence") -- ie. T.silence <$ reserved "silence", see Sound.Tidal.MiniTidal.TH


instance MiniTidal (Pattern String) where
  parser =
    parseBP <|>
    genericPatternExpressions <|>
    pInt_pString <*> parser

parseBP :: (Enumerable a, T.Parseable a) => ExpParser (Pattern a)
parseBP = join ((first show . T.parseBP) <$> string)

instance MiniTidal (Pattern Int) where
  parser =
    (pure . fromIntegral) <$> integer <|>
    parseBP <|>
    genericPatternExpressions <|>
    irand <*> parser

irand :: Num a => ExpParser (Int -> Pattern a)
irand = $(fromTidal "irand")

instance MiniTidal (Pattern Integer) where
  parser =
    pure <$> integer <|>
    parseBP <|>
    genericPatternExpressions <|>
    irand <*> parser

instance MiniTidal (Pattern Double) where
  parser =
    (pure . fromIntegral) <$> integer <|>
    (pure . realToFrac) <$> rational <|>
    parseBP <|>
    genericPatternExpressions <|>
    irand <*> parser <|>
    $(fromTidal "rand") <|>
    $(fromTidal "sine") <|>
    $(fromTidal "saw") <|>
    $(fromTidal "isaw") <|>
    $(fromTidal "tri") <|>
    $(fromTidal "square") <|>
    $(fromTidal "cosine")

instance MiniTidal (Pattern Time) where
  parser =
    (pure . fromIntegral) <$> integer <|>
    pure <$> rational <|>
    parseBP <|>
    genericPatternExpressions <|>
    irand <*> parser




-- * -> *

instance MiniTidal (ControlPattern -> ControlPattern) where
  parser =
    genericTransformations <|>
    (parser :: ExpParser (Pattern Int -> ControlPattern -> ControlPattern)) <*> parser <|>
    (parser :: ExpParser (Pattern Double -> ControlPattern -> ControlPattern)) <*> parser <|>
    (parser :: ExpParser (Pattern Time -> ControlPattern -> ControlPattern)) <*> parser

instance MiniTidal (Pattern String -> Pattern String) where parser = genericTransformations
instance MiniTidal (Pattern Int -> Pattern Int) where
  parser = genericTransformations <|> numTransformations
instance MiniTidal (Pattern Integer -> Pattern Integer) where
  parser = genericTransformations <|> numTransformations
instance MiniTidal (Pattern Time -> Pattern Time) where
  parser = genericTransformations <|> numTransformations
instance MiniTidal (Pattern Double -> Pattern Double) where
  parser = genericTransformations <|> numTransformations

genericTransformations :: forall a. (MiniTidal (Pattern a), MiniTidal (Pattern a -> Pattern a),MiniTidal (Pattern a -> Pattern a -> Pattern a), MiniTidal ((Pattern a -> Pattern a) -> Pattern a -> Pattern a)) => ExpParser (Pattern a -> Pattern a)
genericTransformations =
    (parser :: ExpParser (Pattern a -> Pattern a -> Pattern a)) <*> parser <|>
    asRightSection (parser :: ExpParser (Pattern a -> Pattern a -> Pattern a)) parser <|>
    (parser :: ExpParser ((Pattern a -> Pattern a) -> Pattern a -> Pattern a)) <*> parser <|>
    $(fromTidal "brak") <|>
    $(fromTidal "rev") <|>
    $(fromTidal "palindrome") <|>
    $(fromTidal "stretch") <|>
    $(fromTidal "loopFirst") <|>
    $(fromTidal "degrade") <|>
    constParser <*> parser <|>
    -- more complex possibilities that would involve overlapped MiniTidal instances if they were instances
    pTime_p_p <*> parser <|>
    pInt_p_p <*> parser <|>
    pString_p_p <*> parser <|>
    pDouble_p_p <*> parser <|>
    lpInt_p_p <*> parser <|>
    -- more complex possibilities that wouldn't involve overlapped MiniTidal instances
    (parser :: ExpParser (Time -> Pattern a -> Pattern a)) <*> parser <|>
    (parser :: ExpParser (Int -> Pattern a -> Pattern a)) <*> parser <|>
    (parser :: ExpParser ((Time,Time) -> Pattern a -> Pattern a)) <*> parser <|>
    (parser :: ExpParser ([Time] -> Pattern a -> Pattern a)) <*> parser <|>
    (parser :: ExpParser ([Pattern Time] -> Pattern a -> Pattern a)) <*> parser <|>
    (parser :: ExpParser ([Pattern String] -> Pattern a -> Pattern a)) <*> parser <|>
    (parser :: ExpParser ([Pattern Double] -> Pattern a -> Pattern a)) <*> parser <|>
    (parser :: ExpParser ([Pattern a -> Pattern a] -> Pattern a -> Pattern a)) <*> parser <|>
    lp_p_p <*> parser

numTransformations :: (Num a, Enum a) => ExpParser (Pattern a -> Pattern a)
numTransformations =
  $(fromTidal "run")

instance MiniTidal ([a] -> Pattern a) where
  parser =
    $(fromTidal "listToPat") <|>
    $(fromTidal "choose") <|>
    $(fromTidal "cycleChoose")

instance MiniTidal ([Pattern a] -> Pattern a) where
  parser =
    $(fromTidal "stack") <|>
    $(fromTidal "fastcat") <|>
    $(fromTidal "slowcat") <|>
    $(fromTidal "cat") <|>
    $(fromTidal "randcat")

pInt_p :: MiniTidal a => ExpParser (Pattern Int -> Pattern a)
pInt_p = (parser :: ExpParser ([a] -> Pattern Int -> Pattern a)) <*> parser

instance MiniTidal (Pattern String -> ControlPattern) where
  parser =
    $(fromTidal "s") <|>
    $(fromTidal "sound") <|>
    $(fromTidal "vowel")

instance MiniTidal (Pattern Int -> ControlPattern) where
  parser =
    $(fromTidal "coarse") <|>
    $(fromTidal "cut")

instance MiniTidal (Pattern Double -> ControlPattern) where
  parser =
    $(fromTidal "n") <|>
    $(fromTidal "up") <|>
    $(fromTidal "speed") <|>
    $(fromTidal "pan") <|>
    $(fromTidal "shape") <|>
    $(fromTidal "gain") <|>
    $(fromTidal "accelerate") <|>
    $(fromTidal "bandf") <|>
    $(fromTidal "bandq") <|>
    $(fromTidal "begin") <|>
    $(fromTidal "crush") <|>
    $(fromTidal "cutoff") <|>
    $(fromTidal "delayfeedback") <|>
    $(fromTidal "delaytime") <|>
    $(fromTidal "delay") <|>
    $(fromTidal "end") <|>
    $(fromTidal "hcutoff") <|>
    $(fromTidal "hresonance") <|>
    $(fromTidal "resonance") <|>
    $(fromTidal "loop") <|>
    $(fromTidal "note")

pInt_pString :: ExpParser (Pattern Int -> Pattern String)
pInt_pString = pString_pInt_pString <*> parser


-- * -> * -> *

instance MiniTidal (Pattern String -> Pattern String -> Pattern String) where
  parser =
    genericBinaryPatternFunctions <|>
    pString_p_p

instance MiniTidal (Pattern Int -> Pattern Int -> Pattern Int) where
  parser =
    genericBinaryPatternFunctions <|>
    numMergeOperator <|>
    pInt_p_p

instance MiniTidal (Pattern Integer -> Pattern Integer -> Pattern Integer) where
  parser =
    genericBinaryPatternFunctions <|>
    numMergeOperator

instance MiniTidal (Pattern Time -> Pattern Time -> Pattern Time) where
  parser =
    genericBinaryPatternFunctions <|>
    numMergeOperator <|>
    realMergeOperator <|>
    fractionalMergeOperator <|>
    pTime_p_p

instance MiniTidal (Pattern Double -> Pattern Double -> Pattern Double) where
  parser =
    genericBinaryPatternFunctions <|>
    numMergeOperator <|>
    realMergeOperator <|>
    fractionalMergeOperator <|>
    pDouble_p_p

instance MiniTidal (ControlPattern -> ControlPattern -> ControlPattern) where
  parser =
    genericBinaryPatternFunctions <|>
    numMergeOperator <|>
    fractionalMergeOperator

genericBinaryPatternFunctions :: T.Unionable a => ExpParser (Pattern a -> Pattern a -> Pattern a)
genericBinaryPatternFunctions =
  $(fromTidal "overlay") <|>
  $(fromTidal "append") <|>
  unionableMergeOperator <|>
  pInt_p_p_p <*> parser <|>
  constParser

unionableMergeOperator :: T.Unionable a => ExpParser (Pattern a -> Pattern a -> Pattern a)
unionableMergeOperator =
  $(fromTidal "#") <|>
  $(fromTidal "|>|") <|>
  $(fromTidal "|>") <|>
  $(fromTidal ">|") <|>
  $(fromTidal "|<|") <|>
  $(fromTidal "|<") <|>
  $(fromTidal "<|")

numMergeOperator :: (Num a, MiniTidal (Pattern a)) => ExpParser (Pattern a -> Pattern a -> Pattern a)
numMergeOperator =
  numTernaryTransformations <*> parser <|>
  $(fromTidal "|+|") <|>
  $(fromTidal "|+") <|>
  $(fromTidal "+|") <|>
  $(fromTidal "|-|") <|>
  $(fromTidal "|-") <|>
  $(fromTidal "-|") <|>
  $(fromTidal "|*|") <|>
  $(fromTidal "|*") <|>
  $(fromTidal "*|") <|>
  $(fromHaskell "+") <|>
  $(fromHaskell "*") <|>
  $(fromHaskell "-")

realMergeOperator :: Real a => ExpParser (Pattern a -> Pattern a -> Pattern a)
realMergeOperator =
  $(fromTidal "|%|") <|>
  $(fromTidal "|%") <|>
  $(fromTidal "%|")

fractionalMergeOperator :: Fractional a => ExpParser (Pattern a -> Pattern a -> Pattern a)
fractionalMergeOperator =
  $(fromTidal "|/|") <|>
  $(fromTidal "|/") <|>
  $(fromTidal "/|") <|>
  $(fromHaskell "/")

constParser :: ExpParser (a -> b -> a)
constParser = $(fromHaskell "const")

instance MiniTidal (Time -> Pattern a -> Pattern a) where
  parser =
    $(fromTidal "rotL") <|>
    $(fromTidal "rotR") <|>
    (parser :: ExpParser (Time -> Time -> Pattern a -> Pattern a)) <*> parser

instance MiniTidal (Int -> Pattern a -> Pattern a) where
  parser = $(fromTidal "repeatCycles")

instance MiniTidal ((Time,Time) -> Pattern a -> Pattern a) where
  parser =
    $(fromTidal "compress") <|>
    $(fromTidal "zoom") <|>
    $(fromTidal "compressTo")

pString_pInt_pString :: ExpParser (Pattern String -> Pattern Int -> Pattern String)
pString_pInt_pString = $(fromTidal "samples")

pTime_p_p :: ExpParser (Pattern Time -> Pattern a -> Pattern a)
pTime_p_p =
    $(fromTidal "fast") <|>
    $(fromTidal "fastGap") <|>
    $(fromTidal "density") <|>
    $(fromTidal "slow") <|>
    $(fromTidal "trunc") <|>
    $(fromTidal "densityGap") <|>
    $(fromTidal "sparsity") <|>
    $(fromTidal "linger") <|>
    $(fromTidal "segment") <|>
    $(fromTidal "discretise") <|>
    $(fromTidal "timeLoop") <|>
    $(fromTidal "swing") <|>
    $(fromTidal "<~") <|>
    $(fromTidal "~>") <|>
    (parser :: ExpParser (Pattern Time -> Pattern Time -> Pattern a -> Pattern a)) <*> parser

pInt_p_p :: ExpParser (Pattern Int -> Pattern a -> Pattern a)
pInt_p_p =
    $(fromTidal "iter") <|>
    $(fromTidal "iter'") <|>
    $(fromTidal "ply") <|>
    $(fromTidal "substruct'") <|>
    $(fromTidal "slowstripe") <|>
    $(fromTidal "shuffle") <|>
    $(fromTidal "scramble") <|>
    pInt_pInt_p_p <*> parser

pString_p_p :: ExpParser (Pattern String -> Pattern a -> Pattern a)
pString_p_p = $(fromTidal "substruct")

pDouble_p_p :: ExpParser (Pattern Double -> Pattern a -> Pattern a)
pDouble_p_p =
    $(fromTidal "degradeBy") <|>
    $(fromTidal "unDegradeBy") <|>
    (parser :: ExpParser (Int -> Pattern Double -> Pattern a -> Pattern a)) <*> parser

instance MiniTidal ((Pattern a -> Pattern a) -> Pattern a -> Pattern a) => MiniTidal ([Pattern a -> Pattern a] -> Pattern a -> Pattern a) where
  parser = (parser :: ExpParser (((Pattern a -> Pattern a) -> Pattern a -> Pattern a) -> [Pattern a -> Pattern a] -> Pattern a -> Pattern a)) <*> parser

lp_p_p :: MiniTidal (Pattern a -> Pattern a -> Pattern a) => ExpParser ([Pattern a] -> Pattern a -> Pattern a)
lp_p_p = (parser :: ExpParser ((Pattern a -> Pattern a -> Pattern a) -> [Pattern a] -> Pattern a -> Pattern a)) <*> parser


instance MiniTidal ([Pattern Double] -> Pattern a -> Pattern a) where
  parser = (parser :: ExpParser ((Pattern Double -> Pattern a -> Pattern a) -> [Pattern Double] -> Pattern a -> Pattern a)) <*> pDouble_p_p
instance MiniTidal ([Pattern Time] -> Pattern a -> Pattern a) where
  parser = (parser :: ExpParser ((Pattern Time -> Pattern a -> Pattern a) -> [Pattern Time] -> Pattern a -> Pattern a)) <*> pTime_p_p
instance MiniTidal ([Pattern String] -> Pattern a -> Pattern a) where
  parser = (parser :: ExpParser ((Pattern String -> Pattern a -> Pattern a) -> [Pattern String] -> Pattern a -> Pattern a)) <*> pString_p_p

lpInt_p_p :: ExpParser ([Pattern Int] -> Pattern a -> Pattern a)
lpInt_p_p =
  $(fromTidal "distrib") <|>
  (parser :: ExpParser ((Pattern Int -> Pattern a -> Pattern a) -> [Pattern Int] -> Pattern a -> Pattern a)) <*> pInt_p_p

instance MiniTidal ([Time] -> Pattern a -> Pattern a) where
  parser = $(fromTidal "spaceOut")

instance MiniTidal (Pattern Int -> ControlPattern -> ControlPattern) where
  parser =
    $(fromTidal "chop") <|>
    $(fromTidal "striate")

instance MiniTidal (Pattern Double -> ControlPattern -> ControlPattern) where
  parser = (parser :: ExpParser (Pattern Int -> Pattern Double -> ControlPattern -> ControlPattern)) <*> parser

instance MiniTidal (Pattern Time -> ControlPattern -> ControlPattern) where
  parser = (parser :: ExpParser (Pattern Double -> Pattern Time -> ControlPattern -> ControlPattern)) <*> parser

instance MiniTidal ((ControlPattern -> ControlPattern) -> ControlPattern -> ControlPattern) where
  parser =
    genericAppliedTransformations <|>
    $(fromTidal "jux")

instance MiniTidal ((Pattern String -> Pattern String) -> Pattern String -> Pattern String) where
  parser = genericAppliedTransformations
instance MiniTidal ((Pattern Int -> Pattern Int) -> Pattern Int -> Pattern Int) where
  parser = genericAppliedTransformations
instance MiniTidal ((Pattern Integer -> Pattern Integer) -> Pattern Integer -> Pattern Integer) where
  parser = genericAppliedTransformations
instance MiniTidal ((Pattern Time -> Pattern Time) -> Pattern Time -> Pattern Time) where
  parser = genericAppliedTransformations
instance MiniTidal ((Pattern Double -> Pattern Double) -> Pattern Double -> Pattern Double) where
  parser = genericAppliedTransformations

genericAppliedTransformations :: ExpParser ((Pattern a -> Pattern a) -> Pattern a -> Pattern a)
genericAppliedTransformations =
  $(fromHaskell "$") <|>
  $(fromTidal "sometimes") <|>
  $(fromTidal "often") <|>
  $(fromTidal "rarely") <|>
  $(fromTidal "almostNever") <|>
  $(fromTidal "almostAlways") <|>
  $(fromTidal "never") <|>
  $(fromTidal "always") <|>
  $(fromTidal "superimpose") <|>
  $(fromTidal "someCycles") <|>
  (parser :: ExpParser (Pattern Time -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)) <*> parser <|>
  (parser :: ExpParser (Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)) <*> parser <|>
  (parser :: ExpParser (Pattern Double -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)) <*> parser <|>
  (parser :: ExpParser ([Int] -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)) <*> parser <|>
  (parser :: ExpParser ((Time,Time) -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)) <*> parser <|>
  (parser :: ExpParser (Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)) <*> parser <|>
  (parser :: ExpParser (Double -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)) <*> parser

instance MiniTidal ([a] -> Pattern Int -> Pattern a) where
  parser = (parser :: ExpParser (Int -> [a] -> Pattern Int -> Pattern a)) <*> parser




-- * -> * -> * -> *

numTernaryTransformations :: Num a => ExpParser (Pattern a -> Pattern a -> Pattern a -> Pattern a)
numTernaryTransformations = $(fromTidal "range")

instance MiniTidal (Time -> Time -> Pattern a -> Pattern a) where
  parser = $(fromTidal "playFor")

instance MiniTidal (Pattern Time -> Pattern Time -> Pattern a -> Pattern a) where
  parser = $(fromTidal "swingBy")

pInt_pInt_p_p :: ExpParser (Pattern Int -> Pattern Int -> Pattern a -> Pattern a)
pInt_pInt_p_p =
  $(fromTidal "euclid") <|>
  $(fromTidal "euclidInv") <|>
  (parser :: ExpParser (Int -> Pattern Int -> Pattern Int -> Pattern a -> Pattern a)) <*> parser

instance MiniTidal (Int -> Pattern Double -> Pattern a -> Pattern a) where
  parser = $(fromTidal "degradeOverBy")

instance MiniTidal ((a -> b -> Pattern c) -> [a] -> b -> Pattern c) where
  parser =
    $(fromTidal "spread") <|>
    $(fromTidal "slowspread") <|>
    $(fromTidal "fastspread")

pInt_p_p_p :: ExpParser (Pattern Int -> Pattern a -> Pattern a -> Pattern a)
pInt_p_p_p = (parser :: ExpParser (Pattern Int -> Pattern Int -> Pattern a -> Pattern a -> Pattern a)) <*> parser

instance MiniTidal (Pattern Int -> Pattern Double -> ControlPattern -> ControlPattern) where
  parser = $(fromTidal "striate'")

instance MiniTidal (Pattern Double -> Pattern Time -> ControlPattern -> ControlPattern) where
  parser = (parser :: ExpParser (Pattern Integer -> Pattern Double -> Pattern Time -> ControlPattern -> ControlPattern)) <*> parser

instance MiniTidal (Pattern Time -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  parser = $(fromTidal "off")

instance MiniTidal (Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  parser =
    $(fromTidal "every") <|>
    (parser :: ExpParser (Pattern Int -> Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)) <*> parser

instance MiniTidal (Pattern Double -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  parser = $(fromTidal "sometimesBy")

instance MiniTidal ([Int] -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  parser = $(fromTidal "foldEvery")

instance MiniTidal ((Time,Time) -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  parser = $(fromTidal "within")

instance MiniTidal (Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  parser =
    $(fromTidal "chunk") <|>
    (parser :: ExpParser (Int -> Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)) <*> parser

instance MiniTidal (Double -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  parser = $(fromTidal "someCyclesBy")

instance MiniTidal (Int -> [a] -> Pattern Int -> Pattern a) where
  parser = $(fromTidal "fit")




-- * -> * -> * -> * -> *

instance MiniTidal (Pattern Int -> Pattern Int -> Pattern a -> Pattern a -> Pattern a) where
  parser = $(fromTidal "euclidFull")

instance MiniTidal (Int -> Pattern Int -> Pattern Int -> Pattern a -> Pattern a) where
  parser = (parser :: ExpParser (Pattern Time -> Int -> Pattern Int -> Pattern Int -> Pattern a -> Pattern a)) <*> parser

instance MiniTidal (Pattern Integer -> Pattern Double -> Pattern Time -> ControlPattern -> ControlPattern) where
  parser = $(fromTidal "stut")

instance MiniTidal (Pattern Int -> Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  parser = $(fromTidal "every'")

instance MiniTidal (Int -> Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  parser = $(fromTidal "whenmod")




-- * -> * -> * -> * -> * -> *

instance MiniTidal (Pattern Time -> Int -> Pattern Int -> Pattern Int -> Pattern a -> Pattern a) where
  parser = $(fromTidal "fit'")

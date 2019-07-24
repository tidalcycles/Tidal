{-# LANGUAGE TemplateHaskell, FlexibleInstances, FlexibleContexts, ScopedTypeVariables, UndecidableInstances #-}

module Sound.Tidal.Parse (parseTidal) where

import           Language.Haskell.Exts
import           Control.Applicative
import           Data.Bifunctor

import           Sound.Tidal.Context (Pattern,ControlMap,ControlPattern,Enumerable,Time)
import qualified Sound.Tidal.Context as T
import           Sound.Tidal.Parse.TH
import           Sound.Tidal.Parse.ExpParser


-- This is depended upon by Estuary, and changes to its type will cause problems downstream for Estuary.
parseTidal :: String -> Either String ControlPattern
parseTidal = f . parseExp
  where
    f (ParseOk x) = runExpParser parser x
    f (ParseFailed _ "Parse error: EOF") = Right $ T.silence
    f (ParseFailed l s) = Left $ show l ++ ": " ++ show s

{- test :: Parse a => String -> Either String a
test = f. parseExp
  where
    f (ParseOk x) = value x
    f (ParseFailed l s) = Left $ show l ++ ": " ++ show s -}

-- The class Parse is a class for all of the types that we know how to parse.
-- For each type, we provide all the ways we can think of producing that type
-- via expressions in Parse.

class Parse a where
  parser :: ExpParser a

instance Parse Bool where
  parser = (True <$ reserved "True") <|> (False <$ reserved "False")

instance Parse Int where
  parser = fromIntegral <$> integer

instance Parse Integer where
  parser = integer

instance Parse Time where
  parser = rational

instance Parse Double where
  parser = (fromIntegral <$> integer) <|> (realToFrac <$> rational)

instance {-# INCOHERENT #-} Parse String where
  parser = string

instance Parse a => Parse (a,a) where
  parser = tupleOf parser

instance Parse a => Parse [a] where
  parser = listOf parser

instance Parse ControlMap where
  parser = empty


instance Parse ControlPattern where
  parser =
    (parser :: ExpParser (Pattern String -> ControlPattern)) <*> parser <|>
    (parser :: ExpParser (Pattern Double -> ControlPattern)) <*> parser <|>
    (parser :: ExpParser (Pattern Int -> ControlPattern)) <*> parser <|>
    genericPatternExpressions


genericPatternExpressions :: forall a. (Parse a, Parse (Pattern a),Parse (Pattern a -> Pattern a)) => ExpParser (Pattern a)
genericPatternExpressions =
  (parser :: ExpParser (Pattern a -> Pattern a)) <*> parser <|>
  (parser :: ExpParser ([a] -> Pattern a)) <*> parser <|>
  (parser :: ExpParser ([Pattern a] -> Pattern a)) <*> parser <|>
  pInt_p <*> parser <|>
  silence

silence :: ExpParser (Pattern a)
silence = $(fromTidal "silence") -- ie. T.silence <$ reserved "silence", see Sound.Tidal.Parse.TH

instance Parse (Pattern Bool) where
  parser =
    parseBP <|>
    genericPatternExpressions

instance Parse (Pattern String) where
  parser =
    parseBP <|>
    genericPatternExpressions <|>
    pInt_pString <*> parser

parseBP :: (Enumerable a, T.Parseable a) => ExpParser (Pattern a)
parseBP = join ((first show . T.parseBP) <$> string)

instance Parse (Pattern Int) where
  parser =
    (pure . fromIntegral) <$> integer <|>
    parseBP <|>
    genericPatternExpressions <|>
    irand <*> parser

irand :: Num a => ExpParser (Int -> Pattern a)
irand = $(fromTidal "irand")

instance Parse (Pattern Integer) where
  parser =
    pure <$> integer <|>
    parseBP <|>
    genericPatternExpressions <|>
    irand <*> parser

instance Parse (Pattern Double) where
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

instance Parse (Pattern Time) where
  parser =
    (pure . fromIntegral) <$> integer <|>
    pure <$> rational <|>
    parseBP <|>
    genericPatternExpressions <|>
    irand <*> parser




-- * -> *

instance Parse (ControlPattern -> ControlPattern) where
  parser =
    genericTransformations <|>
    (parser :: ExpParser (Pattern Int -> ControlPattern -> ControlPattern)) <*> parser <|>
    (parser :: ExpParser (Pattern Double -> ControlPattern -> ControlPattern)) <*> parser <|>
    (parser :: ExpParser (Pattern Time -> ControlPattern -> ControlPattern)) <*> parser

instance Parse (Pattern Bool -> Pattern Bool) where parser = genericTransformations
instance Parse (Pattern String -> Pattern String) where parser = genericTransformations
instance Parse (Pattern Int -> Pattern Int) where
  parser = genericTransformations <|> numTransformations
instance Parse (Pattern Integer -> Pattern Integer) where
  parser = genericTransformations <|> numTransformations
instance Parse (Pattern Time -> Pattern Time) where
  parser = genericTransformations <|> numTransformations
instance Parse (Pattern Double -> Pattern Double) where
  parser = genericTransformations <|> numTransformations

genericTransformations :: forall a. (Parse (Pattern a), Parse (Pattern a -> Pattern a),Parse (Pattern a -> Pattern a -> Pattern a), Parse ((Pattern a -> Pattern a) -> Pattern a -> Pattern a)) => ExpParser (Pattern a -> Pattern a)
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
    -- more complex possibilities that would involve overlapped Parse instances if they were instances
    pTime_p_p <*> parser <|>
    pInt_p_p <*> parser <|>
    pString_p_p <*> parser <|>
    pDouble_p_p <*> parser <|>
    lpInt_p_p <*> parser <|>
    -- more complex possibilities that wouldn't involve overlapped Parse instances
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

instance Parse ([a] -> Pattern a) where
  parser =
    $(fromTidal "listToPat") <|>
    $(fromTidal "choose") <|>
    $(fromTidal "cycleChoose")

instance Parse ([Pattern a] -> Pattern a) where
  parser =
    $(fromTidal "stack") <|>
    $(fromTidal "fastcat") <|>
    $(fromTidal "slowcat") <|>
    $(fromTidal "cat") <|>
    $(fromTidal "randcat")

pInt_p :: Parse a => ExpParser (Pattern Int -> Pattern a)
pInt_p = (parser :: ExpParser ([a] -> Pattern Int -> Pattern a)) <*> parser

instance Parse (Pattern String -> ControlPattern) where
  parser =
    $(fromTidal "s") <|>
    $(fromTidal "sound") <|>
    $(fromTidal "vowel")

instance Parse (Pattern Int -> ControlPattern) where
  parser =
    $(fromTidal "coarse") <|>
    $(fromTidal "cut")

instance Parse (Pattern Double -> ControlPattern) where
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

instance Parse (Pattern Bool -> Pattern Bool -> Pattern Bool) where
  parser = genericBinaryPatternFunctions

instance Parse (Pattern String -> Pattern String -> Pattern String) where
  parser =
    genericBinaryPatternFunctions <|>
    pString_p_p

instance Parse (Pattern Int -> Pattern Int -> Pattern Int) where
  parser =
    genericBinaryPatternFunctions <|>
    numMergeOperator <|>
    pInt_p_p

instance Parse (Pattern Integer -> Pattern Integer -> Pattern Integer) where
  parser =
    genericBinaryPatternFunctions <|>
    numMergeOperator

instance Parse (Pattern Time -> Pattern Time -> Pattern Time) where
  parser =
    genericBinaryPatternFunctions <|>
    numMergeOperator <|>
    realMergeOperator <|>
    fractionalMergeOperator <|>
    pTime_p_p

instance Parse (Pattern Double -> Pattern Double -> Pattern Double) where
  parser =
    genericBinaryPatternFunctions <|>
    numMergeOperator <|>
    realMergeOperator <|>
    fractionalMergeOperator <|>
    pDouble_p_p

instance Parse (ControlPattern -> ControlPattern -> ControlPattern) where
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
  (parser :: ExpParser (Pattern Bool -> Pattern a -> Pattern a -> Pattern a)) <*> parser <|>
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

numMergeOperator :: (Num a, Parse (Pattern a)) => ExpParser (Pattern a -> Pattern a -> Pattern a)
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

instance Parse (Time -> Pattern a -> Pattern a) where
  parser =
    $(fromTidal "rotL") <|>
    $(fromTidal "rotR") <|>
    (parser :: ExpParser (Time -> Time -> Pattern a -> Pattern a)) <*> parser

instance Parse (Int -> Pattern a -> Pattern a) where
  parser = $(fromTidal "repeatCycles")

instance Parse ((Time,Time) -> Pattern a -> Pattern a) where
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

instance Parse ((Pattern a -> Pattern a) -> Pattern a -> Pattern a) => Parse ([Pattern a -> Pattern a] -> Pattern a -> Pattern a) where
  parser = (parser :: ExpParser (((Pattern a -> Pattern a) -> Pattern a -> Pattern a) -> [Pattern a -> Pattern a] -> Pattern a -> Pattern a)) <*> parser

lp_p_p :: Parse (Pattern a -> Pattern a -> Pattern a) => ExpParser ([Pattern a] -> Pattern a -> Pattern a)
lp_p_p = (parser :: ExpParser ((Pattern a -> Pattern a -> Pattern a) -> [Pattern a] -> Pattern a -> Pattern a)) <*> parser


instance Parse ([Pattern Double] -> Pattern a -> Pattern a) where
  parser = (parser :: ExpParser ((Pattern Double -> Pattern a -> Pattern a) -> [Pattern Double] -> Pattern a -> Pattern a)) <*> pDouble_p_p
instance Parse ([Pattern Time] -> Pattern a -> Pattern a) where
  parser = (parser :: ExpParser ((Pattern Time -> Pattern a -> Pattern a) -> [Pattern Time] -> Pattern a -> Pattern a)) <*> pTime_p_p
instance Parse ([Pattern String] -> Pattern a -> Pattern a) where
  parser = (parser :: ExpParser ((Pattern String -> Pattern a -> Pattern a) -> [Pattern String] -> Pattern a -> Pattern a)) <*> pString_p_p

lpInt_p_p :: ExpParser ([Pattern Int] -> Pattern a -> Pattern a)
lpInt_p_p =
  $(fromTidal "distrib") <|>
  (parser :: ExpParser ((Pattern Int -> Pattern a -> Pattern a) -> [Pattern Int] -> Pattern a -> Pattern a)) <*> pInt_p_p

instance Parse ([Time] -> Pattern a -> Pattern a) where
  parser = $(fromTidal "spaceOut")

instance Parse (Pattern Int -> ControlPattern -> ControlPattern) where
  parser =
    $(fromTidal "chop") <|>
    $(fromTidal "striate")

instance Parse (Pattern Double -> ControlPattern -> ControlPattern) where
  parser = (parser :: ExpParser (Pattern Int -> Pattern Double -> ControlPattern -> ControlPattern)) <*> parser

instance Parse (Pattern Time -> ControlPattern -> ControlPattern) where
  parser = (parser :: ExpParser (Pattern Double -> Pattern Time -> ControlPattern -> ControlPattern)) <*> parser

instance Parse ((ControlPattern -> ControlPattern) -> ControlPattern -> ControlPattern) where
  parser =
    genericAppliedTransformations <|>
    $(fromTidal "jux")

instance Parse ((Pattern Bool -> Pattern Bool) -> Pattern Bool -> Pattern Bool) where
  parser = genericAppliedTransformations
instance Parse ((Pattern String -> Pattern String) -> Pattern String -> Pattern String) where
  parser = genericAppliedTransformations
instance Parse ((Pattern Int -> Pattern Int) -> Pattern Int -> Pattern Int) where
  parser = genericAppliedTransformations
instance Parse ((Pattern Integer -> Pattern Integer) -> Pattern Integer -> Pattern Integer) where
  parser = genericAppliedTransformations
instance Parse ((Pattern Time -> Pattern Time) -> Pattern Time -> Pattern Time) where
  parser = genericAppliedTransformations
instance Parse ((Pattern Double -> Pattern Double) -> Pattern Double -> Pattern Double) where
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

instance Parse ([a] -> Pattern Int -> Pattern a) where
  parser = (parser :: ExpParser (Int -> [a] -> Pattern Int -> Pattern a)) <*> parser




-- * -> * -> * -> *

numTernaryTransformations :: Num a => ExpParser (Pattern a -> Pattern a -> Pattern a -> Pattern a)
numTernaryTransformations = $(fromTidal "range")

instance Parse (Time -> Time -> Pattern a -> Pattern a) where
  parser = $(fromTidal "playFor")

instance Parse (Pattern Time -> Pattern Time -> Pattern a -> Pattern a) where
  parser = $(fromTidal "swingBy")

instance Parse (Pattern Bool -> Pattern a -> Pattern a -> Pattern a) where
  parser = $(fromTidal "sew")

pInt_pInt_p_p :: ExpParser (Pattern Int -> Pattern Int -> Pattern a -> Pattern a)
pInt_pInt_p_p =
  $(fromTidal "euclid") <|>
  $(fromTidal "euclidInv") <|>
  (parser :: ExpParser (Int -> Pattern Int -> Pattern Int -> Pattern a -> Pattern a)) <*> parser

instance Parse (Int -> Pattern Double -> Pattern a -> Pattern a) where
  parser = $(fromTidal "degradeOverBy")

instance Parse ((a -> b -> Pattern c) -> [a] -> b -> Pattern c) where
  parser =
    $(fromTidal "spread") <|>
    $(fromTidal "slowspread") <|>
    $(fromTidal "fastspread")

pInt_p_p_p :: ExpParser (Pattern Int -> Pattern a -> Pattern a -> Pattern a)
pInt_p_p_p = (parser :: ExpParser (Pattern Int -> Pattern Int -> Pattern a -> Pattern a -> Pattern a)) <*> parser

instance Parse (Pattern Int -> Pattern Double -> ControlPattern -> ControlPattern) where
  parser = $(fromTidal "striate'")

instance Parse (Pattern Double -> Pattern Time -> ControlPattern -> ControlPattern) where
  parser = (parser :: ExpParser (Pattern Integer -> Pattern Double -> Pattern Time -> ControlPattern -> ControlPattern)) <*> parser

instance Parse (Pattern Time -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  parser = $(fromTidal "off")

instance Parse (Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  parser =
    $(fromTidal "every") <|>
    (parser :: ExpParser (Pattern Int -> Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)) <*> parser

instance Parse (Pattern Double -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  parser = $(fromTidal "sometimesBy")

instance Parse ([Int] -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  parser = $(fromTidal "foldEvery")

instance Parse ((Time,Time) -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  parser = $(fromTidal "within")

instance Parse (Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  parser =
    $(fromTidal "chunk") <|>
    (parser :: ExpParser (Int -> Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)) <*> parser

instance Parse (Double -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  parser = $(fromTidal "someCyclesBy")

instance Parse (Int -> [a] -> Pattern Int -> Pattern a) where
  parser = $(fromTidal "fit")




-- * -> * -> * -> * -> *

instance Parse (Pattern Int -> Pattern Int -> Pattern a -> Pattern a -> Pattern a) where
  parser = $(fromTidal "euclidFull")

instance Parse (Int -> Pattern Int -> Pattern Int -> Pattern a -> Pattern a) where
  parser = (parser :: ExpParser (Pattern Time -> Int -> Pattern Int -> Pattern Int -> Pattern a -> Pattern a)) <*> parser

instance Parse (Pattern Integer -> Pattern Double -> Pattern Time -> ControlPattern -> ControlPattern) where
  parser = $(fromTidal "stut")

instance Parse (Pattern Int -> Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  parser = $(fromTidal "every'")

instance Parse (Int -> Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  parser = $(fromTidal "whenmod")




-- * -> * -> * -> * -> * -> *

instance Parse (Pattern Time -> Int -> Pattern Int -> Pattern Int -> Pattern a -> Pattern a) where
  parser = $(fromTidal "fit'")

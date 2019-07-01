{-# LANGUAGE TemplateHaskell, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}

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
    (parser :: ExpParser (ControlPattern -> ControlPattern)) <*> parser <|>
    (parser :: ExpParser (Pattern String -> ControlPattern)) <*> parser <|>
    (parser :: ExpParser (Pattern Double -> ControlPattern)) <*> parser <|>
    (parser :: ExpParser (Pattern Int -> ControlPattern)) <*> parser <|>
    genericPatternExpressions <|>
    unionableMergeOperator <*> parser <*> parser <|>
    numMergeOperator <*> parser <*> parser <|>
    fractionalMergeOperator <*> parser <*> parser

genericPatternExpressions :: forall a. (MiniTidal a, MiniTidal (Pattern a)) => ExpParser (Pattern a)
genericPatternExpressions =
  (parser :: ExpParser (Pattern a -> Pattern a)) <*> parser <|>
  (parser :: ExpParser ([a] -> Pattern a)) <*> parser <|>
  (parser :: ExpParser ([Pattern a] -> Pattern a)) <*> parser <|>
  (parser :: ExpParser (Pattern Int -> Pattern a)) <*> parser <|>
  silence

silence :: ExpParser (Pattern a)
silence = $(fromTidal "silence") -- ie. T.silence <$ reserved "silence", see Sound.Tidal.MiniTidal.TH


instance MiniTidal (Pattern String) where
  parser =
    parseBP <|>
    genericPatternExpressions

parseBP :: (Enumerable a, T.Parseable a) => ExpParser (Pattern a)
parseBP = join ((first show . T.parseBP) <$> string)


instance MiniTidal (Pattern Int) where
  parser =
    (pure . fromIntegral) <$> integer <|>
    parseBP <|>
    genericPatternExpressions <|>
    unionableMergeOperator <*> parser <*> parser <|>
    numMergeOperator <*> parser <*> parser


instance MiniTidal (Pattern Integer) where
  parser =
    pure <$> integer <|>
    parseBP <|>
    genericPatternExpressions <|>
    unionableMergeOperator <*> parser <*> parser <|>
    numMergeOperator <*> parser <*> parser


instance MiniTidal (Pattern Double) where
  parser =
    (pure . fromIntegral) <$> integer <|>
    (pure . realToFrac) <$> rational <|>
    parseBP <|>
    $(fromTidal "rand") <|>
    $(fromTidal "sine") <|>
    $(fromTidal "saw") <|>
    $(fromTidal "isaw") <|>
    $(fromTidal "tri") <|>
    $(fromTidal "square") <|>
    $(fromTidal "cosine") <|>
    genericPatternExpressions <|>
    unionableMergeOperator <*> parser <*> parser <|>
    numMergeOperator <*> parser <*> parser <|>
    realMergeOperator <*> parser <*> parser <|>
    fractionalMergeOperator <*> parser <*> parser


instance MiniTidal (Pattern Time) where
  parser =
    (pure . fromIntegral) <$> integer <|>
    pure <$> rational <|>
    parseBP <|>
    genericPatternExpressions <|>
    unionableMergeOperator <*> parser <*> parser <|>
    numMergeOperator <*> parser <*> parser <|>
    realMergeOperator <*> parser <*> parser <|>
    fractionalMergeOperator <*> parser <*> parser


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


instance (MiniTidal a, MiniTidal (Pattern a)) => MiniTidal (Pattern a -> Pattern a) where
  parser = genericTransformations

genericTransformations :: (MiniTidal a, MiniTidal (Pattern a)) => ExpParser (Pattern a -> Pattern a)
genericTransformations =
  $(fromTidal "brak") <|>
  $(fromTidal "rev") <|>
  $(fromTidal "palindrome") <|>
  $(fromTidal "stretch") <|>
  $(fromTidal "loopFirst") <|>
  $(fromTidal "degrade") <|>
  -- generic transformations with arguments that are a simple (ie. sub-pattern) types, including lists
  (parser :: ExpParser (Time -> Pattern a -> Pattern a)) <*> parser <|>
  (parser :: ExpParser (Int -> Pattern a -> Pattern a)) <*> parser <|>
  (parser :: ExpParser ((Time,Time) -> Pattern a -> Pattern a)) <*> parser <|>
  (parser :: ExpParser ([Time] -> Pattern a -> Pattern a)) <*> parser <|>
  -- generic transformations with arguments that are a concrete Pattern type
  (parser :: ExpParser (Pattern Time -> Pattern a -> Pattern a)) <*> parser <|>
  (parser :: ExpParser (Pattern Int -> Pattern a -> Pattern a)) <*> parser <|>
  (parser :: ExpParser (Pattern String -> Pattern a -> Pattern a)) <*> parser <|>
  (parser :: ExpParser (Pattern Double -> Pattern a -> Pattern a)) <*> parser <|>
  -- generic transformations with arguments that are lists of patterns or transformations
  (parser :: ExpParser ([Pattern Time] -> Pattern a -> Pattern a)) <*> parser <|>
  (parser :: ExpParser ([Pattern Int] -> Pattern a -> Pattern a)) <*> parser <|>
  (parser :: ExpParser ([Pattern String] -> Pattern a -> Pattern a)) <*> parser <|>
  (parser :: ExpParser ([Pattern Double] -> Pattern a -> Pattern a)) <*> parser <|>
  -- generic transformations with generic transformations (or lists of them) as arguments
  (parser :: ExpParser ((Pattern a -> Pattern a) -> Pattern a -> Pattern a)) <*> parser <|>
  (parser :: ExpParser ([Pattern a -> Pattern a] -> Pattern a -> Pattern a)) <*> parser


instance {-# INCOHERENT #-} MiniTidal (Time -> Pattern a -> Pattern a) where
  parser =
    $(fromTidal "rotL") <|>
    $(fromTidal "rotR") <|>
    (parser :: ExpParser (Time -> Time -> Pattern a -> Pattern a)) <*> parser

instance MiniTidal (Time -> Time -> Pattern a -> Pattern a) where
  parser = $(fromTidal "playFor")

instance {-# INCOHERENT #-} MiniTidal (Int -> Pattern a -> Pattern a) where
  parser = $(fromTidal "repeatCycles")

instance {-# INCOHERENT #-} MiniTidal ((Time,Time) -> Pattern a -> Pattern a) where
  parser =
    $(fromTidal "compress") <|>
    $(fromTidal "zoom") <|>
    $(fromTidal "compressTo")

instance MiniTidal (Pattern a -> Pattern a -> Pattern a) where -- *** many binary functions, eg. Num functions, missing from this currently
  parser =
    $(fromTidal "overlay") <|>
    $(fromTidal "append") <|>
    (parser :: ExpParser (Pattern Int -> Pattern a -> Pattern a -> Pattern a)) <*> parser

instance {-# INCOHERENT #-} MiniTidal (Pattern Time -> Pattern a -> Pattern a) where
  parser =
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

instance MiniTidal (Pattern Time -> Pattern Time -> Pattern a -> Pattern a) where
  parser = $(fromTidal "swingBy")

instance {-# INCOHERENT #-} MiniTidal (Pattern Int -> Pattern a -> Pattern a) where
  parser =
    $(fromTidal "iter") <|>
    $(fromTidal "iter'") <|>
    $(fromTidal "ply") <|>
    $(fromTidal "substruct'") <|>
    $(fromTidal "slowstripe") <|>
    $(fromTidal "shuffle") <|>
    $(fromTidal "scramble") <|>
    (parser :: ExpParser (Pattern Int -> Pattern Int -> Pattern a -> Pattern a)) <*> parser

instance MiniTidal (Pattern Int -> Pattern Int -> Pattern a -> Pattern a) where
  parser =
    $(fromTidal "euclid") <|>
    $(fromTidal "euclidInv") <|>
    (parser :: ExpParser (Int -> Pattern Int -> Pattern Int -> Pattern a -> Pattern a)) <*> parser

instance MiniTidal (Int -> Pattern Int -> Pattern Int -> Pattern a -> Pattern a) where
  parser = (parser :: ExpParser (Pattern Time -> Int -> Pattern Int -> Pattern Int -> Pattern a -> Pattern a)) <*> parser

instance MiniTidal (Pattern Time -> Int -> Pattern Int -> Pattern Int -> Pattern a -> Pattern a) where
  parser = $(fromTidal "fit'")

instance {-# INCOHERENT #-} MiniTidal (Pattern String -> Pattern a -> Pattern a) where
  parser = $(fromTidal "substruct")

instance {-# INCOHERENT #-} MiniTidal (Pattern Double -> Pattern a -> Pattern a) where
  parser =
    $(fromTidal "degradeBy") <|>
    $(fromTidal "unDegradeBy") <|>
    (parser :: ExpParser (Int -> Pattern Double -> Pattern a -> Pattern a)) <*> parser

instance MiniTidal (Int -> Pattern Double -> Pattern a -> Pattern a) where
  parser = $(fromTidal "degradeOverBy")

instance MiniTidal ((a -> b -> Pattern c) -> [a] -> b -> Pattern c) where
  parser =
    $(fromTidal "spread") <|>
    $(fromTidal "slowspread") <|>
    $(fromTidal "fastspread")

instance MiniTidal (Pattern Int -> Pattern a -> Pattern a -> Pattern a) where
  parser = (parser :: ExpParser (Pattern Int -> Pattern Int -> Pattern a -> Pattern a -> Pattern a)) <*> parser

instance MiniTidal (Pattern Int -> Pattern Int -> Pattern a -> Pattern a -> Pattern a) where
  parser = $(fromTidal "euclidFull")

{-
instance MiniTidal ((a -> b) -> a -> b) where -- not sure if this instance really gets used (probably just need multiple concrete $ placements ??? )
  parser = $(fromHaskell "$")
-}

instance MiniTidal ([Pattern a -> Pattern a] -> Pattern a -> Pattern a) where
  parser = (parser :: ExpParser (((Pattern a -> Pattern a) -> Pattern a -> Pattern a) -> [Pattern a -> Pattern a] -> Pattern a -> Pattern a)) <*> parser

instance MiniTidal (Pattern b -> Pattern a -> Pattern a) => MiniTidal ([Pattern b] -> Pattern a -> Pattern a) where
  parser = (parser :: ExpParser ((Pattern b -> Pattern a -> Pattern a) -> [Pattern b] -> Pattern a -> Pattern a)) <*> parser

instance {-# INCOHERENT #-} MiniTidal (ControlPattern -> ControlPattern) where
  parser =
    genericTransformations <|>
    (parser :: ExpParser ((ControlPattern -> ControlPattern) -> ControlPattern -> ControlPattern)) <*> parser <|>
    (parser :: ExpParser (Pattern Int -> ControlPattern -> ControlPattern)) <*> parser <|>
    (parser :: ExpParser (Pattern Double -> ControlPattern -> ControlPattern)) <*> parser <|>
    (parser :: ExpParser (Pattern Time -> ControlPattern -> ControlPattern)) <*> parser

instance MiniTidal (Pattern Int -> ControlPattern -> ControlPattern) where
  parser =
    $(fromTidal "chop") <|>
    $(fromTidal "striate")

instance MiniTidal (Pattern Double -> ControlPattern -> ControlPattern) where
  parser = (parser :: ExpParser (Pattern Int -> Pattern Double -> ControlPattern -> ControlPattern)) <*> parser

instance MiniTidal (Pattern Int -> Pattern Double -> ControlPattern -> ControlPattern) where
  parser = $(fromTidal "striate'")

instance MiniTidal (Pattern Time -> ControlPattern -> ControlPattern) where
  parser = (parser :: ExpParser (Pattern Double -> Pattern Time -> ControlPattern -> ControlPattern)) <*> parser

instance MiniTidal (Pattern Double -> Pattern Time -> ControlPattern -> ControlPattern) where
  parser = (parser :: ExpParser (Pattern Integer -> Pattern Double -> Pattern Time -> ControlPattern -> ControlPattern)) <*> parser

instance MiniTidal (Pattern Integer -> Pattern Double -> Pattern Time -> ControlPattern -> ControlPattern) where
  parser = $(fromTidal "stut")

instance {-# INCOHERENT #-} MiniTidal ((Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  parser =
    genericAppliedTransformations <|>
    (parser :: ExpParser (Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)) <*> parser <|>
    (parser :: ExpParser (Pattern Time -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)) <*> parser

instance {-# INCOHERENT #-} MiniTidal ((ControlPattern -> ControlPattern) -> ControlPattern -> ControlPattern) where
  parser =
    genericAppliedTransformations <|>
    parser <*> (parser :: ExpParser (Pattern Int)) <|>
    parser <*> (parser :: ExpParser (Pattern Time)) <|>
    $(fromTidal "jux")

genericAppliedTransformations :: ExpParser ((Pattern a -> Pattern a) -> Pattern a -> Pattern a)
genericAppliedTransformations =
  ($) <$ reserved "$" <|>
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

instance MiniTidal (Pattern Int -> Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  parser = $(fromTidal "every'")

instance MiniTidal (Int -> Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  parser = $(fromTidal "whenmod")

instance {-# INCOHERENT #-} MiniTidal ([Pattern Int] -> Pattern a -> Pattern a) where
  parser = $(fromTidal "distrib")

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

instance {-# INCOHERENT #-} MiniTidal [a] => MiniTidal (Pattern Int -> Pattern a) where
  parser = (parser :: ExpParser ([a] -> Pattern Int -> Pattern a)) <*> parser

instance MiniTidal ([a] -> Pattern Int -> Pattern a) where
  parser = (parser :: ExpParser (Int -> [a] -> Pattern Int -> Pattern a)) <*> parser

instance MiniTidal (Int -> [a] -> Pattern Int -> Pattern a) where
  parser = $(fromTidal "fit")

instance MiniTidal ([Time] -> Pattern a -> Pattern a) where
  parser = $(fromTidal "spaceOut")

unionableMergeOperator :: T.Unionable a => ExpParser (Pattern a -> Pattern a -> Pattern a)
unionableMergeOperator =
  $(fromTidal "#") <|>
  $(fromTidal "|>|") <|>
  $(fromTidal "|>") <|>
  $(fromTidal ">|") <|>
  $(fromTidal "|<|") <|>
  $(fromTidal "|<") <|>
  $(fromTidal "<|")

numMergeOperator :: Num a => ExpParser (Pattern a -> Pattern a -> Pattern a)
numMergeOperator =
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
  $(fromTidal "/|")

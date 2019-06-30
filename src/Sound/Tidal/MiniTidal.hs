{-# LANGUAGE TemplateHaskell, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}

module Sound.Tidal.MiniTidal (miniTidal) where

import           Language.Haskell.Exts
import           Control.Applicative
import           Data.Bifunctor

import           Sound.Tidal.Context (Pattern,ControlMap,ControlPattern,Enumerable,Parseable,Time,Arc,TPat,Stream)
import qualified Sound.Tidal.Context as T
import           Sound.Tidal.MiniTidal.TH
import           Sound.Tidal.MiniTidal.ParseExp


-- This is depended upon by Estuary, and changes to its type will cause problems downstream for Estuary.
miniTidal :: String -> Either String ControlPattern
miniTidal = f . parseExp
  where
    f (ParseOk x) = runParseExp parser x
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
  parser :: ParseExp a

instance MiniTidal Int where
  parser = fromIntegral <$> integer

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
    (parser :: ParseExp (ControlPattern -> ControlPattern)) <*> parser <|>
    (parser :: ParseExp (Pattern String -> ControlPattern)) <*> parser <|>
    (parser :: ParseExp (Pattern Double -> ControlPattern)) <*> parser <|>
    (parser :: ParseExp (Pattern Int -> ControlPattern)) <*> parser <|>
    genericPatternExpressions
    -- mising operators: former unionableMergeOperator, numMergeOperator, fractionalMergeOperator

genericPatternExpressions :: forall a. (MiniTidal a, MiniTidal (Pattern a)) => ParseExp (Pattern a)
genericPatternExpressions =
  (parser :: ParseExp (Pattern a -> Pattern a)) <*> parser <|>
  (parser :: ParseExp ([a] -> Pattern a)) <*> parser <|>
  (parser :: ParseExp ([Pattern a] -> Pattern a)) <*> parser <|>
  (parser :: ParseExp (Pattern Int -> Pattern a)) <*> parser <|>
  silence

silence :: ParseExp (Pattern a)
silence = $(fromTidal "silence") -- ie. T.silence <$ reserved "silence", see Sound.Tidal.MiniTidal.TH


instance MiniTidal (Pattern String) where
  parser =
    parseBP <|>
    genericPatternExpressions

parseBP :: (Enumerable a, T.Parseable a) => ParseExp (Pattern a)
parseBP = join ((first show . T.parseBP) <$> string)


instance MiniTidal (Pattern Int) where
  parser =
    (pure . fromIntegral) <$> integer <|>
    parseBP <|>
    genericPatternExpressions
  -- missing operators (former unionableMergeOperator, numMergeOperator)


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
    genericPatternExpressions
    -- missing operators (former unionableMergeOperator, numMergeOperator, realMergeOperator, fractionalMergeOperator)


instance MiniTidal (Pattern Time) where
  parser =
    (pure . fromIntegral) <$> integer <|>
    pure <$> rational <|>
    parseBP <|>
    genericPatternExpressions
    -- missing operators (former unionableMergeOperator, numMergeOperator, realMergeOperator, fractionalMergeOperator)


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

genericTransformations :: (MiniTidal a, MiniTidal (Pattern a)) => ParseExp (Pattern a -> Pattern a)
genericTransformations =
  $(fromTidal "brak") <|>
  $(fromTidal "rev") <|>
  $(fromTidal "palindrome") <|>
  $(fromTidal "stretch") <|>
  $(fromTidal "loopFirst") <|>
  $(fromTidal "degrade") <|>
  -- generic transformations with arguments that are a simple (ie. sub-pattern) types, including lists
  (parser :: ParseExp (Time -> Pattern a -> Pattern a)) <*> parser <|>
  (parser :: ParseExp (Int -> Pattern a -> Pattern a)) <*> parser <|>
  (parser :: ParseExp ((Time,Time) -> Pattern a -> Pattern a)) <*> parser <|>
  (parser :: ParseExp ([Time] -> Pattern a -> Pattern a)) <*> parser <|>
  -- generic transformations with arguments that are a concrete Pattern type
  (parser :: ParseExp (Pattern Time -> Pattern a -> Pattern a)) <*> parser <|>
  (parser :: ParseExp (Pattern Int -> Pattern a -> Pattern a)) <*> parser <|>
  (parser :: ParseExp (Pattern String -> Pattern a -> Pattern a)) <*> parser <|>
  (parser :: ParseExp (Pattern Double -> Pattern a -> Pattern a)) <*> parser <|>
  -- generic transformations with arguments that are lists of patterns or transformations
  (parser :: ParseExp ([Pattern Time] -> Pattern a -> Pattern a)) <*> parser <|>
  (parser :: ParseExp ([Pattern Int] -> Pattern a -> Pattern a)) <*> parser <|>
  (parser :: ParseExp ([Pattern String] -> Pattern a -> Pattern a)) <*> parser <|>
  (parser :: ParseExp ([Pattern Double] -> Pattern a -> Pattern a)) <*> parser <|>
  -- generic transformations with generic transformations (or lists of them) as arguments
  (parser :: ParseExp ((Pattern a -> Pattern a) -> Pattern a -> Pattern a)) <*> parser <|>
  (parser :: ParseExp ([Pattern a -> Pattern a] -> Pattern a -> Pattern a)) <*> parser


instance {-# INCOHERENT #-} MiniTidal (Time -> Pattern a -> Pattern a) where
  parser =
    $(fromTidal "rotL") <|>
    $(fromTidal "rotR") <|>
    (parser :: ParseExp (Time -> Time -> Pattern a -> Pattern a)) <*> parser

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
    $(fromTidal "append")

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
    (parser :: ParseExp (Pattern Time -> Pattern Time -> Pattern a -> Pattern a)) <*> parser

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
    (parser :: ParseExp (Pattern Int -> Pattern Int -> Pattern a -> Pattern a)) <*> parser

instance MiniTidal (Pattern Int -> Pattern Int -> Pattern a -> Pattern a) where
  parser =
    $(fromTidal "euclid") <|>
    $(fromTidal "euclidInv") <|>
    (parser :: ParseExp (Int -> Pattern Int -> Pattern Int -> Pattern a -> Pattern a)) <*> parser

instance MiniTidal (Int -> Pattern Int -> Pattern Int -> Pattern a -> Pattern a) where
  parser = (parser :: ParseExp (Pattern Time -> Int -> Pattern Int -> Pattern Int -> Pattern a -> Pattern a)) <*> parser

instance MiniTidal (Pattern Time -> Int -> Pattern Int -> Pattern Int -> Pattern a -> Pattern a) where
  parser = $(fromTidal "fit'")

instance {-# INCOHERENT #-} MiniTidal (Pattern String -> Pattern a -> Pattern a) where
  parser = $(fromTidal "substruct")

instance {-# INCOHERENT #-} MiniTidal (Pattern Double -> Pattern a -> Pattern a) where
  parser =
    $(fromTidal "degradeBy") <|>
    $(fromTidal "unDegradeBy") <|>
    (parser :: ParseExp (Int -> Pattern Double -> Pattern a -> Pattern a)) <*> parser

instance MiniTidal (Int -> Pattern Double -> Pattern a -> Pattern a) where
  parser = $(fromTidal "degradeOverBy")

instance MiniTidal ((a -> b -> Pattern c) -> [a] -> b -> Pattern c) where
  parser =
    $(fromTidal "spread") <|>
    $(fromTidal "slowspread") <|>
    $(fromTidal "fastspread")

{- instance MiniTidal ((a -> b) -> a -> b) where -- not sure if this instance really gets used (probably just need multiple concrete $ placements ??? )
  valueByIdentifier "$" = Right ($)
  valueByIdentifier _  = Left "expected (a -> b) -> a -> b" -}

instance MiniTidal ([Pattern a -> Pattern a] -> Pattern a -> Pattern a) where
  parser = (parser :: ParseExp (((Pattern a -> Pattern a) -> Pattern a -> Pattern a) -> [Pattern a -> Pattern a] -> Pattern a -> Pattern a)) <*> parser

instance MiniTidal (Pattern b -> Pattern a -> Pattern a) => MiniTidal ([Pattern b] -> Pattern a -> Pattern a) where
  parser = (parser :: ParseExp ((Pattern b -> Pattern a -> Pattern a) -> [Pattern b] -> Pattern a -> Pattern a)) <*> parser

instance {-# INCOHERENT #-} MiniTidal (ControlPattern -> ControlPattern) where
  parser =
    genericTransformations <|>
    (parser :: ParseExp ((ControlPattern -> ControlPattern) -> ControlPattern -> ControlPattern)) <*> parser

instance {-# INCOHERENT #-} MiniTidal ((Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  parser =
    genericAppliedTransformations <|>
    (parser :: ParseExp (Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)) <*> parser <|>
    (parser :: ParseExp (Pattern Time -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)) <*> parser

instance {-# INCOHERENT #-} MiniTidal ((ControlPattern -> ControlPattern) -> ControlPattern -> ControlPattern) where
  parser =
    genericAppliedTransformations <|>
    parser <*> (parser :: ParseExp (Pattern Int)) <|>
    parser <*> (parser :: ParseExp (Pattern Time)) <|>
    $(fromTidal "jux")

genericAppliedTransformations :: ParseExp ((Pattern a -> Pattern a) -> Pattern a -> Pattern a)
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
  $(fromTidal "someCycles")


{- stuff in this comment block transferred from old MiniTidal not reworked yet
pInt_t_p_p :: MiniTidal a => ParseExp (Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
pInt_t_p_p = choice [
  try $ parens pInt_t_p_p,
  $(function "every"),
  pInt_pInt_t_p_p <*> patternArg
  ]

pDouble_t_p_p :: MiniTidal a => ParseExp (Pattern Double -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
pDouble_t_p_p = $(function "sometimesBy")

lvInt_t_p_p :: MiniTidal a => ParseExp ([Int] -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
lvInt_t_p_p = $(function "foldEvery")

vTime_vTime_p_p :: MiniTidal a => ParseExp (Time -> Time -> Pattern a -> Pattern a)
vTime_vTime_p_p = $(function "playFor")

vTimeTime_t_p_p :: MiniTidal a => ParseExp ((Time,Time) -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
vTimeTime_t_p_p = $(function "within")

vInt_t_p_p :: MiniTidal a => ParseExp (Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
vInt_t_p_p = choice [
  try $ parens vInt_t_p_p,
  $(function "chunk"),
  vInt_vInt_t_p_p <*> literalArg
  ]

vDouble_t_p_p :: MiniTidal a => ParseExp (Double -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
vDouble_t_p_p = $(function "someCyclesBy")
  pInt_t_p_p <*> patternArg,
  pDouble_t_p_p <*> patternArg,
  lvInt_t_p_p <*> listLiteralArg,
  vInt_t_p_p <*> literalArg,
  vDouble_t_p_p <*> literalArg,
  vTimeTime_t_p_p <*> literalArg
  ]
-}


instance MiniTidal (Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  parser = $(fromTidal "every")

instance MiniTidal (Pattern Time -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  parser = $(fromTidal "off")

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
  parser = (parser :: ParseExp ([a] -> Pattern Int -> Pattern a)) <*> parser

instance MiniTidal ([a] -> Pattern Int -> Pattern a) where
  parser = (parser :: ParseExp (Int -> [a] -> Pattern Int -> Pattern a)) <*> parser

instance MiniTidal (Int -> [a] -> Pattern Int -> Pattern a) where
  parser = $(fromTidal "fit")

instance MiniTidal ([Time] -> Pattern a -> Pattern a) where
  parser = $(fromTidal "spaceOut")

unionableMergeOperator :: T.Unionable a => ParseExp (Pattern a -> Pattern a -> Pattern a)
unionableMergeOperator =
  $(fromTidal "#") <|>
  $(fromTidal "|>|") <|>
  $(fromTidal "|>") <|>
  $(fromTidal ">|") <|>
  $(fromTidal "|<|") <|>
  $(fromTidal "|<") <|>
  $(fromTidal "<|")

numMergeOperator :: Num a => ParseExp (Pattern a -> Pattern a -> Pattern a)
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

realMergeOperator :: Real a => ParseExp (Pattern a -> Pattern a -> Pattern a)
realMergeOperator =
  $(fromTidal "|%|") <|>
  $(fromTidal "|%") <|>
  $(fromTidal "%|")

fractionalMergeOperator :: Fractional a => ParseExp (Pattern a -> Pattern a -> Pattern a)
fractionalMergeOperator =
  $(fromTidal "|/|") <|>
  $(fromTidal "|/") <|>
  $(fromTidal "/|")

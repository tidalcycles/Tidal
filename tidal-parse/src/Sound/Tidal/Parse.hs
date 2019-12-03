{-# LANGUAGE TemplateHaskell, FlexibleInstances, FlexibleContexts, ScopedTypeVariables, UndecidableInstances #-}

module Sound.Tidal.Parse (parseTidal) where

import           Language.Haskell.Exts
import           Language.Haskellish as Haskellish
import           Control.Applicative
import           Data.Bifunctor
import           Control.Monad
import           Data.Either

import           Sound.Tidal.Context (Pattern,ControlMap,ControlPattern,Enumerable,Time)
import qualified Sound.Tidal.Context as T
import           Sound.Tidal.Parse.TH


-- This is depended upon by Estuary, and changes to its type will cause problems downstream for Estuary.
parseTidal :: String -> Either String ControlPattern
parseTidal = f . parseExp
  where
    f (ParseOk x) = runHaskellish parser x
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
  parser :: Haskellish a

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

instance (Parse a, Parse b) => Parse (a,b) where
  parser = Haskellish.tuple parser parser

instance Parse a => Parse [a] where
  parser = list parser

instance Parse ControlMap where
  parser = empty


instance Parse ControlPattern where
  parser =
    (parser :: Haskellish (Pattern String -> ControlPattern)) <*> parser <|>
    (parser :: Haskellish (Pattern Double -> ControlPattern)) <*> parser <|>
    (parser :: Haskellish (Pattern Int -> ControlPattern)) <*> parser <|>
    genericPatternExpressions


genericPatternExpressions :: forall a. (Parse a, Parse (Pattern a),Parse (Pattern a -> Pattern a)) => Haskellish (Pattern a)
genericPatternExpressions =
  (parser :: Haskellish (Pattern a -> Pattern a)) <*> parser <|>
  (parser :: Haskellish ([a] -> Pattern a)) <*> parser <|>
  (parser :: Haskellish ([Pattern a] -> Pattern a)) <*> parser <|>
  (parser :: Haskellish ([(Pattern a, Double)] -> Pattern a)) <*> parser <|>
  pInt_p <*> parser <|>
  silence

silence :: Haskellish (Pattern a)
silence = $(fromTidal "silence") -- ie. T.silence <$ reserved "silence", see Sound.Tidal.Parse.TH

instance Parse (Pattern Bool) where
  parser =
    parseBP <|>
    (parser :: Haskellish (Pattern String -> Pattern Bool)) <*> parser <|>
    (parser :: Haskellish (Pattern Int -> Pattern Bool)) <*> parser <|>
    genericPatternExpressions

instance Parse (Pattern String) where
  parser =
    parseBP <|>
    genericPatternExpressions <|>
    pInt_pString <*> parser

parseBP :: (Enumerable a, T.Parseable a) => Haskellish (Pattern a)
parseBP = do
  (b,_) <- askSpan
  p <- T.parseBP <$> string
  case p of
    Left e -> Haskellish (\_ -> Left (show e))
    Right p' -> do
      return $ T.withContext (updateContext b) p'
      where
        dx = fst b
        dy = snd b
        updateContext (dx,dy) c@(T.Context {T.contextPosition = poss}) =
          c {T.contextPosition = map (\((bx,by), (ex,ey)) -> ((bx+dx,by+dy),(ex+dx,ey+dy))) poss}

instance Parse (Pattern Int) where
  parser =
    (pure . fromIntegral) <$> integer <|>
    parseBP <|>
    genericPatternExpressions <|>
    irand <*> parser

irand :: Num a => Haskellish (Int -> Pattern a)
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
    $(fromTidal "cosine") <|>
    $(fromTidal "envEq") <|>
    $(fromTidal "envEqR") <|>
    $(fromTidal "envL") <|>
    $(fromTidal "envLR") <|>
    $(fromTidal "perlin")

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
    (parser :: Haskellish (Pattern Int -> ControlPattern -> ControlPattern)) <*> parser <|>
    (parser :: Haskellish (Pattern Double -> ControlPattern -> ControlPattern)) <*> parser <|>
    (parser :: Haskellish (Pattern Time -> ControlPattern -> ControlPattern)) <*> parser

instance Parse (Pattern Bool -> Pattern Bool) where parser = genericTransformations
instance Parse (Pattern String -> Pattern String) where parser = genericTransformations
instance Parse (Pattern Int -> Pattern Int) where
  parser = genericTransformations <|> numTransformations
instance Parse (Pattern Integer -> Pattern Integer) where
  parser = genericTransformations <|> numTransformations
instance Parse (Pattern Time -> Pattern Time) where
  parser = genericTransformations <|> numTransformations
instance Parse (Pattern Double -> Pattern Double) where
  parser = genericTransformations <|> numTransformations <|> floatingTransformations

genericTransformations :: forall a. (Parse (Pattern a), Parse (Pattern a -> Pattern a),Parse (Pattern a -> Pattern a -> Pattern a), Parse ((Pattern a -> Pattern a) -> Pattern a -> Pattern a)) => Haskellish (Pattern a -> Pattern a)
genericTransformations =
    (parser :: Haskellish (Pattern a -> Pattern a -> Pattern a)) <*> parser <|>
    asRightSection (parser :: Haskellish (Pattern a -> Pattern a -> Pattern a)) parser <|>
    (parser :: Haskellish ((Pattern a -> Pattern a) -> Pattern a -> Pattern a)) <*> parser <|>
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
    pBool_p_p <*> parser <|>
    lpInt_p_p <*> parser <|>
    -- more complex possibilities that wouldn't involve overlapped Parse instances
    (parser :: Haskellish (Time -> Pattern a -> Pattern a)) <*> parser <|>
    (parser :: Haskellish (Int -> Pattern a -> Pattern a)) <*> parser <|>
    (parser :: Haskellish ((Time,Time) -> Pattern a -> Pattern a)) <*> parser <|>
    (parser :: Haskellish ([Time] -> Pattern a -> Pattern a)) <*> parser <|>
    (parser :: Haskellish ([Pattern Time] -> Pattern a -> Pattern a)) <*> parser <|>
    (parser :: Haskellish ([Pattern String] -> Pattern a -> Pattern a)) <*> parser <|>
    (parser :: Haskellish ([Pattern Double] -> Pattern a -> Pattern a)) <*> parser <|>
    (parser :: Haskellish ([Pattern a -> Pattern a] -> Pattern a -> Pattern a)) <*> parser <|>
    lp_p_p <*> parser

numTransformations :: (Num a, Enum a) => Haskellish (Pattern a -> Pattern a)
numTransformations =
  $(fromTidal "run")

floatingTransformations :: (Floating a, Parse (Pattern a)) => Haskellish (Pattern a -> Pattern a)
floatingTransformations = floatingMergeOperator <*> parser

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

instance Parse ([(Pattern a, Double)] -> Pattern a) where
  parser = $(fromTidal "wrandcat")

pInt_p :: Parse a => Haskellish (Pattern Int -> Pattern a)
pInt_p = (parser :: Haskellish ([a] -> Pattern Int -> Pattern a)) <*> parser

instance Parse (Pattern String -> ControlPattern) where
  parser =
    $(fromTidal "s") <|>
    $(fromTidal "sound") <|>
    $(fromTidal "vowel") <|>
    (parser :: Haskellish (String -> Pattern String -> ControlPattern)) <*> parser

instance Parse (Pattern Int -> ControlPattern) where
  parser =
    $(fromTidal "coarse") <|>
    $(fromTidal "cut") <|>
    (parser :: Haskellish (String -> Pattern Int -> ControlPattern)) <*> parser

instance Parse (Pattern String -> Pattern Bool) where
  parser = $(fromTidal "ascii")

instance Parse (Pattern Int -> Pattern Bool) where
  parser =
    $(fromTidal "binary") <|>
    (parser :: Haskellish (Int -> Pattern Int -> Pattern Bool)) <*> parser

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
    $(fromTidal "legato") <|>
    $(fromTidal "cutoff") <|>
    $(fromTidal "delayfeedback") <|>
    $(fromTidal "delaytime") <|>
    $(fromTidal "delay") <|>
    $(fromTidal "end") <|>
    $(fromTidal "hcutoff") <|>
    $(fromTidal "hresonance") <|>
    $(fromTidal "resonance") <|>
    $(fromTidal "loop") <|>
    $(fromTidal "note") <|>
    (parser :: Haskellish (String -> Pattern Double -> ControlPattern)) <*> parser

pInt_pString :: Haskellish (Pattern Int -> Pattern String)
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

genericBinaryPatternFunctions :: T.Unionable a => Haskellish (Pattern a -> Pattern a -> Pattern a)
genericBinaryPatternFunctions =
  $(fromTidal "overlay") <|>
  $(fromTidal "append") <|>
  $(fromTidal "slowAppend") <|>
  $(fromTidal "slowappend") <|>
  $(fromTidal "fastAppend") <|>
  $(fromTidal "fastappend") <|>
  unionableMergeOperator <|>
  pInt_p_p_p <*> parser <|>
  (parser :: Haskellish (Pattern Bool -> Pattern a -> Pattern a -> Pattern a)) <*> parser <|>
  constParser

unionableMergeOperator :: T.Unionable a => Haskellish (Pattern a -> Pattern a -> Pattern a)
unionableMergeOperator =
  $(fromTidal "#") <|>
  $(fromTidal "|>|") <|>
  $(fromTidal "|>") <|>
  $(fromTidal ">|") <|>
  $(fromTidal "|<|") <|>
  $(fromTidal "|<") <|>
  $(fromTidal "<|")

numMergeOperator :: (Num a, Parse (Pattern a)) => Haskellish (Pattern a -> Pattern a -> Pattern a)
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

realMergeOperator :: Real a => Haskellish (Pattern a -> Pattern a -> Pattern a)
realMergeOperator =
  $(fromTidal "|%|") <|>
  $(fromTidal "|%") <|>
  $(fromTidal "%|")

fractionalMergeOperator :: Fractional a => Haskellish (Pattern a -> Pattern a -> Pattern a)
fractionalMergeOperator =
  $(fromTidal "|/|") <|>
  $(fromTidal "|/") <|>
  $(fromTidal "/|") <|>
  $(fromHaskell "/")

floatingMergeOperator :: Floating a => Haskellish (Pattern a -> Pattern a -> Pattern a)
floatingMergeOperator =
  $(fromTidal "|**") <|>
  $(fromTidal "**|") <|>
  $(fromTidal "|**|")

constParser :: Haskellish (a -> b -> a)
constParser = $(fromHaskell "const")

instance Parse (Time -> Pattern a -> Pattern a) where
  parser =
    $(fromTidal "rotL") <|>
    $(fromTidal "rotR") <|>
    (parser :: Haskellish (Time -> Time -> Pattern a -> Pattern a)) <*> parser

instance Parse (Int -> Pattern a -> Pattern a) where
  parser = $(fromTidal "repeatCycles")

instance Parse (Int -> Pattern Int -> Pattern Bool) where
  parser = $(fromTidal "binaryN")

instance Parse ((Time,Time) -> Pattern a -> Pattern a) where
  parser =
    $(fromTidal "compress") <|>
    $(fromTidal "zoom") <|>
    $(fromTidal "compressTo")

pString_pInt_pString :: Haskellish (Pattern String -> Pattern Int -> Pattern String)
pString_pInt_pString = $(fromTidal "samples")

pTime_p_p :: Haskellish (Pattern Time -> Pattern a -> Pattern a)
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
    (parser :: Haskellish (Pattern Time -> Pattern Time -> Pattern a -> Pattern a)) <*> parser

pInt_p_p :: Haskellish (Pattern Int -> Pattern a -> Pattern a)
pInt_p_p =
    $(fromTidal "iter") <|>
    $(fromTidal "iter'") <|>
    $(fromTidal "ply") <|>
    $(fromTidal "substruct'") <|>
    $(fromTidal "slowstripe") <|>
    $(fromTidal "shuffle") <|>
    $(fromTidal "scramble") <|>
    pInt_pInt_p_p <*> parser

pString_p_p :: Haskellish (Pattern String -> Pattern a -> Pattern a)
pString_p_p = $(fromTidal "substruct")

pDouble_p_p :: Haskellish (Pattern Double -> Pattern a -> Pattern a)
pDouble_p_p =
    $(fromTidal "degradeBy") <|>
    $(fromTidal "unDegradeBy") <|>
    (parser :: Haskellish (Int -> Pattern Double -> Pattern a -> Pattern a)) <*> parser

pBool_p_p :: Haskellish (Pattern Bool -> Pattern a -> Pattern a)
pBool_p_p =
    $(fromTidal "mask") <|>
    $(fromTidal "struct")

instance Parse ((Pattern a -> Pattern a) -> Pattern a -> Pattern a) => Parse ([Pattern a -> Pattern a] -> Pattern a -> Pattern a) where
  parser = (parser :: Haskellish (((Pattern a -> Pattern a) -> Pattern a -> Pattern a) -> [Pattern a -> Pattern a] -> Pattern a -> Pattern a)) <*> parser

lp_p_p :: Parse (Pattern a -> Pattern a -> Pattern a) => Haskellish ([Pattern a] -> Pattern a -> Pattern a)
lp_p_p = (parser :: Haskellish ((Pattern a -> Pattern a -> Pattern a) -> [Pattern a] -> Pattern a -> Pattern a)) <*> parser


instance Parse ([Pattern Double] -> Pattern a -> Pattern a) where
  parser = (parser :: Haskellish ((Pattern Double -> Pattern a -> Pattern a) -> [Pattern Double] -> Pattern a -> Pattern a)) <*> pDouble_p_p
instance Parse ([Pattern Time] -> Pattern a -> Pattern a) where
  parser = (parser :: Haskellish ((Pattern Time -> Pattern a -> Pattern a) -> [Pattern Time] -> Pattern a -> Pattern a)) <*> pTime_p_p
instance Parse ([Pattern String] -> Pattern a -> Pattern a) where
  parser = (parser :: Haskellish ((Pattern String -> Pattern a -> Pattern a) -> [Pattern String] -> Pattern a -> Pattern a)) <*> pString_p_p

lpInt_p_p :: Haskellish ([Pattern Int] -> Pattern a -> Pattern a)
lpInt_p_p =
  $(fromTidal "distrib") <|>
  (parser :: Haskellish ((Pattern Int -> Pattern a -> Pattern a) -> [Pattern Int] -> Pattern a -> Pattern a)) <*> pInt_p_p

instance Parse ([Time] -> Pattern a -> Pattern a) where
  parser = $(fromTidal "spaceOut")

instance Parse (Pattern Int -> ControlPattern -> ControlPattern) where
  parser =
    $(fromTidal "chop") <|>
    $(fromTidal "striate") <|>
    $(fromTidal "gap") <|>
    $(fromTidal "randslice") <|>
    $(fromTidal "spin") <|>
    (parser :: Haskellish (Int -> Pattern Int -> ControlPattern -> ControlPattern)) <*> parser

instance Parse (Pattern Double -> ControlPattern -> ControlPattern) where
  parser = (parser :: Haskellish (Pattern Int -> Pattern Double -> ControlPattern -> ControlPattern)) <*> parser

instance Parse (Pattern Time -> ControlPattern -> ControlPattern) where
  parser =
    $(fromTidal "hurry") <|>
    (parser :: Haskellish (Pattern Double -> Pattern Time -> ControlPattern -> ControlPattern)) <*> parser

instance Parse (Pattern Int -> Pattern Int -> ControlPattern -> ControlPattern) where
  parser = $(fromTidal "slice")

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

genericAppliedTransformations :: Haskellish ((Pattern a -> Pattern a) -> Pattern a -> Pattern a)
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
  (parser :: Haskellish (Pattern Time -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)) <*> parser <|>
  (parser :: Haskellish (Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)) <*> parser <|>
  (parser :: Haskellish (Pattern Double -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)) <*> parser <|>
  (parser :: Haskellish ([Int] -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)) <*> parser <|>
  (parser :: Haskellish ((Time,Time) -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)) <*> parser <|>
  (parser :: Haskellish (Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)) <*> parser <|>
  (parser :: Haskellish (Double -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)) <*> parser <|>
  (parser :: Haskellish (Pattern Bool -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)) <*> parser

instance Parse ([a] -> Pattern Int -> Pattern a) where
  parser = (parser :: Haskellish (Int -> [a] -> Pattern Int -> Pattern a)) <*> parser

instance Parse (String -> Pattern Double -> ControlPattern) where
  parser = $(fromTidal "pF")

instance Parse (String -> Pattern Int -> ControlPattern) where
  parser = $(fromTidal "pI")

instance Parse (String -> Pattern String -> ControlPattern) where
  parser = $(fromTidal "pS")


-- * -> * -> * -> *

numTernaryTransformations :: Num a => Haskellish (Pattern a -> Pattern a -> Pattern a -> Pattern a)
numTernaryTransformations = $(fromTidal "range")

instance Parse (Time -> Time -> Pattern a -> Pattern a) where
  parser = $(fromTidal "playFor")

instance Parse (Pattern Time -> Pattern Time -> Pattern a -> Pattern a) where
  parser = $(fromTidal "swingBy")

instance Parse (Pattern Bool -> Pattern a -> Pattern a -> Pattern a) where
  parser = $(fromTidal "sew")

instance Parse (Pattern Bool -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  parser = $(fromTidal "while")

pInt_pInt_p_p :: Haskellish (Pattern Int -> Pattern Int -> Pattern a -> Pattern a)
pInt_pInt_p_p =
  $(fromTidal "euclid") <|>
  $(fromTidal "euclidInv") <|>
  (parser :: Haskellish (Int -> Pattern Int -> Pattern Int -> Pattern a -> Pattern a)) <*> parser

instance Parse (Int -> Pattern Double -> Pattern a -> Pattern a) where
  parser = $(fromTidal "degradeOverBy")

instance Parse ((a -> b -> Pattern c) -> [a] -> b -> Pattern c) where
  parser =
    $(fromTidal "spread") <|>
    $(fromTidal "slowspread") <|>
    $(fromTidal "fastspread")

pInt_p_p_p :: Haskellish (Pattern Int -> Pattern a -> Pattern a -> Pattern a)
pInt_p_p_p = (parser :: Haskellish (Pattern Int -> Pattern Int -> Pattern a -> Pattern a -> Pattern a)) <*> parser

instance Parse (Pattern Int -> Pattern Double -> ControlPattern -> ControlPattern) where
  parser = $(fromTidal "striate'")

instance Parse (Int -> Pattern Int -> ControlPattern  -> ControlPattern) where
  parser = $(fromTidal "chew")

instance Parse (Pattern Double -> Pattern Time -> ControlPattern -> ControlPattern) where
  parser = (parser :: Haskellish (Pattern Integer -> Pattern Double -> Pattern Time -> ControlPattern -> ControlPattern)) <*> parser

instance Parse (Pattern Time -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  parser =
    $(fromTidal "off") <|>
    $(fromTidal "plyWith")

instance Parse (Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  parser =
    $(fromTidal "every") <|>
    $(fromTidal "plyWith") <|>
    (parser :: Haskellish (Pattern Int -> Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)) <*> parser

instance Parse (Pattern Double -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  parser =
    $(fromTidal "sometimesBy") <|>
    $(fromTidal "plyWith")

instance Parse ([Int] -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  parser = $(fromTidal "foldEvery")

instance Parse ((Time,Time) -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  parser = $(fromTidal "within")

instance Parse (Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  parser =
    $(fromTidal "chunk") <|>
    (parser :: Haskellish (Int -> Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)) <*> parser

instance Parse (Double -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  parser = $(fromTidal "someCyclesBy")

instance Parse (Int -> [a] -> Pattern Int -> Pattern a) where
  parser = $(fromTidal "fit")


-- * -> * -> * -> * -> *

instance Parse (Pattern Int -> Pattern Int -> Pattern a -> Pattern a -> Pattern a) where
  parser = $(fromTidal "euclidFull")

instance Parse (Int -> Pattern Int -> Pattern Int -> Pattern a -> Pattern a) where
  parser = (parser :: Haskellish (Pattern Time -> Int -> Pattern Int -> Pattern Int -> Pattern a -> Pattern a)) <*> parser

instance Parse (Pattern Integer -> Pattern Double -> Pattern Time -> ControlPattern -> ControlPattern) where
  parser = $(fromTidal "stut")

instance Parse (Pattern Int -> Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  parser = $(fromTidal "every'")

instance Parse (Int -> Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  parser = $(fromTidal "whenmod")

-- * -> * -> * -> * -> * -> *

instance Parse (Pattern Time -> Int -> Pattern Int -> Pattern Int -> Pattern a -> Pattern a) where
  parser = $(fromTidal "fit'")

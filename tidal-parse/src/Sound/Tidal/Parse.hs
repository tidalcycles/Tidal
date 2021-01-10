{-# LANGUAGE TemplateHaskell, FlexibleInstances, FlexibleContexts, ScopedTypeVariables, UndecidableInstances #-}

module Sound.Tidal.Parse (parseTidal) where

import           Language.Haskell.Exts
import           Language.Haskellish as Haskellish
import           Control.Applicative
import           Data.Bifunctor
import           Control.Monad.Except

import           Sound.Tidal.Context (Pattern,ControlMap,ControlPattern,Enumerable,Time)
import qualified Sound.Tidal.Context as T
import           Sound.Tidal.Parse.TH

type H = Haskellish ()

-- This is depended upon by Estuary, and changes to its type will cause problems downstream for Estuary.
parseTidal :: String -> Either String ControlPattern
parseTidal = f . parseExp
  where
    f (ParseOk x) = second fst $ runHaskellish parser () x
    f (ParseFailed _ "Parse error: EOF") = return T.silence
    f (ParseFailed l s) = throwError $ show l ++ ": " ++ show s

-- The class Parse is a class for all of the types that we know how to parse.
-- For each type, we provide all the ways we can think of producing that type
-- via expressions in Parse.

class Parse a where
  parser :: H a

instance Parse Bool where
  parser = (True <$ reserved "True") <|> (False <$ reserved "False")

instance Parse Int where
  parser = fromIntegral <$> integer

instance Parse Integer where
  parser = integer

instance Parse Time where
  parser = (fromIntegral <$> integer) <|> rational

instance Parse Double where
  parser = (fromIntegral <$> integer) <|> (realToFrac <$> rational)

instance Parse T.Note where
  parser = (T.Note . fromIntegral <$> integer) <|> (T.Note . realToFrac <$> rational)

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
    (parser :: H (Pattern String -> ControlPattern)) <*> parser <|>
    (parser :: H (Pattern Double -> ControlPattern)) <*> parser <|>
    (parser :: H (Pattern T.Note -> ControlPattern)) <*> parser <|>
    (parser :: H (Pattern Int -> ControlPattern)) <*> parser <|>
    listCp_cp <*> parser <|>
    genericPatternExpressions

genericPatternExpressions :: forall a. (Parse a, Parse (Pattern a),Parse (Pattern a -> Pattern a)) => H (Pattern a)
genericPatternExpressions =
  (parser :: H (Pattern a -> Pattern a)) <*> parser <|>
  (parser :: H ([a] -> Pattern a)) <*> parser <|>
  (parser :: H ([Pattern a] -> Pattern a)) <*> parser <|>
  (parser :: H ([(Pattern a, Double)] -> Pattern a)) <*> parser <|>
  (parser :: H ([Pattern a -> Pattern a] -> Pattern a)) <*> parser <|>
  pInt_p <*> parser <|>
  silence

numPatternExpressions :: (Num a,Parse a) => H (Pattern a)
numPatternExpressions =
  $(fromTidal "irand") <*> parser <|>
  pInt_pNumA <*> parser

fractionalPatternExpressions :: Fractional a => H (Pattern a)
fractionalPatternExpressions =
  $(fromTidal "rand") <|>
  pInt_pFractionalA <*> parser

silence :: H (Pattern a)
silence = $(fromTidal "silence") -- ie. T.silence <$ reserved "silence", see Sound.Tidal.Parse.TH

instance Parse (Pattern Bool) where
  parser =
    parseBP <|>
    (parser :: H (Pattern String -> Pattern Bool)) <*> parser <|>
    (parser :: H (Pattern Int -> Pattern Bool)) <*> parser <|>
    genericPatternExpressions

instance Parse (Pattern String) where
  parser =
    parseBP <|>
    genericPatternExpressions <|>
    (parser :: H (Pattern Int -> Pattern String)) <*> parser

parseBP :: (Enumerable a, T.Parseable a) => H (Pattern a)
parseBP = do
  (b,_) <- Haskellish.span
  p <- T.parseBP <$> string
  case p of
    Left e -> throwError $ show e
    Right p' -> do
      return $ T.withContext (updateContext b) p'
      where
        updateContext (dx,dy) c@T.Context {T.contextPosition = poss} =
          c {T.contextPosition = map (\((bx,by), (ex,ey)) -> ((bx+dx,by+dy),(ex+dx,ey+dy))) poss}

instance Parse (Pattern Int) where
  parser =
    pure . fromIntegral <$> integer <|>
    parseBP <|>
    genericPatternExpressions <|>
    numPatternExpressions

instance Parse (Pattern Integer) where
  parser =
    pure <$> integer <|>
    parseBP <|>
    genericPatternExpressions <|>
    numPatternExpressions

instance Parse (Pattern Double) where
  parser =
    pure . fromIntegral <$> integer <|>
    pure . realToFrac <$> rational <|>
    parseBP <|>
    genericPatternExpressions <|>
    numPatternExpressions <|>
    fractionalPatternExpressions <|>
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

instance Parse (Pattern T.Note) where
  parser =
    pure . fromIntegral <$> integer <|>
    pure . realToFrac <$> rational <|>
    parseBP <|>
    genericPatternExpressions <|>
    numPatternExpressions <|>
    fractionalPatternExpressions

instance Parse (Pattern Time) where
  parser =
    pure . fromIntegral <$> integer <|>
    pure <$> rational <|>
    parseBP <|>
    genericPatternExpressions <|>
    numPatternExpressions <|>
    fractionalPatternExpressions

-- * -> *

instance Parse (ControlPattern -> ControlPattern) where
  parser =
    genericTransformations <|>
    $(fromTidal "ghost") <|>
    (parser :: H (Pattern Int -> ControlPattern -> ControlPattern)) <*> parser <|>
    (parser :: H (Pattern Double -> ControlPattern -> ControlPattern)) <*> parser <|>
    (parser :: H (Pattern Time -> ControlPattern -> ControlPattern)) <*> parser <|>
    lCpCp_cp_cp <*> parser

instance Parse (Pattern Bool -> Pattern Bool) where
  parser = genericTransformations <|> ordTransformations <|> fBool_fBool
instance Parse (Pattern String -> Pattern String) where
  parser = genericTransformations <|> ordTransformations
instance Parse (Pattern Int -> Pattern Int) where
  parser = genericTransformations <|> numTransformations <|> ordTransformations
instance Parse (Pattern Integer -> Pattern Integer) where
  parser = genericTransformations <|> numTransformations <|> ordTransformations
instance Parse (Pattern Time -> Pattern Time) where
  parser = genericTransformations <|> numTransformations <|> ordTransformations
instance Parse (Pattern Double -> Pattern Double) where
  parser = genericTransformations <|> numTransformations <|> floatingTransformations <|> ordTransformations
instance Parse (Pattern T.Note -> Pattern T.Note) where
  parser = genericTransformations <|> numTransformations <|> floatingTransformations <|> ordTransformations

genericTransformations :: forall a. (Parse (Pattern a), Parse (Pattern a -> Pattern a), Parse (Pattern a -> Pattern a -> Pattern a), Parse ((Pattern a -> Pattern a) -> Pattern a -> Pattern a)) => H (Pattern a -> Pattern a)
genericTransformations =
    simpleComposition <|>
    $(fromHaskell "id") <|>
    (parser :: H (Pattern a -> Pattern a -> Pattern a)) <*> parser <|>
    asRightSection (parser :: H (Pattern a -> Pattern a -> Pattern a)) parser <|>
    (parser :: H ((Pattern a -> Pattern a) -> Pattern a -> Pattern a)) <*> parser <|>
    $(fromTidal "brak") <|>
    $(fromTidal "rev") <|>
    $(fromTidal "palindrome") <|>
    $(fromTidal "stretch") <|>
    $(fromTidal "loopFirst") <|>
    $(fromTidal "degrade") <|>
    $(fromTidal "arpeggiate") <|>
    constParser <*> parser <|>
    -- more complex possibilities that would involve overlapped Parse instances if they were instances
    pTime_p_p <*> parser <|>
    pInt_p_p <*> parser <|>
    pDouble_p_p <*> parser <|>
    pBool_p_p <*> parser <|>
    lpInt_p_p <*> parser <|>
    pString_p_p <*> parser <|>
    -- more complex possibilities that wouldn't involve overlapped Parse instances
    (parser :: H (Time -> Pattern a -> Pattern a)) <*> parser <|>
    (parser :: H ((Time,Time) -> Pattern a -> Pattern a)) <*> parser <|>
    (parser :: H ([Time] -> Pattern a -> Pattern a)) <*> parser <|>
    (parser :: H ([Pattern Time] -> Pattern a -> Pattern a)) <*> parser <|>
    (parser :: H ([Pattern Double] -> Pattern a -> Pattern a)) <*> parser <|>
    (parser :: H ([Pattern a -> Pattern a] -> Pattern a -> Pattern a)) <*> parser <|>
    lp_p_p <*> parser <|>
    pA_pB

-- this only matches the case where the functions being composed are both a -> a (with the same a)
-- nonetheless, this is an extremely common case with Tidal
simpleComposition :: forall a. Parse (a -> a) => H (a -> a)
simpleComposition = $(fromHaskell ".") <*> (parser :: H (a -> a)) <*> (parser :: H (a -> a))

numTransformations :: (Num a, Enum a) => H (Pattern a -> Pattern a)
numTransformations =
  $(fromTidal "run")

ordTransformations :: Ord a => H (Pattern a -> Pattern a)
ordTransformations =
  pInt_pOrd_pOrd <*> parser

floatingTransformations :: (Floating a, Parse (Pattern a)) => H (Pattern a -> Pattern a)
floatingTransformations = floatingMergeOperator <*> parser

instance Parse ([a] -> Pattern a) where
  parser =
    $(fromTidal "listToPat") <|>
    $(fromTidal "choose") <|>
    $(fromTidal "cycleChoose")

instance  Parse ([Pattern a] -> Pattern a) where
  parser =
    $(fromTidal "stack") <|>
    $(fromTidal "fastcat") <|> $(fromTidal "fastCat") <|>
    $(fromTidal "slowcat") <|> $(fromTidal "slowCat") <|>
    $(fromTidal "cat") <|>
    $(fromTidal "randcat") <|>
    (parser :: H (Pattern Double -> [Pattern a] -> Pattern a)) <*> parser <|>
    (parser :: H (Pattern Int -> [Pattern a] -> Pattern a)) <*> parser

instance Parse ([(Pattern a, Double)] -> Pattern a) where
  parser = $(fromTidal "wrandcat")

pInt_p :: Parse a => H (Pattern Int -> Pattern a)
pInt_p =
  (parser :: H ([a] -> Pattern Int -> Pattern a)) <*> parser
  -- ??? pA_pB

instance Parse (Pattern String -> ControlPattern) where
  parser =
    $(fromTidal "s") <|>
    $(fromTidal "sound") <|>
    $(fromTidal "vowel") <|>
    (parser :: H (String -> Pattern String -> ControlPattern)) <*> parser <|>
    pA_pB

instance Parse (Pattern Int -> ControlPattern) where
  parser =
    $(fromTidal "cut") <|>
    (parser :: H (String -> Pattern Int -> ControlPattern)) <*> parser <|>
    pA_pB

instance Parse (Pattern String -> Pattern Bool) where
  parser =
    $(fromTidal "ascii") <|>
    pA_pB

instance Parse (Pattern Int -> Pattern Bool) where
  parser =
    $(fromTidal "binary") <|>
    (parser :: H (Pattern Int -> Pattern Int -> Pattern Bool)) <*> parser <|>
    pA_pB

instance Parse (Pattern T.Note -> ControlPattern) where
  parser = $(fromTidal "up") <|>
    $(fromTidal "n") <|>
    $(fromTidal "note") <|>
    (parser :: H (String -> Pattern T.Note -> ControlPattern)) <*> parser <|>
    pA_pB

instance Parse (Pattern Double -> ControlPattern) where
  parser =
    $(fromTidal "speed") <|>
    $(fromTidal "pan") <|>
    $(fromTidal "shape") <|>
    $(fromTidal "gain") <|>
    $(fromTidal "overgain") <|>
    $(fromTidal "overshape") <|>
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
    $(fromTidal "coarse") <|>
    $(fromTidal "nudge") <|>
    (parser :: H (String -> Pattern Double -> ControlPattern)) <*> parser <|>
    pA_pB

instance Parse (Pattern Int -> Pattern String) where
  parser =
    pString_pInt_pString <*> parser <|>
    pA_pB

-- note: missing pA_pB pathway
pInt_pNumA :: (Num a, Parse a) => H (Pattern Int -> Pattern a)
pInt_pNumA = listNumA_pInt_pA <*> parser

-- note: missing pA_pB pathway
pInt_pFractionalA :: Fractional a => H (Pattern Int -> Pattern a)
pInt_pFractionalA = (parser :: Fractional a => H (Pattern String -> Pattern Int -> Pattern a)) <*> parser

listCp_cp :: H ([ControlPattern] -> ControlPattern)
listCp_cp = (parser :: H (ControlPattern -> [ControlPattern] -> ControlPattern)) <*> parser

instance Parse (Pattern a) => Parse ([Pattern a -> Pattern a] -> Pattern a) where
  parser = (parser :: H (Pattern a -> [Pattern a -> Pattern a] -> Pattern a)) <*> parser

pA_pB :: Parse (Pattern a -> Pattern b) => H (Pattern a -> Pattern b)
pA_pB = pAB_pA_pB <*> parser

fBool_fBool :: Functor f => H (f Bool -> f Bool)
fBool_fBool = $(fromTidal "inv")

-- * -> * -> *

instance Parse (Pattern Bool -> Pattern Bool -> Pattern Bool) where
  parser = genericBinaryPatternFunctions

instance Parse (Pattern String -> Pattern String -> Pattern String) where
  parser =
    genericBinaryPatternFunctions

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

instance Parse (Pattern T.Note -> Pattern T.Note -> Pattern T.Note) where
  parser =
    genericBinaryPatternFunctions <|>
    numMergeOperator <|>
    realMergeOperator <|>
    fractionalMergeOperator

instance Parse (ControlPattern -> ControlPattern -> ControlPattern) where
  parser =
    genericBinaryPatternFunctions <|>
    numMergeOperator <|>
    fractionalMergeOperator

genericBinaryPatternFunctions :: T.Unionable a => H (Pattern a -> Pattern a -> Pattern a)
genericBinaryPatternFunctions =
  $(fromTidal "overlay") <|>
  $(fromTidal "append") <|>
  $(fromTidal "slowAppend") <|>
  $(fromTidal "slowappend") <|>
  $(fromTidal "fastAppend") <|>
  $(fromTidal "fastappend") <|>
  unionableMergeOperator <|>
  pInt_p_p_p <*> parser <|>
  (parser :: H (Pattern Bool -> Pattern a -> Pattern a -> Pattern a)) <*> parser <|>
  constParser

unionableMergeOperator :: T.Unionable a => H (Pattern a -> Pattern a -> Pattern a)
unionableMergeOperator =
  $(fromTidal "#") <|>
  $(fromTidal "|>|") <|>
  $(fromTidal "|>") <|>
  $(fromTidal ">|") <|>
  $(fromTidal "|<|") <|>
  $(fromTidal "|<") <|>
  $(fromTidal "<|")

numMergeOperator :: (Num a, Parse (Pattern a)) => H (Pattern a -> Pattern a -> Pattern a)
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

realMergeOperator :: Real a => H (Pattern a -> Pattern a -> Pattern a)
realMergeOperator =
  $(fromTidal "|%|") <|>
  $(fromTidal "|%") <|>
  $(fromTidal "%|")

fractionalMergeOperator :: Fractional a => H (Pattern a -> Pattern a -> Pattern a)
fractionalMergeOperator =
  $(fromTidal "|/|") <|>
  $(fromTidal "|/") <|>
  $(fromTidal "/|") <|>
  $(fromHaskell "/")

floatingMergeOperator :: Floating a => H (Pattern a -> Pattern a -> Pattern a)
floatingMergeOperator =
  $(fromTidal "|**") <|>
  $(fromTidal "**|") <|>
  $(fromTidal "|**|")

constParser :: H (a -> b -> a)
constParser = $(fromHaskell "const")

instance Parse (Time -> Pattern a -> Pattern a) where
  parser =
    $(fromTidal "rotL") <|>
    $(fromTidal "rotR") <|>
    (parser :: H (Time -> Time -> Pattern a -> Pattern a)) <*> parser

instance Parse (Pattern Int -> Pattern Int -> Pattern Bool) where
  parser = $(fromTidal "binaryN")

instance Parse ((Time,Time) -> Pattern a -> Pattern a) where
  parser =
    $(fromTidal "compress") <|>
    $(fromTidal "zoom") <|>
    $(fromTidal "compressTo")

pString_pInt_pString :: H (Pattern String -> Pattern Int -> Pattern String)
pString_pInt_pString = $(fromTidal "samples")

pTime_p_p :: H (Pattern Time -> Pattern a -> Pattern a)
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
    $(fromTidal "ply") <|>
    (parser :: H (Pattern Time -> Pattern Time -> Pattern a -> Pattern a)) <*> parser

pInt_p_p :: H (Pattern Int -> Pattern a -> Pattern a)
pInt_p_p =
    $(fromTidal "iter") <|>
    $(fromTidal "iter'") <|>
    $(fromTidal "substruct'") <|>
    $(fromTidal "slowstripe") <|>
    $(fromTidal "shuffle") <|>
    $(fromTidal "scramble") <|>
    $(fromTidal "repeatCycles") <|>
    int_pInt_p_p <*> parser <|>
    pInt_pInt_p_p <*> parser

pInt_pOrd_pOrd :: Ord a => H (Pattern Int -> Pattern a -> Pattern a)
pInt_pOrd_pOrd = $(fromTidal "rot")

pDouble_p_p :: H (Pattern Double -> Pattern a -> Pattern a)
pDouble_p_p =
    $(fromTidal "degradeBy") <|>
    $(fromTidal "unDegradeBy") <|>
    (parser :: H (Int -> Pattern Double -> Pattern a -> Pattern a)) <*> parser

pBool_p_p :: H (Pattern Bool -> Pattern a -> Pattern a)
pBool_p_p =
    $(fromTidal "mask") <|>
    $(fromTidal "struct") <|>
    $(fromTidal "substruct")

instance Parse ((Pattern a -> Pattern a) -> Pattern a -> Pattern a) => Parse ([Pattern a -> Pattern a] -> Pattern a -> Pattern a) where
  parser =
    (parser :: H (((Pattern a -> Pattern a) -> Pattern a -> Pattern a) -> [Pattern a -> Pattern a] -> Pattern a -> Pattern a)) <*> parser <|>
    (parser :: H (Pattern Int -> [Pattern a -> Pattern a] -> Pattern a -> Pattern a)) <*> parser

lp_p_p :: Parse (Pattern a -> Pattern a -> Pattern a) => H ([Pattern a] -> Pattern a -> Pattern a)
lp_p_p = (parser :: H ((Pattern a -> Pattern a -> Pattern a) -> [Pattern a] -> Pattern a -> Pattern a)) <*> parser

instance Parse ([Pattern Double] -> Pattern a -> Pattern a) where
  parser = (parser :: H ((Pattern Double -> Pattern a -> Pattern a) -> [Pattern Double] -> Pattern a -> Pattern a)) <*> pDouble_p_p

instance Parse ([Pattern Time] -> Pattern a -> Pattern a) where
  parser = (parser :: H ((Pattern Time -> Pattern a -> Pattern a) -> [Pattern Time] -> Pattern a -> Pattern a)) <*> pTime_p_p

lpInt_p_p :: H ([Pattern Int] -> Pattern a -> Pattern a)
lpInt_p_p =
  $(fromTidal "distrib") <|>
  (parser :: H ((Pattern Int -> Pattern a -> Pattern a) -> [Pattern Int] -> Pattern a -> Pattern a)) <*> pInt_p_p

instance Parse ([Time] -> Pattern a -> Pattern a) where
  parser = $(fromTidal "spaceOut")

instance Parse (Pattern Int -> ControlPattern -> ControlPattern) where
  parser =
    $(fromTidal "chop") <|>
    $(fromTidal "striate") <|>
    $(fromTidal "gap") <|>
    $(fromTidal "randslice") <|>
    $(fromTidal "spin") <|>
    (parser :: H (Pattern Int -> Pattern Int -> ControlPattern -> ControlPattern)) <*> parser

instance Parse (Pattern Double -> ControlPattern -> ControlPattern) where
  parser = (parser :: H (Pattern Int -> Pattern Double -> ControlPattern -> ControlPattern)) <*> parser

instance Parse (Pattern Time -> ControlPattern -> ControlPattern) where
  parser =
    $(fromTidal "hurry") <|>
    $(fromTidal "loopAt") <|>
    (parser :: H (Pattern Double -> Pattern Time -> ControlPattern -> ControlPattern)) <*> parser

instance Parse ((ControlPattern -> ControlPattern) -> ControlPattern -> ControlPattern) where
  parser =
    genericAppliedTransformations <|>
    $(fromTidal "jux") <|>
    $(fromTidal "juxcut") <|>
    $(fromTidal "jux4") <|>
    pDouble_controlMapToControlMap_controlMap_controlMap <*> parser

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
instance Parse ((Pattern T.Note -> Pattern T.Note) -> Pattern T.Note -> Pattern T.Note) where
  parser = genericAppliedTransformations

genericAppliedTransformations :: H ((Pattern a -> Pattern a) -> Pattern a -> Pattern a)
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
  (parser :: H (Pattern Time -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)) <*> parser <|>
  (parser :: H (Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)) <*> parser <|>
  (parser :: H (Pattern Double -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)) <*> parser <|>
  (parser :: H ([Int] -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)) <*> parser <|>
  (parser :: H ((Time,Time) -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)) <*> parser <|>
  (parser :: H (Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)) <*> parser <|>
  (parser :: H (Pattern Bool -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)) <*> parser

instance Parse ([a] -> Pattern Int -> Pattern a) where
  parser = (parser :: H (Pattern Int -> [a] -> Pattern Int -> Pattern a)) <*> parser

instance Parse (String -> Pattern Double -> ControlPattern) where
  parser = $(fromTidal "pF")

instance Parse (String -> Pattern T.Note -> ControlPattern) where
  parser = $(fromTidal "pN")

instance Parse (String -> Pattern Int -> ControlPattern) where
  parser = $(fromTidal "pI")

instance Parse (String -> Pattern String -> ControlPattern) where
  parser = $(fromTidal "pS")

instance Parse (Pattern Double -> [Pattern a] -> Pattern a) where
  parser = $(fromTidal "select")

instance Parse (Pattern Int -> [Pattern a] -> Pattern a) where
  parser = $(fromTidal "squeeze")

instance Fractional a => Parse (Pattern String -> Pattern Int -> Pattern a) where
  parser = $(fromTidal "scale")

listNumA_pInt_pA :: Num a => H ([a] -> Pattern Int -> Pattern a)
listNumA_pInt_pA = $(fromTidal "toScale")

pString_p_p :: H (Pattern String -> Pattern a -> Pattern a)
pString_p_p = $(fromTidal "arp")

instance Parse (ControlPattern -> [ControlPattern] -> ControlPattern) where
  parser = (parser :: H (Time -> ControlPattern -> [ControlPattern] -> ControlPattern)) <*> parser

instance Parse (Pattern a -> [Pattern a -> Pattern a] -> Pattern a) where
  parser = (parser :: H (Time -> Pattern a -> [Pattern a -> Pattern a] -> Pattern a)) <*> parser

pAB_pA_pB :: H ((Pattern a -> Pattern b) -> Pattern a -> Pattern b)
pAB_pA_pB = pTime_pAB_pA_pB <*> parser

lCpCp_cp_cp :: H ([ControlPattern -> ControlPattern] -> ControlPattern -> ControlPattern)
lCpCp_cp_cp = $(fromTidal "jux'") <|> $(fromTidal "juxcut'")

-- * -> * -> * -> *

numTernaryTransformations :: Num a => H (Pattern a -> Pattern a -> Pattern a -> Pattern a)
numTernaryTransformations = $(fromTidal "range")

instance Parse (Pattern Int -> Pattern Int -> ControlPattern -> ControlPattern) where
  parser = $(fromTidal "slice") <|>
           $(fromTidal "chew") <|>
           $(fromTidal "splice")

instance Parse (Time -> Time -> Pattern a -> Pattern a) where
  parser = $(fromTidal "playFor")

instance Parse (Pattern Time -> Pattern Time -> Pattern a -> Pattern a) where
  parser = $(fromTidal "swingBy")

instance Parse (Pattern Bool -> Pattern a -> Pattern a -> Pattern a) where
  parser = $(fromTidal "sew")

instance Parse (Pattern Bool -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  parser = $(fromTidal "while")

int_pInt_p_p :: H (Int -> Pattern Int -> Pattern a -> Pattern a)
int_pInt_p_p =
  $(fromTidal "bite")

pInt_pInt_p_p :: H (Pattern Int -> Pattern Int -> Pattern a -> Pattern a)
pInt_pInt_p_p =
  $(fromTidal "euclid") <|>
  $(fromTidal "euclidInv") <|>
  (parser :: H (Int -> Pattern Int -> Pattern Int -> Pattern a -> Pattern a)) <*> parser

instance Parse (Int -> Pattern Double -> Pattern a -> Pattern a) where
  parser = $(fromTidal "degradeOverBy")

instance Parse ((a -> b -> Pattern c) -> [a] -> b -> Pattern c) where
  parser =
    $(fromTidal "spread") <|>
    $(fromTidal "slowspread") <|>
    $(fromTidal "fastspread")

pInt_p_p_p :: H (Pattern Int -> Pattern a -> Pattern a -> Pattern a)
pInt_p_p_p = (parser :: H (Pattern Int -> Pattern Int -> Pattern a -> Pattern a -> Pattern a)) <*> parser

instance Parse (Pattern Int -> Pattern Double -> ControlPattern -> ControlPattern) where
  parser = $(fromTidal "striate'")

instance Parse (Pattern Double -> Pattern Time -> ControlPattern -> ControlPattern) where
  parser = (parser :: H (Pattern Integer -> Pattern Double -> Pattern Time -> ControlPattern -> ControlPattern)) <*> parser

instance Parse (Pattern Time -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  parser =
    $(fromTidal "off") <|>
    $(fromTidal "plyWith")

pTime_pAB_pA_pB :: H (Pattern Time -> (Pattern a -> Pattern b) -> Pattern a -> Pattern b)
pTime_pAB_pA_pB =
  $(fromTidal "inside") <|>
  $(fromTidal "outside")

instance Parse (Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  parser =
    $(fromTidal "every") <|>
    $(fromTidal "plyWith") <|>
    $(fromTidal "chunk") <|>
    (parser :: H (Pattern Int -> Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)) <*> parser

instance Parse (Pattern Double -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  parser =
    $(fromTidal "sometimesBy") <|>
    $(fromTidal "someCyclesBy") <|>
    $(fromTidal "plyWith")

pDouble_controlMapToControlMap_controlMap_controlMap :: H (Pattern Double -> (Pattern ControlMap -> Pattern ControlMap) -> Pattern ControlMap -> Pattern ControlMap)
pDouble_controlMapToControlMap_controlMap_controlMap =
  $(fromTidal "juxBy") <|>
  (parser :: H (Pattern Double -> (Pattern ControlMap -> Pattern ControlMap) -> Pattern ControlMap -> Pattern ControlMap))

instance Parse ([Int] -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  parser = $(fromTidal "foldEvery")

instance Parse ((Time,Time) -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  parser = $(fromTidal "within")

instance Parse (Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  parser = (parser :: H (Int -> Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)) <*> parser

instance Parse (Pattern Int -> [a] -> Pattern Int -> Pattern a) where
  parser = $(fromTidal "fit")

instance Parse (Pattern Int -> [Pattern a -> Pattern a] -> Pattern a -> Pattern a) where
  parser = $(fromTidal "pickF")

instance Parse (Time -> ControlPattern -> [ControlPattern] -> ControlPattern) where
  parser = $(fromTidal "weave")

instance Parse (Time -> Pattern a -> [Pattern a -> Pattern a] -> Pattern a) where
  parser = $(fromTidal "weaveWith")

-- * -> * -> * -> * -> *

instance Parse (Pattern Int -> Pattern Int -> Pattern a -> Pattern a -> Pattern a) where
  parser = $(fromTidal "euclidFull")

instance Parse (Int -> Pattern Int -> Pattern Int -> Pattern a -> Pattern a) where
  parser = (parser :: H (Pattern Time -> Int -> Pattern Int -> Pattern Int -> Pattern a -> Pattern a)) <*> parser

instance Parse (Pattern Integer -> Pattern Double -> Pattern Time -> ControlPattern -> ControlPattern) where
  parser = $(fromTidal "stut")

instance Parse (Pattern Int -> Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  parser = $(fromTidal "every'")

instance Parse (Int -> Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  parser = $(fromTidal "whenmod")

-- * -> * -> * -> * -> * -> *

instance Parse (Pattern Time -> Int -> Pattern Int -> Pattern Int -> Pattern a -> Pattern a) where
  parser = $(fromTidal "fit'")

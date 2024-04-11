{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Sound.Tidal.Parse (parseTidal) where

import           Control.Applicative
import           Control.Monad.Except
import           Data.Bifunctor
import           Data.Char
import           Data.List            (dropWhileEnd)
import qualified Data.Text
import           Language.Haskellish  as Haskellish

import qualified Sound.Tidal.Chords   as T
import           Sound.Tidal.Context  (ControlPattern, Enumerable, Pattern,
                                       Time, ValueMap)
import qualified Sound.Tidal.Context  as T
import           Sound.Tidal.Parse.TH

type H = Haskellish ()

-- This is depended upon by Estuary, and changes to its type will cause problems downstream for Estuary.
parseTidal :: String -> Either String ControlPattern
parseTidal x = if x' == [] then (return T.silence) else r
  where
    x' = dropWhileEnd isSpace $ dropWhile isSpace $ Haskellish.removeComments x
    r = bimap showSyntaxError fst $ Haskellish.parseAndRun parser () x

showSyntaxError :: (Span,Data.Text.Text) -> String
showSyntaxError (((lineNumber,columnNumber),(_,_)),msg) =
  show lineNumber ++ ":" ++ show columnNumber ++ " " ++ Data.Text.unpack msg


-- The class Parse is a class for all of the types that we know how to parse.
-- For each type, we provide all the ways we can think of producing that type
-- via expressions in Parse.

class Parse a where
  parser :: H a

instance Parse Bool where
  parser = (True <$ reserved "True") <|> (False <$ reserved "False") <?> "expected literal Bool"

instance Parse Int where
  parser = (fromIntegral <$> integer) <?> "expected literal Int"

instance Parse Integer where
  parser = integer <?> "expected literal Integer"

instance Parse Time where
  parser = (fromIntegral <$> integer) <|> rational <?> "expected literal Time"

instance Parse Double where
  parser = (fromIntegral <$> integer) <|> (realToFrac <$> rational) <?> "expected literal Double"

instance Parse T.Note where
  parser = (T.Note . fromIntegral <$> integer) <|> (T.Note . realToFrac <$> rational) <?> "expected literal Note"

instance Parse String where
  parser =
    string <|>
    (parser :: H (String -> String)) <*!> parser
    <?> "expected String"

instance (Parse a, Parse b) => Parse (a,b) where
  parser = Haskellish.tuple parser parser


-- Parse instances for lists of things
-- for Num types we define the common pathway numList that includes chords
-- then for other types we default to just parsing "literal" lists [a,a,...]

instance Parse [Int] where parser = numList
instance Parse [Integer] where parser = numList
instance Parse [Time] where parser = numList
instance Parse [Double] where parser = numList
instance Parse [T.Note] where parser = numList
instance {-# OVERLAPPABLE #-} Parse a => Parse [a] where parser = list parser

numList :: (Parse a, Num a) => H [a]
numList = list parser <|> chords

chords :: Num a => H [a]
chords =
  $(fromTidalList ["major","aug","six","sixNine","major7","major9","add9","major11",
  "add11","major13","add13","dom7","dom9","dom11","dom13","sevenFlat5","sevenSharp5",
  "sevenFlat9","nine","eleven","thirteen","minor","diminished","minorSharp5","minor6",
  "minorSixNine","minor7flat5","minor7","minor7sharp5","minor7flat9","minor7sharp9",
  "diminished7","minor9","minor11","minor13","one","five","sus2","sus4","sevenSus2",
  "sevenSus4","nineSus4","sevenFlat10","nineSharp5","minor9sharp5","sevenSharp5flat9",
  "minor7sharp5flat9","elevenSharp","minor11sharp"])


instance Parse ValueMap where
  parser = empty

instance Parse ControlPattern where
  parser =
    (parser :: H (Pattern String -> ControlPattern)) <*!> parser <|>
    (parser :: H (Pattern Double -> ControlPattern)) <*!> parser <|>
    (parser :: H (Pattern T.Note -> ControlPattern)) <*!> parser <|>
    (parser :: H (Pattern Int -> ControlPattern)) <*!> parser <|>
    listCp_cp <*!> parser <|>
    genericPatternExpressions
    <?> "expected ControlPattern"

genericPatternExpressions :: forall a. (Parse a, Parse (Pattern a),Parse (Pattern a -> Pattern a),Parse [a]) => H (Pattern a)
genericPatternExpressions =
  (parser :: H (Pattern a -> Pattern a)) <*!> parser <|>
  (parser :: H ([a] -> Pattern a)) <*!> parser <|>
  (parser :: H ([Pattern a] -> Pattern a)) <*!> parser <|>
  (parser :: H ([(Pattern a, Double)] -> Pattern a)) <*!> parser <|>
  (parser :: H ([Pattern a -> Pattern a] -> Pattern a)) <*!> parser <|>
  (parser :: H ([(Time, Pattern a)] -> Pattern a)) <*!> parser <|>
  pInt_p <*!> parser <|>
  list_p <*!> parser <|>
  tupleADouble_p <*!> parser <|>
  listTupleStringTransformation_p <*!> parser <|>
  silence

listTupleStringTransformation_p :: forall a. (Parse (Pattern a), Parse (Pattern a -> Pattern a)) => H ([(String, Pattern a -> Pattern a)] -> Pattern a)
listTupleStringTransformation_p = listTupleStringPattern_listTupleStringTransformation_p <*!> parser

listTupleStringPattern_listTupleStringTransformation_p :: H ([(String, Pattern a)] -> [(String, Pattern a -> Pattern a)] -> Pattern a)
listTupleStringPattern_listTupleStringTransformation_p = pString_listTupleStringPattern_listTupleStringTransformation_p <*!> parser

pString_listTupleStringPattern_listTupleStringTransformation_p :: H (Pattern String -> [(String, Pattern a)] -> [(String, Pattern a -> Pattern a)] -> Pattern a)
pString_listTupleStringPattern_listTupleStringTransformation_p = time_pString_listTupleStringPattern_listTupleStringTransformation_p <*!> parser

time_pString_listTupleStringPattern_listTupleStringTransformation_p :: H (Time -> Pattern String -> [(String, Pattern a)] -> [(String, Pattern a -> Pattern a)] -> Pattern a)
time_pString_listTupleStringPattern_listTupleStringTransformation_p = $(fromTidal "ur")

numPatternExpressions :: (Num a,Parse a,Parse [a]) => H (Pattern a)
numPatternExpressions =
  $(fromTidal "irand") <*!> parser <|>
  pInt_pNumA <*!> parser

fractionalPatternExpressions :: Fractional a => H (Pattern a)
fractionalPatternExpressions =
  $(fromTidal "rand") <|>
  $(fromTidal "perlin") <|>
  pInt_pFractionalA <*!> parser <|>
  pDouble_pFractionalA <*!> parser

silence :: H (Pattern a)
silence = $(fromTidal "silence") -- ie. T.silence <$ reserved "silence", see Sound.Tidal.Parse.TH

instance Parse (Pattern Bool) where
  parser =
    parseBP <|>
    (parser :: H (Pattern String -> Pattern Bool)) <*!> parser <|>
    (parser :: H (Pattern Int -> Pattern Bool)) <*!> parser <|>
    genericPatternExpressions
    <?> "expected Pattern Bool"

instance Parse (Pattern String) where
  parser =
    parseBP <|>
    genericPatternExpressions <|>
    (parser :: H (Pattern Int -> Pattern String)) <*!> parser <|>
    (parser :: H (String -> Pattern String)) <*!> parser <|>
    (parser :: H ([(String, String)] -> Pattern String)) <*!> parser
    <?> "expected Pattern String"

parseBP :: (Enumerable a, T.Parseable a) => H (Pattern a)
parseBP = do
  (b,_) <- Haskellish.span
  p <- T.parseBP <$> string
  case p of
    Left e -> throwError $ Data.Text.pack $ show e
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
    <?> "expected Pattern Int"

instance Parse (Pattern Integer) where
  parser =
    pure <$> integer <|>
    parseBP <|>
    genericPatternExpressions <|>
    numPatternExpressions
    <?> "expected Pattern Integer"

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
    (parser :: H (String -> Pattern Double)) <*!> parser <|>
    $(fromTidalList (fmap (\x -> "in" ++ show x) ([0..127] :: [Int])))
    <?> "expected Pattern Double"

instance Parse (Pattern T.Note) where
  parser =
    pure . fromIntegral <$> integer <|>
    pure . realToFrac <$> rational <|>
    parseBP <|>
    genericPatternExpressions <|>
    numPatternExpressions <|>
    fractionalPatternExpressions
    <?> "expected Pattern Note"

instance Parse (Pattern Time) where
  parser =
    pure . fromIntegral <$> integer <|>
    pure <$> rational <|>
    parseBP <|>
    genericPatternExpressions <|>
    numPatternExpressions <|>
    fractionalPatternExpressions
    <?> "expected Pattern Time"

-- * -> *

a_patternB :: forall a b. Parse (a -> Pattern b) => H (a -> Pattern b)
a_patternB = listAtoPatternB_a_patternB <*> parser <?> "expected a -> Pattern b"

listAtoPatternB_a_patternB :: H ([a -> Pattern b] -> a -> Pattern b)
listAtoPatternB_a_patternB =
  $(fromTidal "layer") <|>
  $(fromTidal "spreadf")

{- -- a_patternB2 :: (Parse (a -> b -> Pattern c),Parse [a]) => H (b -> Pattern c)
-- a_patternB2 = return id
--listA_b_patternC <*!> parser <?> "expected a -> Pattern b"

-- listA_b_patternC :: forall a b c. Parse (a -> b -> Pattern c) => H ([a] -> b -> Pattern c)
listA_b_patternC = (parser :: H ((a -> b -> Pattern c) -> [a] -> b -> Pattern c)) <*!> parser
-}

instance Parse (ControlPattern -> ControlPattern) where
  parser =
    genericTransformations <|>
    $(fromTidal "ghost") <|>
    $(fromTidal "silent") <|>
    (parser :: H (Pattern Int -> ControlPattern -> ControlPattern)) <*!> parser <|>
    (parser :: H (Pattern Double -> ControlPattern -> ControlPattern)) <*!> parser <|>
    (parser :: H (Pattern Time -> ControlPattern -> ControlPattern)) <*!> parser
    -- lCpCp_cp_cp <*!> parser
    <?> "expected ControlPattern -> ControlPattern"

instance Parse (Pattern Bool -> Pattern Bool) where
  parser = genericTransformations <|> ordTransformations <|> fBool_fBool <?> "expected Pattern Bool -> Pattern Bool"
instance Parse (Pattern String -> Pattern String) where
  parser = genericTransformations <|> ordTransformations <?> "expected Pattern String -> Pattern String"
instance Parse (Pattern Int -> Pattern Int) where
  parser = genericTransformations <|> numTransformations <|> ordTransformations <?> "expected Pattern Int -> Pattern Int"
instance Parse (Pattern Integer -> Pattern Integer) where
  parser = genericTransformations <|> numTransformations <|> ordTransformations <?> "expected Pattern Integer -> Pattern Integer"
instance Parse (Pattern Time -> Pattern Time) where
  parser = genericTransformations <|> numTransformations <|> ordTransformations <|> realFracTransformations <?> "expected Pattern Time -> Pattern Time"
instance Parse (Pattern Double -> Pattern Double) where
  parser =
    genericTransformations <|>
    numTransformations <|>
    floatingTransformations <|>
    ordTransformations <|>
    realFracTransformations <|>
    $(fromTidal "perlin2")
    <?> "expected Pattern Double -> Pattern Double"
instance Parse (Pattern T.Note -> Pattern T.Note) where
  parser = genericTransformations <|> numTransformations <|> floatingTransformations <|> ordTransformations <?> "expected Pattern Note -> Pattern Note"

genericTransformations :: forall a. (Parse (Pattern a), Parse (Pattern a -> Pattern a), Parse (Pattern a -> Pattern a -> Pattern a), Parse ((Pattern a -> Pattern a) -> Pattern a -> Pattern a), Parse ([Pattern a -> Pattern a] -> Pattern a -> Pattern a)) => H (Pattern a -> Pattern a)
genericTransformations =
    simpleComposition <|>
    $(fromHaskell "id") <|>
    (parser :: H (Pattern a -> Pattern a -> Pattern a)) <*!> parser <|>
    asRightSection (parser :: H (Pattern a -> Pattern a -> Pattern a)) (required parser) <|>
    (parser :: H ((Pattern a -> Pattern a) -> Pattern a -> Pattern a)) <*!> parser <|>
    $(fromTidal "brak") <|>
    $(fromTidal "rev") <|>
    $(fromTidal "palindrome") <|>
    $(fromTidal "stretch") <|>
    $(fromTidal "loopFirst") <|>
    $(fromTidal "degrade") <|>
    $(fromTidal "arpeggiate") <|>
    $(fromTidal "trigger") <|>
    constParser <*!> parser <|>
    -- more complex possibilities that would involve overlapped Parse instances if they were instances
    pTime_p_p <*!> parser <|>
    pInt_p_p <*!> parser <|>
    pDouble_p_p <*!> parser <|>
    pBool_p_p <*!> parser <|>
    lpInt_p_p <*!> parser <|>
    pString_p_p <*!> parser <|>
    -- more complex possibilities that wouldn't involve overlapped Parse instances
    (parser :: H (Time -> Pattern a -> Pattern a)) <*!> parser <|>
    (parser :: H ((Time,Time) -> Pattern a -> Pattern a)) <*!> parser <|>
    (parser :: H ([Time] -> Pattern a -> Pattern a)) <*!> parser <|>
    (parser :: H ([Pattern Time] -> Pattern a -> Pattern a)) <*!> parser <|>
    (parser :: H ([Pattern Double] -> Pattern a -> Pattern a)) <*!> parser <|>
    (parser :: H ([Pattern a -> Pattern a] -> Pattern a -> Pattern a)) <*!> parser <|>
    lp_p_p <*!> parser <|>
    a_patternB <|>
    pA_pB

-- this only matches the case where the functions being composed are both a -> a (with the same a)
-- nonetheless, this is an extremely common case with Tidal
simpleComposition :: forall a. Parse (a -> a) => H (a -> a)
simpleComposition = $(fromHaskell ".") <*!> (parser :: H (a -> a)) <*!> (parser :: H (a -> a))

numTransformations :: (Num a, Enum a) => H (Pattern a -> Pattern a)
numTransformations =
  $(fromTidal "run")

ordTransformations :: Ord a => H (Pattern a -> Pattern a)
ordTransformations =
  pInt_pOrd_pOrd <*!> parser

floatingTransformations :: (Floating a, Parse a, Parse (Pattern a)) => H (Pattern a -> Pattern a)
floatingTransformations =
  floatingMergeOperator <*!> parser <|>
  floating_pFloating_pFloating <*!> parser

realFracTransformations :: (RealFrac a, Parse a) => H (Pattern a -> Pattern a)
realFracTransformations = realFrac_pRealFrac_pRealFrac <*!> parser

instance Parse ([a] -> Pattern a) where
  parser =
    $(fromTidal "listToPat") <|>
    $(fromTidal "choose") <|>
    $(fromTidal "cycleChoose") <|>
    a_patternB

instance  Parse ([Pattern a] -> Pattern a) where
  parser =
    $(fromTidal "stack") <|>
    $(fromTidal "fastcat") <|> $(fromTidal "fastCat") <|>
    $(fromTidal "slowcat") <|> $(fromTidal "slowCat") <|>
    $(fromTidal "cat") <|>
    $(fromTidal "randcat") <|>
    (parser :: H (Pattern Double -> [Pattern a] -> Pattern a)) <*!> parser <|>
    (parser :: H (Pattern Int -> [Pattern a] -> Pattern a)) <*!> parser <|>
    a_patternB

instance Parse ([(Pattern a, Double)] -> Pattern a) where
  parser =
    $(fromTidal "wrandcat") <|>
    a_patternB

pInt_p :: Parse [a] => H (Pattern Int -> Pattern a)
pInt_p =
  (parser :: H ([a] -> Pattern Int -> Pattern a)) <*!> parser
  -- ??? a_patternB -- also missing from all non-instance entries in this section
  -- ??? pA_pB

instance Parse (Pattern String -> ControlPattern) where
  parser =
    $(fromTidal "s") <|>
    $(fromTidal "sound") <|>
    $(fromTidal "vowel") <|>
    (parser :: H (String -> Pattern String -> ControlPattern)) <*!> parser <|>
    pA_pB <|>
    a_patternB

instance Parse (Pattern Int -> ControlPattern) where
  parser =
    $(fromTidal "cut") <|>
    (parser :: H (String -> Pattern Int -> ControlPattern)) <*!> parser <|>
    pA_pB <|>
    a_patternB

instance Parse (Pattern String -> Pattern Bool) where
  parser =
    $(fromTidal "ascii") <|>
    pA_pB <|>
    a_patternB

instance Parse (Pattern Int -> Pattern Bool) where
  parser =
    $(fromTidal "binary") <|>
    (parser :: H (Pattern Int -> Pattern Int -> Pattern Bool)) <*!> parser <|>
    pA_pB <|>
    a_patternB

instance Parse (Pattern T.Note -> ControlPattern) where
  parser = $(fromTidal "up") <|>
    $(fromTidal "n") <|>
    $(fromTidal "note") <|>
    (parser :: H (String -> Pattern T.Note -> ControlPattern)) <*!> parser <|>
    pA_pB <|>
    a_patternB

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
    (parser :: H (String -> Pattern Double -> ControlPattern)) <*!> parser <|>
    pA_pB <|>
    a_patternB

instance Parse (Pattern Int -> Pattern String) where
  parser =
    pString_pInt_pString <*!> parser <|>
    pA_pB <|>
    a_patternB

instance Parse (String -> Pattern String) where
  parser =
    (parser :: H (String -> String -> Pattern String)) <*> parser <|>
    (parser :: H ([String] -> String -> Pattern String )) <*> parser

instance Parse ([(String, String)] -> Pattern String) where
  parser = $(fromTidal "sseqs")

instance Parse (String -> String) where
  parser = (parser :: H (String -> String -> String)) <*!> parser

-- note: missing pA_pB and a_patternB pathways
pInt_pNumA :: (Num a, Parse [a]) => H (Pattern Int -> Pattern a)
pInt_pNumA = listNumA_pInt_pA <*!> parser

-- note: missing pA_pB and a_patternB pathways
pInt_pFractionalA :: Fractional a => H (Pattern Int -> Pattern a)
pInt_pFractionalA = (parser :: Fractional a => H (Pattern String -> Pattern Int -> Pattern a)) <*!> parser

pDouble_pFractionalA :: Fractional a => H (Pattern Double -> Pattern a)
pDouble_pFractionalA = $(fromTidal "perlinWith")

-- note: mising a_patternB pathway
listCp_cp :: H ([ControlPattern] -> ControlPattern)
listCp_cp = (parser :: H (ControlPattern -> [ControlPattern] -> ControlPattern)) <*!> parser

instance Parse (Pattern a) => Parse ([Pattern a -> Pattern a] -> Pattern a) where
  parser =
    (parser :: H (Pattern a -> [Pattern a -> Pattern a] -> Pattern a)) <*!> parser <|>
    a_patternB

-- note: mising a_patternB pathway (? maybe not necessary here though ?)
pA_pB :: Parse (Pattern a -> Pattern b) => H (Pattern a -> Pattern b)
pA_pB = pAB_pA_pB <*!> parser

fBool_fBool :: Functor f => H (f Bool -> f Bool)
fBool_fBool = $(fromTidal "inv")

-- note: mising a_patternB pathway
list_p :: Parse a => H ([a] -> Pattern a)
list_p = pDouble_list_p <*!> parser

-- note: mising a_patternB pathway
tupleADouble_p :: Parse a => H ([(a,Double)] -> Pattern a)
tupleADouble_p =
  $(fromTidal "wchoose") <|>
  pDouble_tupleADouble_p <*!> parser

instance Parse ([(Time, Pattern a)] -> Pattern a) where
  parser = $(fromTidal "timeCat") <|> $(fromTidal "timecat")

instance Parse (String -> Pattern Double) where
  parser =
    $(fromTidal "cF_") <|>
    $(fromTidal "cF0") <|>
    (parser :: H (Double -> String -> Pattern Double)) <*!> parser


-- * -> * -> *

instance Parse (Pattern Bool -> Pattern Bool -> Pattern Bool) where
  parser = genericBinaryPatternFunctions <?> "expected Pattern Bool -> Pattern Bool -> Pattern Bool"

instance Parse (Pattern String -> Pattern String -> Pattern String) where
  parser =
    genericBinaryPatternFunctions <?> "expected Pattern String -> Pattern String -> Pattern String"

instance Parse (Pattern Int -> Pattern Int -> Pattern Int) where
  parser =
    genericBinaryPatternFunctions <|>
    numMergeOperator <|>
    pInt_p_p
    <?> "expected Pattern Int -> Pattern Int -> Pattern Int"

instance Parse (Pattern Integer -> Pattern Integer -> Pattern Integer) where
  parser =
    genericBinaryPatternFunctions <|>
    numMergeOperator
    <?> "expected Pattern Integer -> Pattern Integer -> Pattern Integer"

instance Parse (Pattern Time -> Pattern Time -> Pattern Time) where
  parser =
    genericBinaryPatternFunctions <|>
    numMergeOperator <|>
    realMergeOperator <|>
    fractionalMergeOperator <|>
    pTime_p_p
    <?> "expected Pattern Time -> Pattern Time -> Pattern Time"

instance Parse (Pattern Double -> Pattern Double -> Pattern Double) where
  parser =
    genericBinaryPatternFunctions <|>
    numMergeOperator <|>
    realMergeOperator <|>
    fractionalMergeOperator <|>
    $(fromTidal "perlin2With") <|>
    pDouble_p_p
    <?> "expected Pattern Double -> Pattern Double -> Pattern Double"

instance Parse (Pattern T.Note -> Pattern T.Note -> Pattern T.Note) where
  parser =
    genericBinaryPatternFunctions <|>
    numMergeOperator <|>
    realMergeOperator <|>
    fractionalMergeOperator
    <?> "expected Pattern Note -> Pattern Note -> Pattern Note"

instance Parse (ControlPattern -> ControlPattern -> ControlPattern) where
  parser =
    genericBinaryPatternFunctions <|>
    numMergeOperator <|>
    fractionalMergeOperator <|>
    $(fromTidal "interlace") <|>
    (parser :: H ((ControlPattern -> ControlPattern) -> ControlPattern -> ControlPattern -> ControlPattern)) <*!> parser
    <?> "expected ControlPattern -> ControlPattern -> ControlPattern"

genericBinaryPatternFunctions :: T.Unionable a => H (Pattern a -> Pattern a -> Pattern a)
genericBinaryPatternFunctions =
  $(fromTidal "overlay") <|>
  $(fromTidal "append") <|>
  $(fromTidal "slowAppend") <|>
  $(fromTidal "slowappend") <|>
  $(fromTidal "fastAppend") <|>
  $(fromTidal "fastappend") <|>
  unionableMergeOperator <|>
  pInt_p_p_p <*!> parser <|>
  pBool_p_p_p <*!> parser <|>
  pTime_p_p_p <*!> parser <|>
  (parser :: H (Pattern Bool -> Pattern a -> Pattern a -> Pattern a)) <*!> parser <|>
  constParser

unionableMergeOperator :: T.Unionable a => H (Pattern a -> Pattern a -> Pattern a)
unionableMergeOperator =
  $(fromTidal "#") <|>
  $(fromTidal "|>|") <|>
  $(fromTidal "|>") <|>
  $(fromTidal ">|") <|>
  $(fromTidal "|<|") <|>
  $(fromTidal "|<") <|>
  $(fromTidal "<|") <|>
  $(fromHaskell "<>")

numMergeOperator :: (Num a, Parse (Pattern a)) => H (Pattern a -> Pattern a -> Pattern a)
numMergeOperator =
  numTernaryTransformations <*!> parser <|>
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

realMergeOperator :: T.Moddable a => H (Pattern a -> Pattern a -> Pattern a)
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
    (parser :: H (Time -> Time -> Pattern a -> Pattern a)) <*!> parser <|>
    integral_time_pA_pA <*!> (parser :: H Int) -- we over-specialized this to Int since don't know of case where this wouldn't suffice


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
    (parser :: H (Pattern Time -> Pattern Time -> Pattern a -> Pattern a)) <*!> parser

pInt_p_p :: H (Pattern Int -> Pattern a -> Pattern a)
pInt_p_p =
    $(fromTidal "iter") <|>
    $(fromTidal "iter'") <|>
    $(fromTidal "substruct'") <|>
    $(fromTidal "slowstripe") <|>
    $(fromTidal "shuffle") <|>
    $(fromTidal "scramble") <|>
    $(fromTidal "repeatCycles") <|>
    $(fromTidal "stripe") <|>
    pInt_pInt_p_p <*!> parser

pInt_pOrd_pOrd :: Ord a => H (Pattern Int -> Pattern a -> Pattern a)
pInt_pOrd_pOrd = $(fromTidal "rot")

pDouble_p_p :: H (Pattern Double -> Pattern a -> Pattern a)
pDouble_p_p =
    $(fromTidal "degradeBy") <|>
    $(fromTidal "unDegradeBy") <|>
    (parser :: H (Int -> Pattern Double -> Pattern a -> Pattern a)) <*!> parser

pBool_p_p :: H (Pattern Bool -> Pattern a -> Pattern a)
pBool_p_p =
    $(fromTidal "mask") <|>
    $(fromTidal "struct") <|>
    $(fromTidal "substruct")

instance Parse ([ControlPattern -> ControlPattern] -> ControlPattern -> ControlPattern) where
  parser =
    $(fromTidal "jux'") <|>
    $(fromTidal "juxcut'") <|>
    lPatApatA_patA_patA
-- *** pathway leading to spread(etc) should be incorporated above

instance {-# OVERLAPPABLE #-} Parse ((Pattern a -> Pattern a) -> Pattern a -> Pattern a) => Parse ([Pattern a -> Pattern a] -> Pattern a -> Pattern a) where
  parser = lPatApatA_patA_patA

lPatApatA_patA_patA :: Parse ((Pattern a -> Pattern a) -> Pattern a -> Pattern a) => H ([Pattern a -> Pattern a] -> Pattern a -> Pattern a)
lPatApatA_patA_patA =
  (parser :: H (((Pattern a -> Pattern a) -> Pattern a -> Pattern a) -> [Pattern a -> Pattern a] -> Pattern a -> Pattern a)) <*> parser <|>
  (parser :: H (Pattern Int -> [Pattern a -> Pattern a] -> Pattern a -> Pattern a)) <*> parser <|>
  (parser :: H (Pattern Double -> [Pattern a -> Pattern a] -> Pattern a -> Pattern a)) <*> parser

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
  -- *** pathway leading to spread(etc) should be incorporated here

instance Parse (Pattern Int -> ControlPattern -> ControlPattern) where
  parser =
    $(fromTidal "chop") <|>
    $(fromTidal "striate") <|>
    $(fromTidal "gap") <|>
    $(fromTidal "randslice") <|>
    $(fromTidal "spin") <|>
    (parser :: H (Pattern Int -> Pattern Int -> ControlPattern -> ControlPattern)) <*!> parser

instance Parse (Pattern Double -> ControlPattern -> ControlPattern) where
  parser =
    (parser :: H (Pattern Int -> Pattern Double -> ControlPattern -> ControlPattern)) <*!> parser <|>
    (parser :: H (Pattern Rational -> Pattern Double -> ControlPattern -> ControlPattern)) <*!> parser

instance Parse (Pattern Time -> ControlPattern -> ControlPattern) where
  parser =
    $(fromTidal "hurry") <|>
    $(fromTidal "loopAt") <|>
    (parser :: H (Pattern Double -> Pattern Time -> ControlPattern -> ControlPattern)) <*!> parser

instance Parse ((ControlPattern -> ControlPattern) -> ControlPattern -> ControlPattern) where
  parser =
    genericAppliedTransformations <|>
    $(fromTidal "jux") <|>
    $(fromTidal "juxcut") <|>
    $(fromTidal "jux4") <|>
    pDouble_controlMapToValueMap_controlMap_controlMap <*!> parser

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
  (parser :: H (Pattern Time -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)) <*!> parser <|>
  (parser :: H (Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)) <*!> parser <|>
  (parser :: H (Pattern Double -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)) <*!> parser <|>
  (parser :: H ([Int] -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)) <*!> parser <|>
  (parser :: H ((Time,Time) -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)) <*!> parser <|>
  (parser :: H (Pattern Bool -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)) <*!> parser


instance Parse ([a] -> Pattern Int -> Pattern a) where
  parser = (parser :: H (Pattern Int -> [a] -> Pattern Int -> Pattern a)) <*!> parser
  -- *** pathway leading to spread(etc) should be incorporated here

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
-- *** pathway leading to spread(etc) should be incorporated here

pString_p_p :: H (Pattern String -> Pattern a -> Pattern a)
pString_p_p = $(fromTidal "arp")

instance Parse (ControlPattern -> [ControlPattern] -> ControlPattern) where
  parser = (parser :: H (Time -> ControlPattern -> [ControlPattern] -> ControlPattern)) <*!> parser

instance Parse (Pattern a -> [Pattern a -> Pattern a] -> Pattern a) where
  parser = (parser :: H (Time -> Pattern a -> [Pattern a -> Pattern a] -> Pattern a)) <*!> parser

pAB_pA_pB :: H ((Pattern a -> Pattern b) -> Pattern a -> Pattern b)
pAB_pA_pB = pTime_pAB_pA_pB <*!> parser

pDouble_list_p :: Parse a => H (Pattern Double -> [a] -> Pattern a)
pDouble_list_p = $(fromTidal "chooseBy")

pDouble_tupleADouble_p :: Parse a => H (Pattern Double -> [(a,Double)] -> Pattern a)
pDouble_tupleADouble_p = $(fromTidal "wchooseBy")

instance Parse (String -> String -> Pattern String) where
  parser = $(fromTidal "sseq")

instance Parse ([String] -> String -> Pattern String) where
  parser = $(fromTidal "sseq'")

instance Parse (String -> String -> String) where
  parser = (parser :: H (Int -> String -> String -> String)) <*!> parser

floating_pFloating_pFloating :: (Floating a, Parse a) => H (a -> Pattern a -> Pattern a)
floating_pFloating_pFloating = floating_floating_pFloating_pFloating <*!> parser

realFrac_pRealFrac_pRealFrac :: RealFrac a => H (a -> Pattern a -> Pattern a)
realFrac_pRealFrac_pRealFrac = $(fromTidal "quantise")

instance Parse (Double -> String -> Pattern Double) where
  parser = $(fromTidal "cF")


-- * -> * -> * -> *

numTernaryTransformations :: Num a => H (Pattern a -> Pattern a -> Pattern a -> Pattern a)
numTernaryTransformations = $(fromTidal "range")

instance Parse (Pattern Int -> Pattern Int -> ControlPattern -> ControlPattern) where
  parser = $(fromTidal "slice") <|>
           $(fromTidal "splice") <|>
           $(fromTidal "chew") <|>
           $(fromTidal "bite")

instance Parse (Time -> Time -> Pattern a -> Pattern a) where
  parser = $(fromTidal "playFor")

instance Parse (Pattern Time -> Pattern Time -> Pattern a -> Pattern a) where
  parser = $(fromTidal "swingBy")

pTime_p_p_p :: H (Pattern Time -> Pattern a -> Pattern a -> Pattern a)
pTime_p_p_p = $(fromTidal "wedge")

instance Parse (Pattern Bool -> Pattern a -> Pattern a -> Pattern a) where
  parser = $(fromTidal "sew")

instance Parse (Pattern Bool -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  parser = $(fromTidal "while")

pInt_pInt_p_p :: H (Pattern Int -> Pattern Int -> Pattern a -> Pattern a)
pInt_pInt_p_p =
  $(fromTidal "euclid") <|>
  $(fromTidal "euclidInv") <|>
  (parser :: H (Int -> Pattern Int -> Pattern Int -> Pattern a -> Pattern a)) <*!> parser

instance Parse (Int -> Pattern Double -> Pattern a -> Pattern a) where
  parser = $(fromTidal "degradeOverBy")

instance Parse ((a -> b -> Pattern c) -> [a] -> b -> Pattern c) where
  parser =
    $(fromTidal "spread") <|>
    $(fromTidal "slowspread") <|>
    $(fromTidal "fastspread") <|>
    $(fromTidal "spreadChoose") <|>
    $(fromTidal "spreadr")

pInt_p_p_p :: H (Pattern Int -> Pattern a -> Pattern a -> Pattern a)
pInt_p_p_p = (parser :: H (Pattern Int -> Pattern Int -> Pattern a -> Pattern a -> Pattern a)) <*!> parser

pBool_p_p_p :: H (Pattern Bool -> Pattern a -> Pattern a -> Pattern a)
pBool_p_p_p = $(fromTidal "stitch")

instance Parse (Pattern Int -> Pattern Double -> ControlPattern -> ControlPattern) where
  parser = $(fromTidal "striate'") <|> $(fromTidal "striateBy")

instance Parse (Pattern Double -> Pattern Time -> ControlPattern -> ControlPattern) where
  parser = (parser :: H (Pattern Integer -> Pattern Double -> Pattern Time -> ControlPattern -> ControlPattern)) <*!> parser

instance Parse (Pattern Time -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  parser =
    $(fromTidal "off") <|>
    $(fromTidal "plyWith") <|>
    (parser :: H (Pattern Time -> Pattern Time -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)) <*!> parser <|>
    (parser :: H (Pattern Int -> Pattern Time -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)) <*!> parser


pTime_pAB_pA_pB :: H (Pattern Time -> (Pattern a -> Pattern b) -> Pattern a -> Pattern b)
pTime_pAB_pA_pB =
  $(fromTidal "inside") <|>
  $(fromTidal "outside")

instance Parse (Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  parser =
    $(fromTidal "every") <|>
    $(fromTidal "plyWith") <|>
    $(fromTidal "chunk") <|>
    $(fromTidal "chunk'") <|> -- note: chunk' is actually generalized to Integral, but not clear what non-Int cases would be
    (parser :: H (Pattern Int -> Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)) <*!> parser

instance Parse (Pattern Double -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  parser =
    $(fromTidal "sometimesBy") <|>
    $(fromTidal "someCyclesBy") <|>
    $(fromTidal "plyWith")

pDouble_controlMapToValueMap_controlMap_controlMap :: H (Pattern Double -> (Pattern ValueMap -> Pattern ValueMap) -> Pattern ValueMap -> Pattern ValueMap)
pDouble_controlMapToValueMap_controlMap_controlMap =
  $(fromTidal "juxBy") <|>
  (parser :: H (Pattern Double -> (Pattern ValueMap -> Pattern ValueMap) -> Pattern ValueMap -> Pattern ValueMap))

instance Parse ((ControlPattern -> ControlPattern) -> ControlPattern -> ControlPattern -> ControlPattern) where
  parser =
    $(fromTidal "fix") <|>
    $(fromTidal "unfix") <|>
    (parser :: H ((ControlPattern -> ControlPattern) -> (ControlPattern -> ControlPattern) -> ControlPattern -> ControlPattern -> ControlPattern)) <*!> parser
    <?> "expected (ControlPattern -> ControlPattern) -> ControlPattern -> ControlPattern -> ControlPattern"

instance Parse ([Int] -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  parser = $(fromTidal "foldEvery")

instance Parse ((Time,Time) -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  parser = $(fromTidal "within")

instance Parse (Pattern Int -> [a] -> Pattern Int -> Pattern a) where
  parser = $(fromTidal "fit")

instance Parse (Pattern Int -> [Pattern a -> Pattern a] -> Pattern a -> Pattern a) where
  parser = $(fromTidal "pickF")

instance Parse (Time -> ControlPattern -> [ControlPattern] -> ControlPattern) where
  parser = $(fromTidal "weave")

instance Parse (Time -> Pattern a -> [Pattern a -> Pattern a] -> Pattern a) where
  parser = $(fromTidal "weaveWith")

instance Parse (Pattern Double -> [Pattern a -> Pattern a] -> Pattern a -> Pattern a) where
  parser = $(fromTidal "selectF")

instance Parse (Int -> String -> String -> String) where
  parser = $(fromTidal "lindenmayer")

floating_floating_pFloating_pFloating :: Floating a => H (a -> a -> Pattern a -> Pattern a)
floating_floating_pFloating_pFloating = $(fromTidal "rangex")
-- note: rangex actually generalized to Functor a rather than Pattern a, so we are over-specializing

integral_time_pA_pA :: Integral i => H (i -> Time -> Pattern a -> Pattern a)
integral_time_pA_pA = $(fromTidal "stutter")

instance Parse (Pattern Rational -> Pattern Double -> ControlPattern -> ControlPattern) where
  parser = (parser :: H (Pattern Integer -> Pattern Rational -> Pattern Double -> ControlPattern -> ControlPattern)) <*!> parser

-- * -> * -> * -> * -> *

instance Parse (Pattern Int -> Pattern Int -> Pattern a -> Pattern a -> Pattern a) where
  parser = $(fromTidal "euclidFull")

instance Parse (Int -> Pattern Int -> Pattern Int -> Pattern a -> Pattern a) where
  parser = (parser :: H (Pattern Time -> Int -> Pattern Int -> Pattern Int -> Pattern a -> Pattern a)) <*!> parser

instance Parse (Pattern Integer -> Pattern Double -> Pattern Time -> ControlPattern -> ControlPattern) where
  parser = $(fromTidal "stut")

instance Parse (Pattern Integer -> Pattern Rational -> Pattern Double -> ControlPattern -> ControlPattern) where
  parser = $(fromTidal "echo")

instance Parse (Pattern Int -> Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  parser = $(fromTidal "every'")

instance Parse (Pattern Int -> Pattern Time -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  parser =
    $(fromTidal "stutWith") <|>
    $(fromTidal "echoWith")

instance Parse (Pattern Time -> Pattern Time -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) where
  parser = $(fromTidal "whenmod")

instance Parse ((ControlPattern -> ControlPattern) -> (ControlPattern -> ControlPattern) -> ControlPattern -> ControlPattern -> ControlPattern) where
  parser = $(fromTidal "contrast")



-- * -> * -> * -> * -> * -> *

instance Parse (Pattern Time -> Int -> Pattern Int -> Pattern Int -> Pattern a -> Pattern a) where
  parser = $(fromTidal "fit'")

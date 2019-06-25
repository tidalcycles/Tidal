{-# LANGUAGE FlexibleInstances, TemplateHaskell #-}

module Sound.Tidal.MiniTidal (miniTidal,miniTidalIO,main) where

import           Text.Parsec.Prim (parserZero)
import           Text.ParserCombinators.Parsec
import           Control.Monad (forever)
import           Control.Applicative (liftA2)
-- import           Language.Haskell.TH

import           Sound.Tidal.Context (Pattern,ControlMap,ControlPattern,Enumerable,Parseable,Time,Arc,TPat,Stream)
import qualified Sound.Tidal.Context as T
import           Sound.Tidal.MiniTidal.Token
import           Sound.Tidal.MiniTidal.TH


-- This is depended upon by Estuary, and changes to its type will cause problems downstream for Estuary.
miniTidal :: String -> Either ParseError (Pattern ControlMap)
miniTidal = parse miniTidalParser "miniTidal"

miniTidalParser :: Parser ControlPattern
miniTidalParser = whiteSpace >> choice [
  eof >> return T.silence,
  do
    x <- pattern
    eof
    return x
  ]

class MiniTidal a where
  literal :: Parser a -- parse an individual pure value of this type
  simplePattern :: Parser (Pattern a) -- any way of making this pattern that wouldn't require parentheses if it was an argument
  complexPattern :: Parser (Pattern a) -- producing this pattern by way of unary functions with an argument of a different type
  transformationWithArguments:: Parser (Pattern a -> Pattern a) -- producing this pattern by with unary functions that take same type
  transformationWithoutArguments :: Parser (Pattern a -> Pattern a) -- also producing this pattern by unary functions of same type
  mergeOperator :: Parser (Pattern a -> Pattern a -> Pattern a) -- operators for combining this type of pattern, eg. # or |>
  binaryFunctions :: Parser (a -> a -> a) -- binary functions on pure values of this type, eg. (+) for Int or other Num instances

literalArg :: MiniTidal a => Parser a
literalArg = choice [
  literal,
  nestedParens literal,
  try $ applied $ parensOrNot literal
  ]

listLiteralArg :: MiniTidal a => Parser [a]
listLiteralArg = brackets (commaSep $ parensOrNot literal)

pattern :: MiniTidal a => Parser (Pattern a)
pattern = chainl1 pattern' mergeOperator

pattern' :: MiniTidal a => Parser (Pattern a)
pattern' = choice [
  try $ patternOperators <*> pattern'',
  pattern''
  ]

patternOperators :: Parser (Pattern a -> Pattern a)
patternOperators = choice [
  try $ pattern'' >>= (\x -> reservedOp "<~" >> return ((T.<~) x)),
  pattern'' >>= (\x -> reservedOp "~>" >> return ((T.~>) x))
  ]

pattern'' :: MiniTidal a => Parser (Pattern a)
pattern'' = choice [
  try $ parens pattern,
  transformation <*> patternArg,
  genericComplexPattern,
  complexPattern,
  simplePattern,
  silence
  ]

-- as an argument, a pattern is either a simple pattern (eg. "1 2 3 4") or it is
-- anything which makes such a pattern in parentheses (or on the other side of $)
patternArg :: MiniTidal a => Parser (Pattern a)
patternArg = choice [
  try $ parensOrApplied $ pattern,
  simplePattern
  ]

transformation :: MiniTidal a => Parser (Pattern a -> Pattern a)
transformation = transformationWithArguments <|> transformationWithoutArguments

transformationArg :: MiniTidal a => Parser (Pattern a -> Pattern a)
transformationArg = choice [
  try $ appliedOrNot $ transformationWithoutArguments,
  parensOrApplied $ transformationWithArguments
  ]

listPatternArg :: MiniTidal a => Parser [Pattern a]
listPatternArg = try $ parensOrNot $ brackets (commaSep pattern)

listTransformationArg :: MiniTidal a => Parser [Pattern a -> Pattern a]
listTransformationArg = try $ parensOrNot $ brackets (commaSep transformation)

silence :: Parser (Pattern a)
silence = $(function "silence")

instance MiniTidal ControlMap where
  literal = parserZero
  simplePattern = parserZero
  transformationWithArguments = p_p <|> pControl_pControl
  transformationWithoutArguments = p_p_noArgs
  complexPattern = specificControlPatterns
  mergeOperator = controlPatternMergeOperator
  binaryFunctions = parserZero

controlPatternMergeOperator :: Parser (ControlPattern -> ControlPattern -> ControlPattern)
controlPatternMergeOperator = choice [
  $(op "#"),
  $(op "|>"),
  $(op "<|"),
  $(op "|>"),
  $(op "|<|"),
  $(op "|+|"),
  $(op "|-|"),
  $(op "|*|"),
  $(op "|/|")
  ]

specificControlPatterns :: Parser ControlPattern
specificControlPatterns = choice [
  try $ parens specificControlPatterns,
  $(function "coarse") <*> patternArg,
  $(function "cut") <*> patternArg,
  $(function "n") <*> patternArg,
  $(function "up") <*> patternArg,
  $(function "speed") <*> patternArg,
  $(function "pan") <*> patternArg,
  $(function "shape") <*> patternArg,
  $(function "gain") <*> patternArg,
  $(function "accelerate") <*> patternArg,
  $(function "bandf") <*> patternArg,
  $(function "bandq") <*> patternArg,
  $(function "begin") <*> patternArg,
  $(function "crush") <*> patternArg,
  $(function "cutoff") <*> patternArg,
  $(function "delayfeedback") <*> patternArg,
  $(function "delaytime") <*> patternArg,
  $(function "delay") <*> patternArg,
  $(function "end") <*> patternArg,
  $(function "hcutoff") <*> patternArg,
  $(function "hresonance") <*> patternArg,
  $(function "resonance") <*> patternArg,
  $(function "shape") <*> patternArg,
  $(function "loop") <*> patternArg,
  $(function "s") <*> patternArg,
  $(function "sound") <*> patternArg,
  $(function "vowel") <*> patternArg,
  $(function "unit") <*> patternArg,
  $(function "note") <*> patternArg
  ]

genericComplexPattern :: MiniTidal a => Parser (Pattern a)
genericComplexPattern = choice [
  try $ parens genericComplexPattern,
  lp_p <*> listPatternArg,
  l_p <*> listLiteralArg,
  pInt_p <*> patternArg
  ]

p_p_noArgs :: Parser (Pattern a -> Pattern a)
p_p_noArgs  = choice [
  $(function "brak"),
  $(function "rev"),
  $(function "palindrome"),
  $(function "stretch"),
  $(function "loopFirst"),
  $(function "degrade")
  ]

p_p :: (MiniTidal a, MiniTidal a) => Parser (Pattern a -> Pattern a)
p_p = choice [
  try $ parens p_p,
  p_p_p <*> patternArg,
  t_p_p <*> transformationArg,
  lp_p_p <*> listPatternArg,
  lt_p_p <*> listTransformationArg,
  lpInt_p_p <*> listPatternArg,
  pTime_p_p <*> patternArg,
  pInt_p_p <*> patternArg,
  pString_p_p <*> patternArg,
  pDouble_p_p <*> patternArg,
  vTime_p_p <*> literalArg,
  vInt_p_p <*> literalArg,
  vTimeTime_p_p <*> literalArg,
  pDouble_p_p <*> patternArg,
  lTime_p_p <*> listLiteralArg
  ]

lt_p_p :: MiniTidal a => Parser ([t -> Pattern a] -> t -> Pattern a)
lt_p_p = choice [
  try $ parens lt_p_p,
  spreads <*> (nestedParens $ reservedOp "$" >> return ($))
  ]

l_p :: MiniTidal a => Parser ([a] -> Pattern a)
l_p = choice [
  $(function "listToPat"),
  $(function "choose"),
  $(function "cycleChoose")
  ]

lp_p :: MiniTidal a => Parser ([Pattern a] -> Pattern a)
lp_p = choice [
  $(function "stack"),
  $(function "fastcat"),
  $(function "slowcat"),
  $(function "cat"),
  $(function "randcat")
  ]

pInt_p :: MiniTidal a => Parser (Pattern Int -> Pattern a)
pInt_p = choice [
  try $ parens pInt_p,
  l_pInt_p <*> listLiteralArg
  ]

p_p_p :: MiniTidal a => Parser (Pattern a -> Pattern a -> Pattern a)
p_p_p = choice [
  try $ parens p_p_p,
  liftA2 <$> binaryFunctions,
  $(function "overlay"),
  $(function "append"),
  vTime_p_p_p <*> literalArg,
  pInt_p_p_p <*> patternArg
  ]

pTime_p_p :: MiniTidal a => Parser (Pattern Time -> Pattern a -> Pattern a)
pTime_p_p = choice [
  try $ parens pTime_p_p,
  $(function "fast"),
  $(function "fastGap"),
  $(function "density"),
  $(function "slow"),
  $(function "trunc"),
  $(function "fastGap"),
  $(function "densityGap"),
  $(function "sparsity"),
  $(function "trunc"),
  $(function "linger"),
  $(function "segment"),
  $(function "discretise"),
  $(function "timeLoop"),
  $(function "swing"),
  pTime_pTime_p_p <*> patternArg
  ]

lTime_p_p :: MiniTidal a => Parser ([Time] -> Pattern a -> Pattern a)
lTime_p_p = choice [
  try $ parens lTime_p_p,
  $(function "spaceOut"),
  spreads <*> parens vTime_p_p -- re: spread
  ]

spreads :: MiniTidal a => Parser ((b -> t -> Pattern a) -> [b] -> t -> Pattern a)
spreads = choice [
  $(function "spread"),
  $(function "slowspread"),
  $(function "fastspread")
  ]

pInt_p_p :: MiniTidal a => Parser (Pattern Int -> Pattern a -> Pattern a)
pInt_p_p = choice [
  try $ parens pInt_p_p,
  $(function "iter"),
  $(function "iter'"),
  $(function "ply"),
  $(function "substruct'"),
  $(function "slowstripe"),
  $(function "shuffle"),
  $(function "scramble"),
  pInt_pInt_p_p <*> patternArg
  ]

pString_p_p :: MiniTidal a => Parser (Pattern String -> Pattern a -> Pattern a)
pString_p_p = $(function "substruct")

pDouble_p_p :: MiniTidal a => Parser (Pattern Double -> Pattern a -> Pattern a)
pDouble_p_p = choice [
  try $ parens pDouble_p_p,
  $(function "degradeBy"),
  $(function "unDegradeBy"),
  vInt_pDouble_p_p <*> literalArg
  ]

vTime_p_p :: MiniTidal a => Parser (Time -> Pattern a -> Pattern a)
vTime_p_p = choice [
  try $ parens vTime_p_p,
  $(function "rotL"),
  $(function "rotR"),
  vTime_vTime_p_p <*> literalArg
  ]

vInt_p_p :: MiniTidal a => Parser (Int -> Pattern a -> Pattern a)
vInt_p_p = $(function "repeatCycles")

vTimeTime_p_p :: MiniTidal a => Parser ((Time,Time) -> Pattern a -> Pattern a)
vTimeTime_p_p = choice [
  $(function "compress"),
  $(function "zoom"),
  $(function "compressTo")
  ]

t_p_p :: MiniTidal a => Parser ((Pattern a -> Pattern a) -> Pattern a -> Pattern a)
t_p_p = choice [
  try $ parens t_p_p,
  $(function "sometimes"),
  $(function "often"),
  $(function "rarely"),
  $(function "almostNever"),
  $(function "almostAlways"),
  $(function "never"),
  $(function "always"),
  $(function "superimpose"),
  $(function "someCycles"),
  pInt_t_p_p <*> patternArg,
  pDouble_t_p_p <*> patternArg,
  lvInt_t_p_p <*> listLiteralArg,
  vInt_t_p_p <*> literalArg,
  vDouble_t_p_p <*> literalArg,
  vTimeTime_t_p_p <*> literalArg,
  pTime_t_p_p <*> patternArg
  ]

lpInt_p_p :: MiniTidal a => Parser ([Pattern Int] -> Pattern a -> Pattern a)
lpInt_p_p = $(function "distrib")

lp_p_p :: MiniTidal a => Parser ([Pattern a] -> Pattern a -> Pattern a)
lp_p_p = choice [
  try $ parens lp_p_p,
  try $ spreads <*> parens p_p_p
  ]

l_pInt_p :: MiniTidal a => Parser ([a] -> Pattern Int -> Pattern a)
l_pInt_p = choice [
  try $ parens l_pInt_p,
  vInt_l_pInt_p <*> literalArg
  ]

pTime_t_p_p :: MiniTidal a => Parser (Pattern Time -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
pTime_t_p_p = $(function "off")

vInt_l_pInt_p :: MiniTidal a => Parser (Int -> [a] -> Pattern Int -> Pattern a)
vInt_l_pInt_p = $(function "fit")

vTime_p_p_p :: MiniTidal a => Parser (Time -> Pattern a -> Pattern a -> Pattern a)
vTime_p_p_p = $(function "wedge")

vInt_pDouble_p_p :: MiniTidal a => Parser (Int -> Pattern Double -> Pattern a -> Pattern a)
vInt_pDouble_p_p = $(function "degradeOverBy")

pInt_t_p_p :: MiniTidal a => Parser (Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
pInt_t_p_p = choice [
  try $ parens pInt_t_p_p,
  $(function "every"),
  pInt_pInt_t_p_p <*> patternArg
  ]

pDouble_t_p_p :: MiniTidal a => Parser (Pattern Double -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
pDouble_t_p_p = $(function "sometimesBy")

lvInt_t_p_p :: MiniTidal a => Parser ([Int] -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
lvInt_t_p_p = $(function "foldEvery")

vTime_vTime_p_p :: MiniTidal a => Parser (Time -> Time -> Pattern a -> Pattern a)
vTime_vTime_p_p = $(function "playFor")

vTimeTime_t_p_p :: MiniTidal a => Parser ((Time,Time) -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
vTimeTime_t_p_p = $(function "within")

vInt_t_p_p :: MiniTidal a => Parser (Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
vInt_t_p_p = choice [
  try $ parens vInt_t_p_p,
  $(function "chunk"),
  vInt_vInt_t_p_p <*> literalArg
  ]

vDouble_t_p_p :: MiniTidal a => Parser (Double -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
vDouble_t_p_p = $(function "someCyclesBy")

pInt_pInt_p_p :: MiniTidal a => Parser (Pattern Int -> Pattern Int -> Pattern a -> Pattern a)
pInt_pInt_p_p = choice [
  try $ parens pInt_pInt_p_p,
  $(function "euclid"),
  $(function "euclidInv"),
  vInt_pInt_pInt_p_p <*> literalArg
  ]

pTime_pTime_p_p :: MiniTidal a => Parser (Pattern Time -> Pattern Time -> Pattern a -> Pattern a)
pTime_pTime_p_p = $(function "swingBy")

pInt_pInt_t_p_p :: MiniTidal a => Parser (Pattern Int -> Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
pInt_pInt_t_p_p = $(function "every'")

vInt_vInt_t_p_p :: MiniTidal a => Parser (Int -> Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
vInt_vInt_t_p_p = $(function "whenmod")

pInt_p_p_p :: MiniTidal a => Parser (Pattern Int -> Pattern a -> Pattern a -> Pattern a)
pInt_p_p_p = choice [
  try $ parens pInt_p_p_p,
  pInt_pInt_p_p_p <*> patternArg
  ]

pInt_pInt_p_p_p :: MiniTidal a => Parser (Pattern Int -> Pattern Int -> Pattern a -> Pattern a -> Pattern a)
pInt_pInt_p_p_p = $(function "euclidFull")

vInt_pInt_pInt_p_p :: MiniTidal a => Parser (Int -> Pattern Int -> Pattern Int -> Pattern a -> Pattern a)
vInt_pInt_pInt_p_p = choice [
  try $ parens vInt_pInt_pInt_p_p,
  pTime_vInt_pInt_pInt_p_p <*> patternArg
  ]

pTime_vInt_pInt_pInt_p_p :: MiniTidal a => Parser (Pattern Time -> Int -> Pattern Int -> Pattern Int -> Pattern a -> Pattern a)
pTime_vInt_pInt_pInt_p_p = $(function "fit'")

pControl_pControl :: Parser (ControlPattern -> ControlPattern)
pControl_pControl = choice [
  try $ parens pControl_pControl,
  pInt_pControl_pControl <*> patternArg,
  pDouble_pControl_pControl <*> patternArg,
  pTime_pControl_pControl <*> patternArg,
  tControl_pControl_pControl <*> transformationArg
  ]

tControl_pControl_pControl :: Parser ((ControlPattern -> ControlPattern) -> ControlPattern -> ControlPattern)
tControl_pControl_pControl = $(function "jux")

pInt_pControl_pControl :: Parser (Pattern Int -> ControlPattern -> ControlPattern)
pInt_pControl_pControl = choice [
  $(function "chop"),
  $(function "striate")
  ]

pDouble_pControl_pControl :: Parser (Pattern Double -> ControlPattern -> ControlPattern)
pDouble_pControl_pControl = choice [
  try $ parens pDouble_pControl_pControl,
  pInt_pDouble_pControl_pControl <*> patternArg
  ]

pInt_pDouble_pControl_pControl :: Parser (Pattern Int -> Pattern Double -> ControlPattern -> ControlPattern)
pInt_pDouble_pControl_pControl = $(function "striate'")

pTime_pControl_pControl :: Parser (Pattern Time -> ControlPattern -> ControlPattern)
pTime_pControl_pControl = choice [
  try $ parens pTime_pControl_pControl,
  pDouble_pTime_pControl_pControl <*> patternArg
  ]

pDouble_pTime_pControl_pControl :: Parser (Pattern Double -> Pattern Time -> ControlPattern -> ControlPattern)
pDouble_pTime_pControl_pControl = choice [
  try $ parens pDouble_pTime_pControl_pControl,
  pInteger_pDouble_pTime_pControl_pControl <*> patternArg
  ]

pInteger_pDouble_pTime_pControl_pControl :: Parser (Pattern Integer -> Pattern Double -> Pattern Time -> ControlPattern -> ControlPattern)
pInteger_pDouble_pTime_pControl_pControl = $(function "stut")

simpleDoublePatterns :: Parser (Pattern Double)
simpleDoublePatterns = choice [
  $(function "rand"),
  $(function "sine"),
  $(function "saw"),
  $(function "isaw"),
  $(function "tri"),
  $(function "square"),
  $(function "cosine")
  ]

binaryNumFunctions :: Num a => Parser (a -> a -> a)
binaryNumFunctions = choice [
  try $ parens binaryNumFunctions,
  reservedOp "+" >> return (+),
  reservedOp "-" >> return (-),
  reservedOp "*" >> return (*)
  ]

instance MiniTidal Int where
  literal = int
  simplePattern = parseBP' <|> (pure <$> int)
  transformationWithArguments = p_p_noArgs
  transformationWithoutArguments = p_p
  complexPattern = (atom <*> int) <|> enumComplexPatterns <|> numComplexPatterns <|> intComplexPatterns
  mergeOperator = numMergeOperator
  binaryFunctions = binaryNumFunctions

instance MiniTidal Integer where
  literal = integer
  simplePattern = parseBP' <|> (pure <$> integer)
  transformationWithArguments = p_p_noArgs
  transformationWithoutArguments = p_p
  complexPattern = (atom <*> integer) <|> enumComplexPatterns <|> numComplexPatterns
  mergeOperator = numMergeOperator
  binaryFunctions = binaryNumFunctions

instance MiniTidal Double where
  literal = double
  simplePattern = parseBP' <|> (try $ pure <$> double) <|> simpleDoublePatterns
  transformationWithArguments = p_p_noArgs
  transformationWithoutArguments = p_p
  complexPattern = (atom <*> double) <|> enumComplexPatterns <|> numComplexPatterns
  mergeOperator = numMergeOperator <|> fractionalMergeOperator
  binaryFunctions = binaryNumFunctions

instance MiniTidal Time where
  literal = (toRational <$> double) <|> (fromIntegral <$> integer)
  simplePattern = parseBP' <|> (pure <$> literal)
  transformationWithArguments = p_p_noArgs
  transformationWithoutArguments = p_p
  complexPattern = atom <*> literal <|> numComplexPatterns
  mergeOperator = numMergeOperator <|> fractionalMergeOperator
  binaryFunctions = binaryNumFunctions

instance MiniTidal Arc where
  literal = do
    xs <- parens (commaSep1 literal)
    if length xs == 2 then return (T.Arc (xs!!0) (xs!!1)) else unexpected "Arcs must contain exactly two values"
  simplePattern = pure <$> literal
  transformationWithArguments = p_p_noArgs
  transformationWithoutArguments = p_p
  complexPattern = atom <*> literal
  mergeOperator = parserZero
  binaryFunctions = parserZero

instance MiniTidal (Time,Time) where
  literal = do
    xs <- parens (commaSep1 literal)
    if length xs == 2 then return ((xs!!0),(xs!!1)) else unexpected "(Time,Time) must contain exactly two values"
  simplePattern = pure <$> literal
  transformationWithArguments = p_p_noArgs
  transformationWithoutArguments = p_p
  complexPattern = atom <*> literal
  mergeOperator = parserZero
  binaryFunctions = parserZero

instance MiniTidal String where
  literal = stringLiteral
  simplePattern = parseBP'
  transformationWithArguments = p_p_noArgs
  transformationWithoutArguments = p_p
  complexPattern = atom <*> stringLiteral
  mergeOperator = parserZero
  binaryFunctions = parserZero

fractionalMergeOperator :: Fractional a => Parser (Pattern a -> Pattern a -> Pattern a)
fractionalMergeOperator = opParser "/" >> return (/)

numMergeOperator :: Num a => Parser (Pattern a -> Pattern a -> Pattern a)
numMergeOperator = choice [
  opParser "+" >> return (+),
  opParser "-" >> return (-),
  opParser "*" >> return (*)
  ]

enumComplexPatterns :: (Enum a, Num a, MiniTidal a) => Parser (Pattern a)
enumComplexPatterns = choice [
  $(function "run") <*> patternArg,
  $(function "scan") <*> patternArg
  ]

numComplexPatterns :: (Num a, MiniTidal a) => Parser (Pattern a)
numComplexPatterns = choice [
  $(function "irand") <*> literal,
  $(function "toScale'") <*> literalArg <*> listLiteralArg <*> patternArg,
  $(function "toScale") <*> listLiteralArg <*> patternArg
  ]

intComplexPatterns :: Parser (Pattern Int)
intComplexPatterns = choice [
  $(function "randStruct") <*> literalArg
  ]

atom :: Applicative m => Parser (a -> m a)
atom = (functionParser "pure" <|> functionParser "atom" <|> functionParser "return") >> return (pure)

parseBP' :: (Enumerable a, Parseable a) => Parser (Pattern a)
parseBP' = parseTPat' >>= return . T.toPat

parseTPat' :: Parseable a => Parser (TPat a)
parseTPat' = parseRhythm' T.tPatParser

parseRhythm' :: Parseable a => Parser (TPat a) -> Parser (TPat a)
parseRhythm' f = do
  char '\"' >> whiteSpace
  x <- T.pSequence f'
  char '\"' >> whiteSpace
  return x
  where f' = f
             <|> do _ <- symbol "~" <?> "rest"
                    return T.TPat_Silence

miniTidalIO :: Stream -> String -> Either ParseError (IO ())
miniTidalIO tidal = parse (miniTidalIOParser tidal) "miniTidal"

miniTidalIOParser :: Stream -> Parser (IO ())
miniTidalIOParser tidal = whiteSpace >> choice [
  eof >> return (return ()),
  dParser tidal <*> patternArg
{-  tParser tidal <*> transitionArg tidal <*> patternArg, -}
--  (reserved "setcps" >> return (T.streamOnce tidal True . T.cps)) <*> literalArg
  ]

dParser :: Stream -> Parser (ControlPattern -> IO ())
dParser tidal = choice [
  reserved "d1" >> return (T.streamReplace tidal "1"),
  reserved "d2" >> return (T.streamReplace tidal "2"),
  reserved "d3" >> return (T.streamReplace tidal "3"),
  reserved "d4" >> return (T.streamReplace tidal "4"),
  reserved "d5" >> return (T.streamReplace tidal "5"),
  reserved "d6" >> return (T.streamReplace tidal "6"),
  reserved "d7" >> return (T.streamReplace tidal "7"),
  reserved "d8" >> return (T.streamReplace tidal "8"),
  reserved "d9" >> return (T.streamReplace tidal "9"),
  reserved "d10" >> return (T.streamReplace tidal "10"),
  reserved "d11" >> return (T.streamReplace tidal "11"),
  reserved "d12" >> return (T.streamReplace tidal "12"),
  reserved "d13" >> return (T.streamReplace tidal "13"),
  reserved "d14" >> return (T.streamReplace tidal "14"),
  reserved "d15" >> return (T.streamReplace tidal "15"),
  reserved "d16" >> return (T.streamReplace tidal "16")
  ]

{- tParser :: Stream -> Parser ((Time -> [ControlPattern] -> ControlPattern) -> ControlPattern -> IO ())
tParser tidal = choice [
  reserved "t1" >> return ((ts tidal)!!0),
  reserved "t2" >> return ((ts tidal)!!1),
  reserved "t3" >> return ((ts tidal)!!2),
  reserved "t4" >> return ((ts tidal)!!3),
  reserved "t5" >> return ((ts tidal)!!4),
  reserved "t6" >> return ((ts tidal)!!5),
  reserved "t7" >> return ((ts tidal)!!6),
  reserved "t8" >> return ((ts tidal)!!7),
  reserved "t9" >> return ((ts tidal)!!8)
  ] -}

{- transitionArg :: Stream -> Parser (Time -> [ControlPattern] -> ControlPattern)
transitionArg tidal = choice [
  parensOrApplied $ (reserved "xfadeIn" >> return (T.transition tidal . T.xfadeIn)) <*> literalArg
  ] -}

-- below is a stand-alone Tidal interpreter
-- can be compiled, for example, with: ghc --make Sound/Tidal/MiniTidal.hs -main-is Sound.Tidal.MiniTidal -o miniTidal

main :: IO ()
main = do
  putStrLn "miniTidal"
  tidal <- T.startTidal T.superdirtTarget T.defaultConfig
  forever $ do
    cmd <- miniTidalIO tidal <$> getLine
    either (\x -> putStrLn $ "error: " ++ show x) id cmd

-- things whose status in new tidal we are unsure of
--(function "within'" >> return T.within') <*> literalArg <*> transformationArg,
-- (function "revArc" >> return T.revArc) <*> literalArg,
--  (function "prr" >> return T.prr) <*> literalArg <*> literalArg <*> patternArg,
--  (function "preplace" >> return T.preplace) <*> literalArg <*> patternArg,
--  (function "prep" >> return T.prep) <*> literalArg <*> patternArg,
--  (function "preplace1" >> return T.preplace1) <*> patternArg,
--  (function "protate" >> return T.protate) <*> literalArg <*> literalArg,
--  (function "prot" >> return T.prot) <*> literalArg <*> literalArg,
--  (function "prot1" >> return T.prot1) <*> literalArg,
--  (function "fill" >> return T.fill) <*> patternArg,
--function "struct" >> return T.struct,
--  (function "sliceArc" >> return T.sliceArc) <*> literalArg
--  function "breakUp" >> return T.breakUp, -- removed from new Tidal?

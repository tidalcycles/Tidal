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


-- A type that is an instance of the class MiniTidal is a type that we know how to parse
-- both as an individual value and in the form of patterns. For each such type we will
-- provide separate pattern parsers at different levels of fixity.
class MiniTidal a where
  literal :: Parser a
  patternByInfixr0 :: Parser (Pattern a) -- eg. fast 4 $ s "bd cp"...
  patternByInfixl6 :: Parser (Pattern a) -- eg. "0 1" + "2 3", "0 1" - "2 3"
  patternByInfixl7 :: Parser (Pattern a) -- eg. "0 1" * "2 3", "0 1" / "2 3"
  patternByInfixl9 :: Parser (Pattern a) -- eg. s "bd cp" # n "0 2"... note: almost all Tidal-specific operators are infixl 9
  patternByApplication :: Parser (Pattern a) -- eg. s "bd cp"
  atomicPattern :: Parser (Pattern a) -- eg. sine or "1 2 3 4" or silence, a pattern that could be an patternForApplication without parens

pattern :: MiniTidal a => Parser (Pattern a)
pattern = choice [
  try $ parens pattern,
  try $ patternByInfixr0,
  try $ patternByInfixl6,
  try $ patternByInfixl7,
  try $ patternByInfixl9,
  try $ patternByApplication,
  atomicPattern
  ]

patternForInfixr0 :: MiniTidal a => Parser (Pattern a)
patternForInfixr0 = choice [
  try $ parens pattern,
  try $ patternByInfixl6,
  try $ patternByInfixl7,
  try $ patternByInfixl9,
  try $ patternByApplication,
  atomicPattern
  ]

patternForInfixl6 :: MiniTidal a => Parser (Pattern a)
patternForInfixl6 = choice [
  try $ parens pattern,
  try $ patternByInfixl7,
  try $ patternByInfixl9,
  try $ patternByApplication,
  atomicPattern
  ]

patternForInfixl7 :: MiniTidal a => Parser (Pattern a)
patternForInfixl7 = choice [
  try $ parens pattern,
  try $ patternByInfixl9,
  try $ patternByApplication,
  atomicPattern
  ]

patternForInfixl9 :: MiniTidal a => Parser (Pattern a)
patternForInfixl9 = choice [
  try $ parens pattern,
  try $ patternByApplication,
  atomicPattern
  ]

patternForApplication :: MiniTidal a => Parser (Pattern a)
patternForApplication = choice [
  try $ parens pattern,
  atomicPattern
  ]

literalArg :: MiniTidal a => Parser a
literalArg = choice [
  try $ parens literalArg,
  literal
  ]

listLiteralArg :: MiniTidal a => Parser [a]
listLiteralArg = try $ brackets $ commaSep literalArg

listPatternArg :: MiniTidal a => Parser [Pattern a]
listPatternArg = try $ brackets $ commaSep pattern

transformationArg :: MiniTidal a => Parser (Pattern a -> Pattern a)
transformationArg = choice [
  try $ appliedOrNot $ genericTransformationsWithoutArguments,
  parensOrApplied $ genericTransformationsWithArguments
  ]

listTransformationArg :: MiniTidal a => Parser [Pattern a -> Pattern a]
listTransformationArg = try $ parensOrNot $ brackets $ commaSep genericTransformations

silence :: Parser (Pattern a)
silence = $(function "silence")


instance MiniTidal Int where
  literal = int
  patternByInfixr0 = appAfter genericTransformations <*> patternForInfixr0
  patternByInfixl6 = chainl1 patternForInfixl6 (addition <|> subtraction)
  patternByInfixl7 = chainl1 patternForInfixl7 multiplication
  patternByInfixl9 = try $ applyOperator patternForInfixl9 rotationOperators patternForInfixl9
  patternByApplication = genericTransformations <*> patternForApplication
  atomicPattern = silence <|> (pure <$> literal) <|> parseBP'

matchAfter :: Parser a -> Parser b -> Parser a
matchAfter a b = do
  a' <- a
  b
  return a'

appAfter :: Parser a -> Parser a
appAfter x = matchAfter x $ reservedOp "$"

instance MiniTidal Integer where
  literal = integer
  patternByInfixr0 = appAfter genericTransformations <*> patternForInfixr0
  patternByInfixl6 = chainl1 patternForInfixl6 (addition <|> subtraction)
  patternByInfixl7 = chainl1 patternForInfixl7 multiplication
  patternByInfixl9 = try $ applyOperator patternForInfixl9 rotationOperators patternForInfixl9
  patternByApplication = genericTransformations <*> patternForApplication
  atomicPattern = silence <|> (pure <$> literal) <|> parseBP'


instance MiniTidal Double where
  literal = double
  patternByInfixr0 = appAfter genericTransformations <*> patternForInfixr0
  patternByInfixl6 = chainl1 patternForInfixl6 (addition <|> subtraction)
  patternByInfixl7 = chainl1 patternForInfixl7 multiplication
  patternByInfixl9 = try $ applyOperator patternForInfixl9 rotationOperators patternForInfixl9
  patternByApplication = genericTransformations <*> patternForApplication
  atomicPattern = choice [
    silence,
    pure <$> literal,
    parseBP',
    $(function "rand"),
    $(function "sine"),
    $(function "saw"),
    $(function "isaw"),
    $(function "tri"),
    $(function "square"),
    $(function "cosine")
    ]


instance MiniTidal Time where
  literal = (toRational <$> double) <|> (fromIntegral <$> integer)
  patternByInfixr0 = appAfter genericTransformations <*> patternForInfixr0
  patternByInfixl6 = chainl1 patternForInfixl6 (addition <|> subtraction)
  patternByInfixl7 = chainl1 patternForInfixl7 multiplication
  patternByInfixl9 = try $ applyOperator patternForInfixl9 rotationOperators patternForInfixl9
  patternByApplication = genericTransformations <*> patternForApplication
  atomicPattern = silence <|> (pure <$> literal) <|> parseBP'


instance MiniTidal Arc where
  literal = do
    xs <- parens (commaSep1 literal)
    if length xs == 2 then return (T.Arc (xs!!0) (xs!!1)) else unexpected "Arcs must contain exactly two values"
  patternByInfixr0 = appAfter genericTransformations <*> patternForInfixr0
  patternByInfixl6 = parserZero
  patternByInfixl7 = parserZero
  patternByInfixl9 = try $ applyOperator patternForInfixl9 rotationOperators patternForInfixl9
  patternByApplication = genericTransformations <*> patternForApplication
  atomicPattern = silence <|> (pure <$> literal)


instance MiniTidal (Time,Time) where
  literal = do
    xs <- parens (commaSep1 literal)
    if length xs == 2 then return ((xs!!0),(xs!!1)) else unexpected "(Time,Time) must contain exactly two values"
  patternByInfixr0 = appAfter genericTransformations <*> patternForInfixr0
  patternByInfixl6 = parserZero
  patternByInfixl7 = parserZero
  patternByInfixl9 = try $ applyOperator patternForInfixl9 rotationOperators patternForInfixl9
  patternByApplication = genericTransformations <*> patternForApplication
  atomicPattern = silence <|> (pure <$> literal)


instance MiniTidal String where
  literal = stringLiteral
  patternByInfixr0 = appAfter genericTransformations <*> patternForInfixr0
  patternByInfixl6 = parserZero
  patternByInfixl7 = parserZero
  patternByInfixl9 = try $ applyOperator patternForInfixl9 rotationOperators patternForInfixl9
  patternByApplication = genericTransformations <*> patternForApplication
  atomicPattern = parseBP' <|> silence


instance MiniTidal ControlMap where
  literal = parserZero
  patternByInfixr0 = choice [
    appAfter genericTransformations <*> patternForInfixr0, -- eg. fast 4 $ s "bd cp"
    appAfter pInt_pControl <*> patternForInfixr0, -- eg. n $ "0 1"
    appAfter pDouble_pControl <*> patternForInfixr0, -- eg. shape $ "0.2 0.4"
    appAfter pString_pControl <*> patternForInfixr0 -- eg. s $ "bd cp"
    ]
  patternByInfixl6 = parserZero
  patternByInfixl7 = parserZero
  patternByInfixl9 = choice [
    try $ applyOperator patternForInfixl9 controlPatternMergeOperator patternForInfixl9,
    try $ applyOperator patternForInfixl9 rotationOperators patternForInfixl9
    ]
  patternByApplication = choice [
    genericTransformations <*> patternForApplication, -- eg. fast 4 (s "bd cp")
    pInt_pControl <*> patternForApplication, -- eg. n "0 1"
    pDouble_pControl <*> patternForApplication, -- eg. shape "0.2 0.4"
    pString_pControl <*> patternForApplication -- eg. s "bd cp"
    ]
  atomicPattern = silence


{- instance (MiniTidal a) => MiniTidal [a] where
  literal = parserZero
  patternByInfixr0 = parserZero
  patternByInfixl6 = parserZero
  patternByInfixl7 = parserZero
  patternByInfixl9 = parserZero
  patternByApplication = parserZero
  atomicPattern = brackets pattern -}


applyOperator :: Parser a -> Parser (a -> b -> c) -> Parser b -> Parser c
applyOperator a f b = do
  a' <- a
  f' <- f
  b' <- b
  return $ f' a' b'


rotationOperators :: Parser (Pattern Time -> Pattern a -> Pattern a)
rotationOperators = choice [
  reservedOp "<~" >> return (T.<~),
  reservedOp "~>" >> return (T.~>)
  ]


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

pInt_pControl :: Parser (Pattern Int -> ControlPattern)
pInt_pControl = choice [
  $(function "coarse"),
  $(function "cut")
  ]

pDouble_pControl :: Parser (Pattern Double -> ControlPattern)
pDouble_pControl = choice [
  $(function "n"),
  $(function "up"),
  $(function "speed"),
  $(function "pan"),
  $(function "shape"),
  $(function "gain"),
  $(function "accelerate"),
  $(function "bandf"),
  $(function "bandq"),
  $(function "begin"),
  $(function "crush"),
  $(function "cutoff"),
  $(function "delayfeedback"),
  $(function "delaytime"),
  $(function "delay"),
  $(function "end"),
  $(function "hcutoff"),
  $(function "hresonance"),
  $(function "resonance"),
  $(function "loop"),
  $(function "note")
  ]

pString_pControl :: Parser (Pattern String -> ControlPattern)
pString_pControl = choice [
  $(function "s"),
  $(function "sound"),
  $(function "vowel"),
  $(function "unit")
  ]

genericComplexPattern :: MiniTidal a => Parser (Pattern a)
genericComplexPattern = choice [
  lp_p <*> listPatternArg,
  l_p <*> listLiteralArg,
  pInt_p <*> patternForApplication
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

genericTransformations :: MiniTidal a => Parser (Pattern a -> Pattern a)
genericTransformations = p_p

genericTransformationsWithArguments :: MiniTidal a => Parser (Pattern a -> Pattern a)
genericTransformationsWithArguments = p_p

genericTransformationsWithoutArguments :: MiniTidal a => Parser (Pattern a -> Pattern a)
genericTransformationsWithoutArguments = p_p_noArgs

p_p :: MiniTidal a => Parser (Pattern a -> Pattern a)
p_p = choice [
  try $ parens p_p,
  p_p_p <*> patternForApplication,
  t_p_p <*> transformationArg,
  lp_p_p <*> listPatternArg,
  lt_p_p <*> listTransformationArg,
  lpInt_p_p <*> listPatternArg,
  pTime_p_p <*> patternForApplication,
  pInt_p_p <*> patternForApplication,
  pString_p_p <*> patternForApplication,
  pDouble_p_p <*> patternForApplication,
  vTime_p_p <*> literalArg,
  vInt_p_p <*> literalArg,
  vTimeTime_p_p <*> literalArg,
  pDouble_p_p <*> patternForApplication,
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
--  liftA2 <$> binaryFunctions,
  $(function "overlay"),
  $(function "append"),
  vTime_p_p_p <*> literalArg,
  pInt_p_p_p <*> patternForApplication
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
  pTime_pTime_p_p <*> patternForApplication
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
  pInt_pInt_p_p <*> patternForApplication
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
  pInt_t_p_p <*> patternForApplication,
  pDouble_t_p_p <*> patternForApplication,
  lvInt_t_p_p <*> listLiteralArg,
  vInt_t_p_p <*> literalArg,
  vDouble_t_p_p <*> literalArg,
  vTimeTime_t_p_p <*> literalArg,
  pTime_t_p_p <*> patternForApplication
  ]

lpInt_p_p :: MiniTidal a=> Parser ([Pattern Int] -> Pattern a -> Pattern a)
lpInt_p_p = $(function "distrib")

lp_p_p :: MiniTidal a => Parser ([Pattern a] -> Pattern a -> Pattern a)
lp_p_p = choice [
  try $ parens lp_p_p,
  try $ spreads <*> parens p_p_p
  ]

l_pInt_p :: MiniTidal a=> Parser ([a] -> Pattern Int -> Pattern a)
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
  pInt_pInt_t_p_p <*> patternForApplication
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
  pInt_pInt_p_p_p <*> patternForApplication
  ]

pInt_pInt_p_p_p :: MiniTidal a => Parser (Pattern Int -> Pattern Int -> Pattern a -> Pattern a -> Pattern a)
pInt_pInt_p_p_p = $(function "euclidFull")

vInt_pInt_pInt_p_p :: MiniTidal a => Parser (Int -> Pattern Int -> Pattern Int -> Pattern a -> Pattern a)
vInt_pInt_pInt_p_p = choice [
  try $ parens vInt_pInt_pInt_p_p,
  pTime_vInt_pInt_pInt_p_p <*> patternForApplication
  ]

pTime_vInt_pInt_pInt_p_p :: MiniTidal a => Parser (Pattern Time -> Int -> Pattern Int -> Pattern Int -> Pattern a -> Pattern a)
pTime_vInt_pInt_pInt_p_p = $(function "fit'")

pControl_pControl :: Parser (ControlPattern -> ControlPattern)
pControl_pControl = choice [
  try $ parens pControl_pControl,
  pInt_pControl_pControl <*> patternForApplication,
  pDouble_pControl_pControl <*> patternForApplication,
  pTime_pControl_pControl <*> patternForApplication,
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
  pInt_pDouble_pControl_pControl <*> patternForApplication
  ]

pInt_pDouble_pControl_pControl :: Parser (Pattern Int -> Pattern Double -> ControlPattern -> ControlPattern)
pInt_pDouble_pControl_pControl = $(function "striate'")

pTime_pControl_pControl :: Parser (Pattern Time -> ControlPattern -> ControlPattern)
pTime_pControl_pControl = choice [
  try $ parens pTime_pControl_pControl,
  pDouble_pTime_pControl_pControl <*> patternForApplication
  ]

pDouble_pTime_pControl_pControl :: Parser (Pattern Double -> Pattern Time -> ControlPattern -> ControlPattern)
pDouble_pTime_pControl_pControl = choice [
  try $ parens pDouble_pTime_pControl_pControl,
  pInteger_pDouble_pTime_pControl_pControl <*> patternForApplication
  ]

pInteger_pDouble_pTime_pControl_pControl :: Parser (Pattern Integer -> Pattern Double -> Pattern Time -> ControlPattern -> ControlPattern)
pInteger_pDouble_pTime_pControl_pControl = $(function "stut")









addition :: Num a => Parser (a -> a -> a)
addition = opParser "+" >> return (+)

subtraction :: Num a => Parser (a -> a -> a)
subtraction = opParser "-" >> return (-)

multiplication :: Num a => Parser (a -> a -> a)
multiplication = opParser "*" >> return (*)

division :: Fractional a => Parser (a -> a -> a)
division = opParser "/" >> return (/)


{- enumComplexPatterns :: (Enum a, Num a, MiniTidal a) => Parser (Pattern a -> Pattern a)
enumComplexPatterns = choice [
  $(function "run"),
  $(function "scan")
  ]

numComplexPatterns :: (Num a, MiniTidal a) => Parser (Pattern a)
numComplexPatterns = choice [
  $(function "irand") <*> literal,
  $(function "toScale'") <*> literalArg <*> patternForApplication <*> patternForApplication,
  $(function "toScale") <*> patternForApplication <*> patternForApplication
  ]

intComplexPatterns :: Parser (Pattern Int)
intComplexPatterns = choice [
  $(function "randStruct") <*> literalArg
  ]
-}

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
  dParser tidal <*> patternForApplication
{-  tParser tidal <*> transitionArg tidal <*> patternForApplication, -}
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

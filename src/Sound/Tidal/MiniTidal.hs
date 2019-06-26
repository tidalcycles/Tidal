{-# LANGUAGE FlexibleInstances, TemplateHaskell, FlexibleContexts #-}

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
    x <- value
    eof
    return x
  ]

-- A type that is an instance of the class Value is a type that we know how to parse.
-- For each such type we will provide separate parsers at different levels of fixity.
class Value a where
  dollarValue :: Parser a -- eg. fast 4 $ s "bd cp"...
  infixlValue  :: Parser a -- all other ways of parsing this value, typically infixl operators, eg. "0 1" + "2 3", s "bd cp" # n "0 2"
  applicationValue :: Parser a -- eg. s "bd cp"
  atomicValue :: Parser a -- eg. sine or "1 2 3 4" or silence

value :: Value a => Parser a
value = choice [
  try $ parens value,
  try $ dollarValue,
  try $ infixlValue,
  try $ applicationValue,
  atomicValue
  ]

valueForInfixl :: Value a => Parser a
valueForInfixl = choice [
  try $ parens value,
  try $ applicationValue,
  atomicValue
  ]

arg :: Value a => Parser a
arg = choice [
  try $ parens value,
  atomicValue
  ]

instance (Value a) => Value [a] where
  dollarValue = parserZero
  infixlValue = parserZero
  applicationValue = parserZero
  atomicValue = try $ brackets $ commaSep value


instance (Value a, Value (Pattern a)) => Value (Pattern a -> Pattern a) where
  dollarValue = parserZero -- TODO
  infixlValue = parserZero -- will adapt later for function composition
  applicationValue = p_p
  atomicValue = p_p_noArgs


instance Value Int where
  dollarValue = parserZero
  infixlValue = parserZero
  applicationValue = parserZero
  atomicValue = int


instance Value (Pattern Int) where
  dollarValue = appAfter genericTransformations <*> value
  infixlValue = choice [
    try $ chainl1 valueForInfixl (additionOp <|> subtractionOp),
    try $ chainl1 valueForInfixl multiplicationOp,
    applyOperator valueForInfixl rotationOperators valueForInfixl
    ]
  applicationValue = genericTransformations <*> arg
  atomicValue = silence <|> (pure <$> arg) <|> parseBP'


instance Value Integer where
  dollarValue = parserZero
  infixlValue = parserZero
  applicationValue = parserZero
  atomicValue = integer


instance Value (Pattern Integer) where
  dollarValue = appAfter genericTransformations <*> value
  infixlValue = choice [
    try $ chainl1 valueForInfixl (additionOp <|> subtractionOp),
    try $ chainl1 valueForInfixl multiplicationOp,
    applyOperator valueForInfixl rotationOperators valueForInfixl
    ]
  applicationValue = genericTransformations <*> arg
  atomicValue = silence <|> (pure <$> arg) <|> parseBP'


instance Value Double where
  dollarValue = parserZero
  infixlValue = parserZero
  applicationValue = parserZero
  atomicValue = double


instance Value (Pattern Double) where
  dollarValue = appAfter genericTransformations <*> value
  infixlValue = choice [
    try $ chainl1 valueForInfixl (additionOp <|> subtractionOp),
    try $ chainl1 valueForInfixl multiplicationOp,
    applyOperator valueForInfixl rotationOperators valueForInfixl
    ]
  applicationValue = genericTransformations <*> arg
  atomicValue = choice [
    silence,
    pure <$> arg,
    parseBP',
    $(function "rand"),
    $(function "sine"),
    $(function "saw"),
    $(function "isaw"),
    $(function "tri"),
    $(function "square"),
    $(function "cosine")
    ]


instance Value Time where
  dollarValue = parserZero
  infixlValue = parserZero
  applicationValue = parserZero
  atomicValue = (toRational <$> double) <|> (fromIntegral <$> integer)


instance Value (Pattern Time) where
  dollarValue = appAfter genericTransformations <*> value
  infixlValue = choice [
    try $ chainl1 valueForInfixl (additionOp <|> subtractionOp),
    try $ chainl1 valueForInfixl multiplicationOp,
    applyOperator valueForInfixl rotationOperators valueForInfixl
    ]
  applicationValue = genericTransformations <*> arg
  atomicValue = silence <|> (pure <$> arg) <|> parseBP'


instance Value Arc where
  dollarValue = parserZero
  infixlValue = parserZero
  applicationValue = parserZero
  atomicValue = do
    xs <- parens (commaSep1 atomicValue)
    if length xs == 2 then return (T.Arc (xs!!0) (xs!!1)) else unexpected "Arcs must contain exactly two values"


instance Value (Pattern Arc) where
  dollarValue = appAfter genericTransformations <*> value
  infixlValue = applyOperator valueForInfixl rotationOperators valueForInfixl
  applicationValue = genericTransformations <*> arg
  atomicValue = silence <|> (pure <$> arg)


instance Value (Time,Time) where
  dollarValue = parserZero
  infixlValue = parserZero
  applicationValue = parserZero
  atomicValue = do
    xs <- parens (commaSep1 arg)
    if length xs == 2 then return ((xs!!0),(xs!!1)) else unexpected "(Time,Time) must contain exactly two values"


instance Value (Pattern (Time,Time)) where
  dollarValue = appAfter genericTransformations <*> value
  infixlValue = applyOperator valueForInfixl rotationOperators valueForInfixl
  applicationValue = genericTransformations <*> arg
  atomicValue = silence <|> (pure <$> arg)


instance {-# OVERLAPPING #-} Value String where
  dollarValue = parserZero
  infixlValue = parserZero
  applicationValue = parserZero
  atomicValue = stringLiteral


instance Value (Pattern String) where
  dollarValue = appAfter genericTransformations <*> value
  infixlValue = applyOperator valueForInfixl rotationOperators valueForInfixl
  applicationValue = genericTransformations <*> arg
  atomicValue = silence <|> parseBP'


instance Value ControlMap where
  dollarValue = parserZero
  infixlValue = parserZero
  applicationValue = parserZero
  atomicValue = parserZero


instance Value (Pattern ControlMap) where
  dollarValue = choice [
    try $ appAfter genericTransformations <*> value, -- eg. fast 4 $ s "bd cp"
    try $ appAfter pInt_pControl <*> value, -- eg. n $ "0 1"
    try $ appAfter pDouble_pControl <*> value, -- eg. shape $ "0.2 0.4"
    appAfter pString_pControl <*> value -- eg. s $ "bd cp"
    ]
  infixlValue = choice [
    try $ chainl1 valueForInfixl controlPatternMergeOperator,
    applyOperator valueForInfixl rotationOperators valueForInfixl
    ]
  applicationValue = choice [
    genericTransformations <*> arg, -- eg. fast 4 (s "bd cp")
    pInt_pControl <*> arg, -- eg. n "0 1"
    pDouble_pControl <*> arg, -- eg. shape "0.2 0.4"
    pString_pControl <*> arg -- eg. s "bd cp"
    ]
  atomicValue = silence



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

{- genericComplexPattern :: Value a => Parser (Pattern a)
genericComplexPattern = choice [
  lp_p <*> arg,
  l_p <*> arg,
  pInt_p <*> arg
  ] -}

genericTransformations :: (Value a, Value (Pattern a)) => Parser (Pattern a -> Pattern a)
genericTransformations = p_p_noArgs <|> p_p

p_p_noArgs :: Parser (Pattern a -> Pattern a)
p_p_noArgs  = choice [
  $(function "brak"),
  $(function "rev"),
  $(function "palindrome"),
  $(function "stretch"),
  $(function "loopFirst"),
  $(function "degrade")
  ]

p_p :: (Value a, Value (Pattern a)) => Parser (Pattern a -> Pattern a)
p_p = choice [
  try $ parens p_p,
  p_p_p <*> arg,
  t_p_p <*> arg,
  lp_p_p <*> arg,
  lt_p_p <*> arg,
  lpInt_p_p <*> arg,
  pTime_p_p <*> arg,
  pInt_p_p <*> arg,
  pString_p_p <*> arg,
  pDouble_p_p <*> arg,
  vTime_p_p <*> arg,
  vInt_p_p <*> arg,
  vTimeTime_p_p <*> arg,
  pDouble_p_p <*> arg,
  lTime_p_p <*> arg
  ]

lt_p_p :: Parser ([t -> Pattern a] -> t -> Pattern a)
lt_p_p = choice [
  try $ parens lt_p_p,
  spreads <*> (nestedParens $ reservedOp "$" >> return ($))
  ]

l_p :: Parser ([a] -> Pattern a)
l_p = choice [
  $(function "listToPat"),
  $(function "choose"),
  $(function "cycleChoose")
  ]

lp_p :: Value a => Parser ([Pattern a] -> Pattern a)
lp_p = choice [
  $(function "stack"),
  $(function "fastcat"),
  $(function "slowcat"),
  $(function "cat"),
  $(function "randcat")
  ]

pInt_p :: (Value a, Value [a]) => Parser (Pattern Int -> Pattern a)
pInt_p = choice [
  try $ parens pInt_p,
  l_pInt_p <*> arg
  ]

p_p_p :: Value a => Parser (Pattern a -> Pattern a -> Pattern a)
p_p_p = choice [
  try $ parens p_p_p,
--  liftA2 <$> binaryFunctions,
  $(function "overlay"),
  $(function "append"),
  vTime_p_p_p <*> arg,
  pInt_p_p_p <*> arg
  ]

pTime_p_p :: Value a => Parser (Pattern Time -> Pattern a -> Pattern a)
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
  pTime_pTime_p_p <*> arg
  ]

lTime_p_p :: Value a => Parser ([Time] -> Pattern a -> Pattern a)
lTime_p_p = choice [
  try $ parens lTime_p_p,
  $(function "spaceOut"),
  spreads <*> parens vTime_p_p -- re: spread
  ]

spreads :: Parser ((b -> t -> Pattern a) -> [b] -> t -> Pattern a)
spreads = choice [
  $(function "spread"),
  $(function "slowspread"),
  $(function "fastspread")
  ]

pInt_p_p :: Value a => Parser (Pattern Int -> Pattern a -> Pattern a)
pInt_p_p = choice [
  try $ parens pInt_p_p,
  $(function "iter"),
  $(function "iter'"),
  $(function "ply"),
  $(function "substruct'"),
  $(function "slowstripe"),
  $(function "shuffle"),
  $(function "scramble"),
  pInt_pInt_p_p <*> arg
  ]

pString_p_p :: Parser (Pattern String -> Pattern a -> Pattern a)
pString_p_p = $(function "substruct")

pDouble_p_p :: Value a => Parser (Pattern Double -> Pattern a -> Pattern a)
pDouble_p_p = choice [
  try $ parens pDouble_p_p,
  $(function "degradeBy"),
  $(function "unDegradeBy"),
  vInt_pDouble_p_p <*> arg
  ]

vTime_p_p :: Value a => Parser (Time -> Pattern a -> Pattern a)
vTime_p_p = choice [
  try $ parens vTime_p_p,
  $(function "rotL"),
  $(function "rotR"),
  vTime_vTime_p_p <*> arg
  ]

vInt_p_p :: Parser (Int -> Pattern a -> Pattern a)
vInt_p_p = $(function "repeatCycles")

vTimeTime_p_p :: Parser ((Time,Time) -> Pattern a -> Pattern a)
vTimeTime_p_p = choice [
  $(function "compress"),
  $(function "zoom"),
  $(function "compressTo")
  ]

t_p_p :: Value a => Parser ((Pattern a -> Pattern a) -> Pattern a -> Pattern a)
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
  pInt_t_p_p <*> arg,
  pDouble_t_p_p <*> arg,
  lvInt_t_p_p <*> arg,
  vInt_t_p_p <*> arg,
  vDouble_t_p_p <*> arg,
  vTimeTime_t_p_p <*> arg,
  pTime_t_p_p <*> arg
  ]

lpInt_p_p :: Parser ([Pattern Int] -> Pattern a -> Pattern a)
lpInt_p_p = $(function "distrib")

lp_p_p :: Value a => Parser ([Pattern a] -> Pattern a -> Pattern a)
lp_p_p = choice [
  try $ parens lp_p_p,
  try $ spreads <*> parens p_p_p
  ]

l_pInt_p :: Value a => Parser ([a] -> Pattern Int -> Pattern a)
l_pInt_p = choice [
  try $ parens l_pInt_p,
  vInt_l_pInt_p <*> arg
  ]

pTime_t_p_p :: Parser (Pattern Time -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
pTime_t_p_p = $(function "off")

vInt_l_pInt_p :: Parser (Int -> [a] -> Pattern Int -> Pattern a)
vInt_l_pInt_p = $(function "fit")

vTime_p_p_p :: Parser (Time -> Pattern a -> Pattern a -> Pattern a)
vTime_p_p_p = $(function "wedge")

vInt_pDouble_p_p :: Parser (Int -> Pattern Double -> Pattern a -> Pattern a)
vInt_pDouble_p_p = $(function "degradeOverBy")

pInt_t_p_p :: Value a => Parser (Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
pInt_t_p_p = choice [
  try $ parens pInt_t_p_p,
  $(function "every"),
  pInt_pInt_t_p_p <*> arg
  ]

pDouble_t_p_p :: Parser (Pattern Double -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
pDouble_t_p_p = $(function "sometimesBy")

lvInt_t_p_p :: Parser ([Int] -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
lvInt_t_p_p = $(function "foldEvery")

vTime_vTime_p_p :: Parser (Time -> Time -> Pattern a -> Pattern a)
vTime_vTime_p_p = $(function "playFor")

vTimeTime_t_p_p :: Parser ((Time,Time) -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
vTimeTime_t_p_p = $(function "within")

vInt_t_p_p :: Value a => Parser (Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
vInt_t_p_p = choice [
  try $ parens vInt_t_p_p,
  $(function "chunk"),
  vInt_vInt_t_p_p <*> arg
  ]

vDouble_t_p_p :: Parser (Double -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
vDouble_t_p_p = $(function "someCyclesBy")

pInt_pInt_p_p :: Value a => Parser (Pattern Int -> Pattern Int -> Pattern a -> Pattern a)
pInt_pInt_p_p = choice [
  try $ parens pInt_pInt_p_p,
  $(function "euclid"),
  $(function "euclidInv"),
  vInt_pInt_pInt_p_p <*> arg
  ]

pTime_pTime_p_p :: Parser (Pattern Time -> Pattern Time -> Pattern a -> Pattern a)
pTime_pTime_p_p = $(function "swingBy")

pInt_pInt_t_p_p :: Parser (Pattern Int -> Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
pInt_pInt_t_p_p = $(function "every'")

vInt_vInt_t_p_p :: Parser (Int -> Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
vInt_vInt_t_p_p = $(function "whenmod")

pInt_p_p_p :: Value a => Parser (Pattern Int -> Pattern a -> Pattern a -> Pattern a)
pInt_p_p_p = choice [
  try $ parens pInt_p_p_p,
  pInt_pInt_p_p_p <*> arg
  ]

pInt_pInt_p_p_p :: Parser (Pattern Int -> Pattern Int -> Pattern a -> Pattern a -> Pattern a)
pInt_pInt_p_p_p = $(function "euclidFull")

vInt_pInt_pInt_p_p :: Value a => Parser (Int -> Pattern Int -> Pattern Int -> Pattern a -> Pattern a)
vInt_pInt_pInt_p_p = choice [
  try $ parens vInt_pInt_pInt_p_p,
  pTime_vInt_pInt_pInt_p_p <*> arg
  ]

pTime_vInt_pInt_pInt_p_p :: Parser (Pattern Time -> Int -> Pattern Int -> Pattern Int -> Pattern a -> Pattern a)
pTime_vInt_pInt_pInt_p_p = $(function "fit'")

pControl_pControl :: Parser (ControlPattern -> ControlPattern)
pControl_pControl = choice [
  try $ parens pControl_pControl,
  pInt_pControl_pControl <*> arg,
  pDouble_pControl_pControl <*> arg,
  pTime_pControl_pControl <*> arg,
  tControl_pControl_pControl <*> tControlArg
  ]

tControlArg :: Parser (ControlPattern -> ControlPattern)
tControlArg = p_p_noArgs <|> p_p

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
  pInt_pDouble_pControl_pControl <*> arg
  ]

pInt_pDouble_pControl_pControl :: Parser (Pattern Int -> Pattern Double -> ControlPattern -> ControlPattern)
pInt_pDouble_pControl_pControl = $(function "striate'")

pTime_pControl_pControl :: Parser (Pattern Time -> ControlPattern -> ControlPattern)
pTime_pControl_pControl = choice [
  try $ parens pTime_pControl_pControl,
  pDouble_pTime_pControl_pControl <*> arg
  ]

pDouble_pTime_pControl_pControl :: Parser (Pattern Double -> Pattern Time -> ControlPattern -> ControlPattern)
pDouble_pTime_pControl_pControl = choice [
  try $ parens pDouble_pTime_pControl_pControl,
  pInteger_pDouble_pTime_pControl_pControl <*> arg
  ]

pInteger_pDouble_pTime_pControl_pControl :: Parser (Pattern Integer -> Pattern Double -> Pattern Time -> ControlPattern -> ControlPattern)
pInteger_pDouble_pTime_pControl_pControl = $(function "stut")

additionOp :: Num a => Parser (a -> a -> a)
additionOp = opParser "+" >> return (+)

subtractionOp :: Num a => Parser (a -> a -> a)
subtractionOp = opParser "-" >> return (-)

multiplicationOp :: Num a => Parser (a -> a -> a)
multiplicationOp = opParser "*" >> return (*)

divisionOp :: Fractional a => Parser (a -> a -> a)
divisionOp = opParser "/" >> return (/)

enumP_enumP :: (Enum a, Num a) => Parser (Pattern a -> Pattern a)
enumP_enumP = choice [
  $(function "run"),
  $(function "scan")
  ]

{-
numComplexPatterns :: (Num a) => Parser (Pattern a)
numComplexPatterns = choice [
  $(function "irand") <*> literal,
  $(function "toScale'") <*> atomicValue <*> arg <*> arg,
  $(function "toScale") <*> arg <*> arg
  ]

intComplexPatterns :: Parser (Pattern Int)
intComplexPatterns = choice [
  $(function "randStruct") <*> atomicValue
  ]
-}

silence :: Parser (Pattern a)
silence = $(function "silence")

matchAfter :: Parser a -> Parser b -> Parser a
matchAfter a b = do
  a' <- a
  b
  return a'

appAfter :: Parser a -> Parser a
appAfter x = matchAfter x $ reservedOp "$"

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
  dParser tidal <*> arg
{-  tParser tidal <*> transitionArg tidal <*> arg, -}
--  (reserved "setcps" >> return (T.streamOnce tidal True . T.cps)) <*> atomicValue
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
  parensOrApplied $ (reserved "xfadeIn" >> return (T.transition tidal . T.xfadeIn)) <*> atomicValue
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

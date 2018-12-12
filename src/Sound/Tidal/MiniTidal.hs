{-# LANGUAGE FlexibleInstances #-}

module Sound.Tidal.MiniTidal (miniTidal,miniTidalIO,main) where

import           Data.Functor.Identity (Identity)
import           Text.Parsec.Language (haskellDef)
import           Text.Parsec.Prim (ParsecT)
import           Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import           Control.Monad (forever)
-- import Data.List (intercalate)
-- import Data.Bool (bool)
-- import Data.Ratio

import           Sound.Tidal.Context (Pattern,ControlMap,ControlPattern,Enumerable,Parseable,Time,Arc,TPat,Stream)
import qualified Sound.Tidal.Context as T

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

class Pattern' a where
  simplePattern :: Parser (Pattern a)
  complexPattern :: Parser (Pattern a)
  mergeOperator :: Parser (Pattern a -> Pattern a -> Pattern a)
  transformationWithoutArgs :: Parser (Pattern a -> Pattern a)
  transformationWithArgs :: Parser (Pattern a -> Pattern a)
  literal :: Parser a

pattern :: Pattern' a => Parser (Pattern a)
pattern = chainl1 pattern' mergeOperator

pattern' :: Pattern' a => Parser (Pattern a)
pattern' = choice [
  nestedParens $ chainl1 pattern mergeOperator,
  parensOrNot complexPattern,
  parensOrNot genericComplexPatterns,
  parensOrNot transformedPattern,
  parensOrNot simplePattern,
  silence
  ]

patternArg :: Pattern' a => Parser (Pattern a)
patternArg = choice [
  try $ parensOrApplied $ chainl1 pattern mergeOperator,
  try $ parensOrApplied transformedPattern,
  try $ parensOrApplied complexPattern,
  try $ parensOrApplied genericComplexPatterns,
  appliedOrNot simplePattern,
  appliedOrNot silence
  ]

literalArg :: Pattern' a => Parser a
literalArg = choice [
  literal,
  nestedParens literal,
  try $ applied $ parensOrNot literal
  ]

listLiteralArg :: Pattern' a => Parser [a]
listLiteralArg = brackets (commaSep $ parensOrNot literal)

listPatternArg :: Pattern' a => Parser [Pattern a]
listPatternArg = parensOrNot $ brackets (commaSep pattern)

silence :: Parser (Pattern a)
silence = function "silence" >> return T.silence

genericComplexPatterns :: Pattern' a => Parser (Pattern a)
genericComplexPatterns = choice [
  (function "stack" >> return T.stack) <*> listPatternArg,
  (function "fastcat" >> return T.fastcat) <*> listPatternArg,
  (function "slowcat" >> return T.slowcat) <*> listPatternArg,
  (function "cat" >> return T.cat) <*> listPatternArg,
  (function "listToPat" >> return T.listToPat) <*> listLiteralArg,
  (function "fit" >> return T.fit) <*> literalArg <*> listLiteralArg <*> patternArg,
  (function "choose" >> return T.choose) <*> listLiteralArg,
  (function "randcat" >> return T.randcat) <*> listPatternArg,
  (function "cycleChoose" >> return T.cycleChoose) <*> listLiteralArg
  ]

enumComplexPatterns :: (Enum a, Num a, Pattern' a) => Parser (Pattern a)
enumComplexPatterns = choice [
  (function "run" >> return T.run) <*> patternArg,
  (function "scan" >> return T.scan) <*> patternArg
  ]

numComplexPatterns :: (Num a, Pattern' a) => Parser (Pattern a)
numComplexPatterns = choice [
  (function "irand" >> return T.irand) <*> literal,
  (function "toScale'" >> return T.toScale') <*> literalArg <*> listLiteralArg <*> patternArg,
  (function "toScale" >> return T.toScale) <*> listLiteralArg <*> patternArg
  ]

intComplexPatterns :: Parser (Pattern Int)
intComplexPatterns = choice [
  (function "randStruct" >> return T.randStruct) <*> literalArg
  ]

transformedPattern :: Pattern' a => Parser (Pattern a)
transformedPattern = (transformationWithArgs <|> transformationWithoutArgs) <*> patternArg

instance Pattern' ControlMap where
  simplePattern = choice []
  complexPattern = specificControlPatterns
  mergeOperator = controlPatternMergeOperator
  transformationWithArgs = controlPatternTransformation <|> patternTransformationWithArgs
  transformationWithoutArgs = patternTransformationWithoutArgs
  literal = choice []

controlPatternMergeOperator :: Parser (ControlPattern -> ControlPattern -> ControlPattern)
controlPatternMergeOperator = choice [
  op "#" >> return (T.#),
  op "|>" >> return (T.|>),
  op "<|" >> return (T.<|),
  op "|>|" >> return (T.|>|),
  op "|<|" >> return (T.|<),
  op "|+|" >> return (T.|+|),
  op "|-|" >> return (T.|-|),
  op "|*|" >> return (T.|*|),
  op "|/|" >> return (T.|/|)
  ]

specificControlPatterns :: Parser ControlPattern
specificControlPatterns = choice [
  (function "coarse" >> return T.coarse) <*> patternArg,
  (function "cut" >> return T.cut) <*> patternArg,
  (function "n" >> return T.n) <*> patternArg,
  (function "up" >> return T.up) <*> patternArg,
  (function "speed" >> return T.speed) <*> patternArg,
  (function "pan" >> return T.pan) <*> patternArg,
  (function "shape" >> return T.shape) <*> patternArg,
  (function "gain" >> return T.gain) <*> patternArg,
  (function "accelerate" >> return T.accelerate) <*> patternArg,
  (function "bandf" >> return T.bandf) <*> patternArg,
  (function "bandq" >> return T.bandq) <*> patternArg,
  (function "begin" >> return T.begin) <*> patternArg,
  (function "crush" >> return T.crush) <*> patternArg,
  (function "cutoff" >> return T.cutoff) <*> patternArg,
  (function "delayfeedback" >> return T.delayfeedback) <*> patternArg,
  (function "delaytime" >> return T.delaytime) <*> patternArg,
  (function "delay" >> return T.delay) <*> patternArg,
  (function "end" >> return T.end) <*> patternArg,
  (function "hcutoff" >> return T.hcutoff) <*> patternArg,
  (function "hresonance" >> return T.hresonance) <*> patternArg,
  (function "resonance" >> return T.resonance) <*> patternArg,
  (function "shape" >> return T.shape) <*> patternArg,
  (function "loop" >> return T.loop) <*> patternArg,
  (function "s" >> return T.s) <*> patternArg,
  (function "sound" >> return T.sound) <*> patternArg,
  (function "vowel" >> return T.vowel) <*> patternArg,
  (function "unit" >> return T.unit) <*> patternArg,
  (function "note" >> return T.note) <*> patternArg
  ]

controlPatternTransformation :: Parser (ControlPattern -> ControlPattern)
controlPatternTransformation = choice [
  function "chop" >> patternArg >>= return . T.chop,
  function "striate" >> patternArg >>= return . T.striate,
  (function "striate'" >> return T.striate') <*> patternArg <*> patternArg,
  (function "stut" >> return T.stut) <*> patternArg <*> patternArg <*> patternArg,
  function "jux" >> patternTransformationArg >>= return . T.jux
  ]

patternTransformationArg :: Pattern' a => Parser (Pattern a -> Pattern a)
patternTransformationArg = appliedOrNot transformationWithoutArgs <|> parensOrApplied transformationWithArgs

patternTransformationWithoutArgs :: Parser (Pattern a -> Pattern a)
patternTransformationWithoutArgs = choice [
  function "brak" >> return T.brak,
  function "rev" >> return T.rev,
  function "palindrome" >> return T.palindrome,
  function "stretch" >> return T.stretch,
  function "loopFirst" >> return T.loopFirst,
--  function "breakUp" >> return T.breakUp, -- removed from new Tidal?
  function "degrade" >> return T.degrade
  ]

patternTransformationWithArgs :: Pattern' a => Parser (Pattern a -> Pattern a)
patternTransformationWithArgs = parensOrNot $ choice [
  function "fast" >> patternArg >>= return . T.fast,
--  function "fast'" >> patternArg >>= return . T.fast', -- removed from Tidal 1.0?
  function "density" >> patternArg >>= return . T.density,
  function "slow" >> patternArg >>= return . T.slow,
  function "iter" >> patternArg >>= return . T.iter,
  function "iter'" >> patternArg >>= return . T.iter',
  function "trunc" >> patternArg >>= return . T.trunc,
  (function "swingBy" >> return T.swingBy) <*> patternArg <*> patternArg,
  (function "append" >> return T.append) <*> patternArg,
--  (function "append'" >> return T.append') <*> patternArg,
  (function "every" >> return T.every) <*> patternArg <*> patternTransformationArg,
  (function "every'" >> return T.every') <*> patternArg <*> patternArg <*> patternTransformationArg,
  (function "whenmod" >> return T.whenmod) <*> int <*> int <*> patternTransformationArg,
  (function "overlay" >> return T.overlay) <*> patternArg,
  (function "fastGap" >> return T.fastGap) <*> patternArg,
  (function "densityGap" >> return T.densityGap) <*> patternArg,
  (function "sparsity" >> return T.sparsity) <*> patternArg,
  (function "rotL" >> return T.rotL) <*> literalArg,
  (function "rotR" >> return T.rotR) <*> literalArg,
  (function "playFor" >> return T.playFor) <*> literalArg <*> literalArg,
  (function "foldEvery" >> return T.foldEvery) <*> listLiteralArg <*> patternTransformationArg,
  (function "superimpose" >> return T.superimpose) <*> patternTransformationArg,
  (function "trunc" >> return T.trunc) <*> patternArg,
  (function "linger" >> return T.linger) <*> patternArg,
  (function "zoom" >> return T.zoomArc) <*> literalArg,
  (function "compress" >> return T.compressArc) <*> literalArg,
--  (function "sliceArc" >> return T.sliceArc) <*> literalArg,
  (function "within" >> return T.withinArc) <*> literalArg <*> patternTransformationArg,
  --(function "within'" >> return T.within') <*> literalArg <*> patternTransformationArg,
  -- (function "revArc" >> return T.revArc) <*> literalArg,
  (function "euclid" >> return T.euclid) <*> patternArg <*> patternArg,
  (function "euclidFull" >> return T.euclidFull) <*> patternArg <*> patternArg <*> patternArg,
  (function "euclidInv" >> return T.euclidInv) <*> patternArg <*> patternArg,
  (function "distrib" >> return T.distrib) <*> listPatternArg,
  (function "wedge" >> return T.wedge) <*> literalArg <*> patternArg,
--  (function "prr" >> return T.prr) <*> literalArg <*> literalArg <*> patternArg,
--  (function "preplace" >> return T.preplace) <*> literalArg <*> patternArg,
--  (function "prep" >> return T.prep) <*> literalArg <*> patternArg,
--  (function "preplace1" >> return T.preplace1) <*> patternArg,
--  (function "protate" >> return T.protate) <*> literalArg <*> literalArg,
--  (function "prot" >> return T.prot) <*> literalArg <*> literalArg,
--  (function "prot1" >> return T.prot1) <*> literalArg,
  (function "discretise" >> return T.discretise) <*> patternArg,
  (function "segment" >> return T.segment) <*> patternArg,
  --(function "struct" >> return T.struct) <*> patternArg,
  (function "substruct" >> return T.substruct) <*> patternArg,
  (function "compressTo" >> return T.compressArcTo) <*> literalArg,
  (function "substruct'" >> return T.substruct') <*> patternArg,
  (function "slowstripe" >> return T.slowstripe) <*> patternArg,
  (function "fit'" >> return T.fit') <*> patternArg <*> literalArg <*> patternArg <*> patternArg,
  (function "chunk" >> return T.chunk) <*> literalArg <*> patternTransformationArg,
  (function "timeLoop" >> return T.timeLoop) <*> patternArg,
  (function "swing" >> return T.swing) <*> patternArg,
  (function "degradeBy" >> return T.degradeBy) <*> patternArg,
  (function "unDegradeBy" >> return T.unDegradeBy) <*> patternArg,
  (function "degradeOverBy" >> return T.degradeOverBy) <*> literalArg <*> patternArg,
  (function "sometimesBy" >> return T.sometimesBy) <*> patternArg <*> patternTransformationArg,
  (function "sometimes" >> return T.sometimes) <*> patternTransformationArg,
  (function "often" >> return T.often) <*> patternTransformationArg,
  (function "rarely" >> return T.rarely) <*> patternTransformationArg,
  (function "almostNever" >> return T.almostNever) <*> patternTransformationArg,
  (function "almostAlways" >> return T.almostAlways) <*> patternTransformationArg,
  (function "never" >> return T.never) <*> patternTransformationArg,
  (function "always" >> return T.always) <*> patternTransformationArg,
  (function "someCyclesBy" >> return T.someCyclesBy) <*> literalArg <*> patternTransformationArg,
  (function "somecyclesBy" >> return T.somecyclesBy) <*> literalArg <*> patternTransformationArg,
  (function "someCycles" >> return T.someCycles) <*> patternTransformationArg,
  (function "somecycles" >> return T.somecycles) <*> patternTransformationArg,
  (function "substruct'" >> return T.substruct') <*> patternArg,
  (function "repeatCycles" >> return T.repeatCycles) <*> literalArg,
  (function "spaceOut" >> return T.spaceOut) <*> listLiteralArg,
--  (function "fill" >> return T.fill) <*> patternArg, -- removed from tidal-1.0?
  (function "ply" >> return T.ply) <*> patternArg,
  (function "shuffle" >> return T.shuffle) <*> literalArg,
  (function "scramble" >> return T.scramble) <*> literalArg
  ]

simpleDoublePatterns :: Parser (Pattern Double)
simpleDoublePatterns = choice [
  function "rand" >> return T.rand,
  function "sine" >> return T.sine,
  function "saw" >> return T.saw,
  function "isaw" >> return T.isaw,
  function "tri" >> return T.tri,
  function "square" >> return T.square,
  function "cosine" >> return T.cosine
  ]

instance Pattern' Int where
  simplePattern = choice [
    parseBP',
    pure <$> int
    ]
  complexPattern = (atom <*> int) <|> enumComplexPatterns <|> numComplexPatterns <|> intComplexPatterns
  mergeOperator = numMergeOperator
  transformationWithoutArgs = patternTransformationWithoutArgs
  transformationWithArgs = patternTransformationWithArgs
  literal = int

instance Pattern' Integer where
  simplePattern = choice [
    parseBP',
    pure <$> integer
    ]
  complexPattern = (atom <*> integer) <|> enumComplexPatterns <|> numComplexPatterns
  mergeOperator = numMergeOperator
  transformationWithoutArgs = patternTransformationWithoutArgs
  transformationWithArgs = patternTransformationWithArgs
  literal = integer

instance Pattern' Double where
  simplePattern = choice [
    parseBP',
    try $ pure <$> double,
    simpleDoublePatterns
    ]
  complexPattern = (atom <*> double) <|> enumComplexPatterns <|> numComplexPatterns
  mergeOperator = numMergeOperator <|> fractionalMergeOperator
  transformationWithoutArgs = patternTransformationWithoutArgs
  transformationWithArgs = patternTransformationWithArgs
  literal = double

instance Pattern' Time where
  simplePattern = choice [
    parseBP',
    pure <$> literal
    ]
  complexPattern = atom <*> literal <|> numComplexPatterns
  mergeOperator = numMergeOperator <|> fractionalMergeOperator
  transformationWithoutArgs = patternTransformationWithoutArgs
  transformationWithArgs = patternTransformationWithArgs
  literal = choice [
    toRational <$> double,
    fromIntegral <$> integer
    ]

instance Pattern' Arc where
  simplePattern = pure <$> literal
  complexPattern = atom <*> literal
  mergeOperator = choice []
  transformationWithoutArgs = patternTransformationWithoutArgs
  transformationWithArgs = patternTransformationWithArgs
  literal = do
    xs <- parens (commaSep1 literal)
    if length xs == 2 then return (T.Arc (xs!!0) (xs!!1)) else unexpected "Arcs must contain exactly two values"

instance Pattern' String where
  simplePattern = parseBP'
  complexPattern = atom <*> stringLiteral
  mergeOperator = choice [] -- ??
  transformationWithoutArgs = patternTransformationWithoutArgs
  transformationWithArgs = patternTransformationWithArgs
  literal = stringLiteral

fractionalMergeOperator :: Fractional a => Parser (Pattern a -> Pattern a -> Pattern a)
fractionalMergeOperator = op "/" >> return (/)

numMergeOperator :: Num a => Parser (Pattern a -> Pattern a -> Pattern a)
numMergeOperator = choice [
  op "+" >> return (+),
  op "-" >> return (-),
  op "*" >> return (*)
  ]

atom :: Applicative m => Parser (a -> m a)
atom = (function "pure" <|> function "atom" <|> function "return") >> return (pure)

double :: Parser Double
double = choice [
  parens $ symbol "-" >> float >>= return . (* (-1)),
  parens double,
  try float,
  try $ fromIntegral <$> integer
  ]

int :: Parser Int
int = try $ parensOrNot $ fromIntegral <$> integer

function :: String -> Parser ()
function x = reserved x <|> try (parens (function x))

op :: String -> Parser ()
op x = reservedOp x <|> try (parens (op x))

parensOrNot :: Parser a -> Parser a
parensOrNot p = p <|> try (parens (parensOrNot p))

nestedParens :: Parser a -> Parser a
nestedParens p = try (parens p) <|> try (parens (nestedParens p))

applied :: Parser a -> Parser a
applied p = op "$" >> p

appliedOrNot :: Parser a -> Parser a
appliedOrNot p = applied p <|> p

parensOrApplied :: Parser a -> Parser a
parensOrApplied p = try (parens p) <|> try (applied p)

tokenParser :: P.TokenParser a
tokenParser = P.makeTokenParser $ haskellDef {
  P.reservedNames = ["chop","striate","striate'","stut","jux","brak","rev",
    "palindrome","fast","density","slow","iter","iter'","trunc","swingBy","every","whenmod",
    "append","append'","silence","s","sound","n","up","speed","vowel","pan","shape","gain",
    "accelerate","bandf","bandq","begin","coarse","crush","cut","cutoff","delayfeedback",
    "delaytime","delay","end","hcutoff","hresonance","loop","resonance","shape","unit",
    "sine","saw","isaw","fit","irand",
    "tri","square","rand",
    "pure","return","stack","fastcat","slowcat","cat","atom","overlay","run","scan","fast'",
    "fastGap","densityGap","sparsity","rotL","rotR","playFor","every'","foldEvery",
    "cosine","superimpose","trunc","linger","zoom","compress","sliceArc","within","within'",
    "revArc","euclid","euclidFull","euclidInv","distrib","wedge","prr","preplace","prep","preplace1",
    "protate","prot","prot1","discretise","segment","struct","substruct","compressTo",
    "substruct'","stripe","slowstripe","stretch","fit'","chunk","loopFirst","timeLoop","swing",
    "choose","degradeBy","unDegradeBy","degradeOverBy","sometimesBy","sometimes","often",
    "rarely","almostNever","almostAlways","never","always","someCyclesBy","somecyclesBy",
    "someCycles","somecycles","substruct'","repeatCycles","spaceOut","fill","ply","shuffle",
    "scramble","breakUp","degrade","randcat","randStruct","toScale'","toScale","cycleChoose",
    "d1","d2","d3","d4","d5","d6","d7","d8","d9","t1","t2","t3","t4","t5","t6","t7","t8","t9",
    "cps","xfadeIn","note"],
  P.reservedOpNames = ["+","-","*","/","<~","~>","#","|+|","|-|","|*|","|/|","$","\"","|>","<|","|>|","|<|"]
  }

{- Not currently in use
angles :: ParsecT String u Identity a -> ParsecT String u Identity a
angles = P.angles tokenParser
braces :: ParsecT String u Identity a -> ParsecT String u Identity a
braces = P.braces tokenParser
charLiteral :: ParsecT String u Identity Char
charLiteral = P.charLiteral tokenParser
colon :: ParsecT String u Identity String
colon = P.colon tokenParser
comma :: ParsecT String u Identity String
comma = P.comma tokenParser
decimal :: ParsecT String u Identity Integer
decimal = P.decimal tokenParser
dot :: ParsecT String u Identity String
dot = P.dot tokenParser
hexadecimal :: ParsecT String u Identity Integer
hexadecimal = P.hexadecimal tokenParser
identifier :: ParsecT String u Identity String
identifier = P.identifier tokenParser
lexeme :: ParsecT String u Identity a -> ParsecT String u Identity a
lexeme = P.lexeme tokenParser
naturalOrFloat :: ParsecT String u Identity (Either Integer Double)
naturalOrFloat = P.naturalOrFloat tokenParser
natural :: ParsecT String u Identity Integer
natural = P.natural tokenParser
octal :: ParsecT String u Identity Integer
octal = P.octal tokenParser
operator :: ParsecT String u Identity String
operator = P.operator tokenParser
semi :: ParsecT String u Identity String
semi = P.semi tokenParser
semiSep1 :: ParsecT String u Identity a -> ParsecT String u Identity [a]
semiSep1 = P.semiSep1 tokenParser
semiSep :: ParsecT String u Identity a -> ParsecT String u Identity [a]
semiSep = P.semiSep tokenParser
-}

brackets :: ParsecT String u Identity a -> ParsecT String u Identity a
brackets = P.brackets tokenParser
commaSep1 :: ParsecT String u Identity a -> ParsecT String u Identity [a]
commaSep1 = P.commaSep1 tokenParser
commaSep :: ParsecT String u Identity a -> ParsecT String u Identity [a]
commaSep = P.commaSep tokenParser
float :: ParsecT String u Identity Double
float = P.float tokenParser
integer :: ParsecT String u Identity Integer
integer = P.integer tokenParser
parens :: ParsecT String u Identity a -> ParsecT String u Identity a
parens = P.parens tokenParser
reservedOp :: String -> ParsecT String u Identity ()
reservedOp = P.reservedOp tokenParser
reserved :: String -> ParsecT String u Identity ()
reserved = P.reserved tokenParser
stringLiteral :: ParsecT String u Identity String
stringLiteral = P.stringLiteral tokenParser
symbol :: String -> ParsecT String u Identity String
symbol = P.symbol tokenParser
whiteSpace :: ParsecT String u Identity ()
whiteSpace = P.whiteSpace tokenParser

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

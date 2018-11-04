{-# LANGUAGE FlexibleInstances #-}

module Sound.Tidal.MiniTidal (miniTidal,miniTidalIO,miniTidalMain) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.Parsec.Language (haskellDef)
import Data.List (intercalate)
import Data.Bool (bool)
import Data.Ratio
import Control.Monad (forever)
import Sound.Tidal.Context (Pattern,ParamPattern,Enumerable,Parseable,Time,ParamMap,Arc,TPat)
import qualified Sound.Tidal.Context as T

-- This is depended upon by Estuary, and changes to its type will cause problems downstream for Estuary.
miniTidal :: String -> Either ParseError ParamPattern
miniTidal = parse miniTidalParser "miniTidal"

miniTidalParser :: Parser ParamPattern
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

instance Pattern' ParamMap where
  simplePattern = choice []
  complexPattern = specificParamPatterns
  mergeOperator = paramPatternMergeOperator
  transformationWithArgs = paramPatternTransformation <|> patternTransformationWithArgs
  transformationWithoutArgs = patternTransformationWithoutArgs
  literal = choice []

paramPatternMergeOperator :: Parser (ParamPattern -> ParamPattern -> ParamPattern)
paramPatternMergeOperator = choice [
  op "#" >> return (T.#),
  op "|=|" >> return (T.|=|),
  op "|+|" >> return (T.|+|),
  op "|-|" >> return (T.|-|),
  op "|*|" >> return (T.|*|),
  op "|/|" >> return (T.|/|)
  ]

specificParamPatterns :: Parser ParamPattern
specificParamPatterns = choice [
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
  (function "unit" >> return T.unit) <*> patternArg
  ]

paramPatternTransformation :: Parser (ParamPattern -> ParamPattern)
paramPatternTransformation = choice [
  function "chop" >> patternArg >>= return . T.chop,
  function "striate" >> patternArg >>= return . T.striate,
  (function "striate'" >> return T.striate') <*> patternArg <*> patternArg,
  (function "stut" >> return T.stut) <*> patternArg <*> patternArg <*> patternArg,
  function "jux" >> patternTransformationArg >>= return . T.jux
  ]

patternTransformationArg :: Pattern' a => Parser (Pattern a -> Pattern a)
patternTransformationArg = appliedOrNot transformationWithoutArgs <|> parensOrApplied transformationWithArgs

patternTransformationWithoutArgs :: Pattern' a => Parser (Pattern a -> Pattern a)
patternTransformationWithoutArgs = choice [
  function "brak" >> return T.brak,
  function "rev" >> return T.rev,
  function "palindrome" >> return T.palindrome,
  function "stretch" >> return T.stretch,
  function "loopFirst" >> return T.loopFirst,
  function "breakUp" >> return T.breakUp,
  function "degrade" >> return T.degrade
  ]

patternTransformationWithArgs :: Pattern' a => Parser (Pattern a -> Pattern a)
patternTransformationWithArgs = parensOrNot $ choice [
  function "fast" >> patternArg >>= return . T.fast,
  function "fast'" >> patternArg >>= return . T.fast',
  function "density" >> patternArg >>= return . T.density,
  function "slow" >> patternArg >>= return . T.slow,
  function "iter" >> patternArg >>= return . T.iter,
  function "iter'" >> patternArg >>= return . T.iter',
  function "trunc" >> patternArg >>= return . T.trunc,
  (function "swingBy" >> return T.swingBy) <*> patternArg <*> patternArg,
  (function "append" >> return T.append) <*> patternArg,
  (function "append'" >> return T.append') <*> patternArg,
  (function "every" >> return T.every) <*> patternArg <*> patternTransformationArg,
  (function "every'" >> return T.every') <*> patternArg <*> patternArg <*> patternTransformationArg,
  (function "whenmod" >> return T.whenmod) <*> int <*> int <*> patternTransformationArg,
  (function "overlay" >> return T.overlay) <*> patternArg,
  (function "fastGap" >> return T.fastGap) <*> literalArg,
  (function "densityGap" >> return T.densityGap) <*> literalArg,
  (function "sparsity" >> return T.sparsity) <*> patternArg,
  (function "slow'" >> return T.slow') <*> patternArg,
  (function "rotL" >> return T.rotL) <*> literalArg,
  (function "rotR" >> return T.rotR) <*> literalArg,
  (function "playFor" >> return T.playFor) <*> literalArg <*> literalArg,
  (function "foldEvery" >> return T.foldEvery) <*> listLiteralArg <*> patternTransformationArg,
  (function "superimpose" >> return T.superimpose) <*> patternTransformationArg,
  (function "trunc" >> return T.trunc) <*> patternArg,
  (function "linger" >> return T.linger) <*> patternArg,
  (function "zoom" >> return T.zoom) <*> literalArg,
  (function "compress" >> return T.compress) <*> literalArg,
  (function "sliceArc" >> return T.sliceArc) <*> literalArg,
  (function "within" >> return T.within) <*> literalArg <*> patternTransformationArg,
  (function "within'" >> return T.within') <*> literalArg <*> patternTransformationArg,
  (function "revArc" >> return T.revArc) <*> literalArg,
  (function "e" >> return T.e) <*> patternArg <*> patternArg,
  (function "e'" >> return T.e') <*> patternArg <*> patternArg,
  (function "einv" >> return T.einv) <*> patternArg <*> patternArg,
  (function "distrib" >> return T.distrib) <*> listPatternArg,
  (function "efull" >> return T.efull) <*> patternArg <*> patternArg <*> patternArg,
  (function "wedge" >> return T.wedge) <*> literalArg <*> patternArg,
  (function "prr" >> return T.prr) <*> literalArg <*> literalArg <*> patternArg,
  (function "preplace" >> return T.preplace) <*> literalArg <*> patternArg,
  (function "prep" >> return T.prep) <*> literalArg <*> patternArg,
  (function "preplace1" >> return T.preplace1) <*> patternArg,
  (function "protate" >> return T.protate) <*> literalArg <*> literalArg,
  (function "prot" >> return T.prot) <*> literalArg <*> literalArg,
  (function "prot1" >> return T.prot1) <*> literalArg,
  (function "discretise" >> return T.discretise) <*> literalArg,
  (function "discretise'" >> return T.discretise') <*> patternArg,
  (function "struct" >> return T.struct) <*> patternArg,
  (function "substruct" >> return T.substruct) <*> patternArg,
  (function "compressTo" >> return T.compressTo) <*> literalArg,
  (function "substruct'" >> return T.substruct') <*> patternArg,
  (function "slowstripe" >> return T.slowstripe) <*> patternArg,
  (function "fit'" >> return T.fit') <*> patternArg <*> literalArg <*> patternArg <*> patternArg,
  (function "chunk" >> return T.chunk) <*> literalArg <*> patternTransformationArg,
  (function "timeLoop" >> return T.timeLoop) <*> patternArg,
  (function "swing" >> return T.swing) <*> patternArg,
  (function "degradeBy" >> return T.degradeBy) <*> patternArg,
  (function "unDegradeBy" >> return T.unDegradeBy) <*> patternArg,
  (function "degradeOverBy" >> return T.degradeOverBy) <*> literalArg <*> patternArg,
  (function "sometimesBy" >> return T.sometimesBy) <*> literalArg <*> patternTransformationArg,
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
  (function "fill" >> return T.fill) <*> patternArg,
  (function "ply" >> return T.ply) <*> patternArg,
  (function "shuffle" >> return T.shuffle) <*> literalArg,
  (function "scramble" >> return T.scramble) <*> literalArg
  ]

simpleDoublePatterns :: Parser (Pattern Double)
simpleDoublePatterns = choice [
  function "rand" >> return T.rand,
  function "sinewave1" >> return T.sinewave1,
  function "sinewave" >> return T.sinewave,
  function "sine1" >> return T.sine1,
  function "sine" >> return T.sine,
  function "sawwave1" >> return T.sawwave1,
  function "sawwave" >> return T.sawwave,
  function "saw1" >> return T.saw1,
  function "saw" >> return T.saw,
  function "triwave1" >> return T.triwave1,
  function "triwave" >> return T.triwave,
  function "tri1" >> return T.tri1,
  function "tri" >> return T.tri,
  function "squarewave1" >> return T.squarewave1,
  function "square1" >> return T.square1,
  function "square" >> return T.square,
  function "squarewave" >> return T.squarewave,
  function "cosine" >> return T.cosine
  ]

instance Pattern' Int where
  simplePattern = choice [
    pure <$> int,
    p'
    ]
  complexPattern = (atom <*> int) <|> enumComplexPatterns <|> numComplexPatterns <|> intComplexPatterns
  mergeOperator = numMergeOperator
  transformationWithoutArgs = patternTransformationWithoutArgs
  transformationWithArgs = patternTransformationWithArgs
  literal = int

instance Pattern' Integer where
  simplePattern = choice [
    pure <$> integer,
    p'
    ]
  complexPattern = (atom <*> integer) <|> enumComplexPatterns <|> numComplexPatterns
  mergeOperator = numMergeOperator
  transformationWithoutArgs = patternTransformationWithoutArgs
  transformationWithArgs = patternTransformationWithArgs
  literal = integer

instance Pattern' Double where
  simplePattern = choice [
    p',
    pure <$> double,
    simpleDoublePatterns
    ]
  complexPattern = (atom <*> double) <|> enumComplexPatterns <|> numComplexPatterns
  mergeOperator = numMergeOperator <|> fractionalMergeOperator
  transformationWithoutArgs = patternTransformationWithoutArgs
  transformationWithArgs = patternTransformationWithArgs
  literal = double

instance Pattern' Time where
  simplePattern = choice [
    pure <$> literal,
    p'
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
    if length xs == 2 then return (xs!!0,xs!!1) else unexpected "Arcs must contain exactly two values"

instance Pattern' String where
  simplePattern = p'
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
double = try $ parensOrNot $ choice [float,fromIntegral <$> integer]

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
    "sinewave1","sinewave","sine1","sine","sawwave1","sawwave","saw1","saw","fit","irand",
    "triwave1","triwave","tri1","tri","squarewave1","square1","square","squarewave","rand",
    "pure","return","stack","fastcat","slowcat","cat","atom","overlay","run","scan","fast'",
    "fastGap","densityGap","sparsity","slow'","rotL","rotR","playFor","every'","foldEvery",
    "cosine","superimpose","trunc","linger","zoom","compress","sliceArc","within","within'",
    "revArc","e","e'","einv","distrib","efull","wedge","prr","preplace","prep","preplace1",
    "protate","prot","prot1","discretise","discretise'","struct","substruct","compressTo",
    "substruct'","stripe","slowstripe","stretch","fit'","chunk","loopFirst","timeLoop","swing",
    "choose","degradeBy","unDegradeBy","degradeOverBy","sometimesBy","sometimes","often",
    "rarely","almostNever","almostAlways","never","always","someCyclesBy","somecyclesBy",
    "someCycles","somecycles","substruct'","repeatCycles","spaceOut","fill","ply","shuffle",
    "scramble","breakUp","degrade","randcat","randStruct","toScale'","toScale","cycleChoose",
    "d1","d2","d3","d4","d5","d6","d7","d8","d9","t1","t2","t3","t4","t5","t6","t7","t8","t9",
    "cps","xfadeIn"],
  P.reservedOpNames = ["+","-","*","/","<~","~>","#","|=|","|+|","|-|","|*|","|/|","$","\""]
  }

identifier = P.identifier tokenParser
reserved = P.reserved tokenParser
operator = P.operator tokenParser
reservedOp = P.reservedOp tokenParser
charLiteral = P.charLiteral tokenParser
stringLiteral = P.stringLiteral tokenParser
natural = P.natural tokenParser
integer = P.integer tokenParser
float = P.float tokenParser
naturalOrFloat = P.naturalOrFloat tokenParser
decimal = P.decimal tokenParser
hexadecimal = P.hexadecimal tokenParser
octal = P.octal tokenParser
symbol = P.symbol tokenParser
lexeme = P.lexeme tokenParser
whiteSpace = P.whiteSpace tokenParser
parens = P.parens tokenParser
braces = P.braces tokenParser
angles = P.angles tokenParser
brackets = P.brackets tokenParser
semi = P.semi tokenParser
comma = P.comma tokenParser
colon = P.colon tokenParser
dot = P.dot tokenParser
semiSep = P.semiSep tokenParser
semiSep1 = P.semiSep1 tokenParser
commaSep = P.commaSep tokenParser
commaSep1 = P.commaSep1 tokenParser

p' :: (Enumerable a, Parseable' a) => Parser (Pattern a)
p' = parseTPat' >>= return . T.toPat

-- The class Parseable' and instances below basically just duplicate the class
-- Parseable in Tidal but incorporates the parser into our parser instead of
-- running it. Probably this idea should just be part of Parseable instead.

class Parseable' a where
  parseTPat' :: Parser (TPat a)

instance Parseable' Double where
  parseTPat' = parseRhythm' T.pDouble

instance Parseable' String where
  parseTPat' = parseRhythm' T.pVocable

instance Parseable' Bool where
  parseTPat' = parseRhythm' T.pBool

instance Parseable' Int where
  parseTPat' = parseRhythm' T.pIntegral

instance Parseable' Integer where
  parseTPat' = parseRhythm' T.pIntegral

instance Parseable' Rational where
  parseTPat' = parseRhythm' T.pRational

parseRhythm' :: Parseable a => Parser (TPat a) -> Parser (TPat a)
parseRhythm' f = do
  reservedOp "\""
  x <- T.pSequence f'
  reservedOp "\""
  return x
  where f' = f
             <|> do symbol "~" <?> "rest"
                    return T.TPat_Silence

-- a temporary model of a Tidal runtime environment, likely obsolete given
-- recent work on Tidal...
data Tidal = Tidal {
  ds :: [ParamPattern -> IO ()],
  ts :: [(Time -> [ParamPattern] -> ParamPattern) -> ParamPattern -> IO ()],
  cps' :: Double -> IO ()
  }

miniTidalIO :: Tidal -> String -> Either ParseError (IO ())
miniTidalIO tidal = parse (miniTidalIOParser tidal) "miniTidal"

miniTidalIOParser :: Tidal -> Parser (IO ())
miniTidalIOParser tidal = whiteSpace >> choice [
  eof >> return (return ()),
  dParser tidal <*> patternArg,
  tParser tidal <*> transitionArg <*> patternArg,
  (reserved "cps" >> return (cps' tidal)) <*> literalArg
  ]

dParser :: Tidal -> Parser (ParamPattern -> IO ())
dParser tidal = choice [
  reserved "d1" >> return ((ds tidal)!!0),
  reserved "d2" >> return ((ds tidal)!!1),
  reserved "d3" >> return ((ds tidal)!!2),
  reserved "d4" >> return ((ds tidal)!!3),
  reserved "d5" >> return ((ds tidal)!!4),
  reserved "d6" >> return ((ds tidal)!!5),
  reserved "d7" >> return ((ds tidal)!!6),
  reserved "d8" >> return ((ds tidal)!!7),
  reserved "d9" >> return ((ds tidal)!!8)
  ]

tParser :: Tidal -> Parser ((Time -> [ParamPattern] -> ParamPattern) -> ParamPattern -> IO ())
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
  ]

transitionArg :: Parser (Time -> [ParamPattern] -> ParamPattern)
transitionArg = choice [
  parensOrApplied $ (reserved "xfadeIn" >> return T.xfadeIn) <*> literalArg
  ]

miniTidalMain :: IO ()
miniTidalMain = do
  putStrLn "miniTidal"
  (cps, getNow) <- T.cpsUtils
  (d1,t1) <- T.superDirtSetters getNow
  (d2,t2) <- T.superDirtSetters getNow
  (d3,t3) <- T.superDirtSetters getNow
  (d4,t4) <- T.superDirtSetters getNow
  (d5,t5) <- T.superDirtSetters getNow
  (d6,t6) <- T.superDirtSetters getNow
  (d7,t7) <- T.superDirtSetters getNow
  (d8,t8) <- T.superDirtSetters getNow
  (d9,t9) <- T.superDirtSetters getNow
  let tidal = Tidal {
    ds = [d1,d2,d3,d4,d5,d6,d7,d8,d9],
    ts = [t1,t2,t3,t4,t5,t6,t7,t8,t9],
    cps' = cps
  }
  forever $ do
    cmd <- miniTidalIO tidal <$> getLine
    either (\x -> putStrLn $ "error: " ++ show x) id cmd

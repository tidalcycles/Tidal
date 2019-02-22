{-# LANGUAGE FlexibleInstances, TemplateHaskell #-}

module Sound.Tidal.MiniTidal (miniTidal,miniTidalIO,main) where

import           Text.Parsec.Prim (parserZero)
import           Text.ParserCombinators.Parsec
import           Control.Monad (forever)
import           Control.Applicative (liftA2)
import           Language.Haskell.TH

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
  nestedParens $ chainl1 pattern mergeOperator,
  transformation <*> patternArg,
  genericComplexPattern,
  complexPattern,
  simplePattern,
  silence
  ]

patternArg :: MiniTidal a => Parser (Pattern a)
patternArg = choice [
  try $ parensOrApplied $ chainl1 pattern mergeOperator,
  try $ parensOrApplied $ transformation <*> patternArg,
  try $ parensOrApplied genericComplexPattern,
  try $ parensOrApplied complexPattern,
  try $ appliedOrNot simplePattern,
  appliedOrNot silence
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

--  d1 $ spread ($) [density 2, rev, slow 2, striate 3, (# speed "0.8")] $ sound "[bd*2 [~ bd]] [sn future]*2 cp jvbass*4"
--  spread  ((a -> b) -> a -> b) -> [ControlPattern -> ControlPattern] -> ControlPattern -> ControlPattern


silence :: Parser (Pattern a)
silence = function "silence" >> return T.silence

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
  $(opParser "#"),
  $(opParser "|>"),
  $(opParser "<|"),
  $(opParser "|>"),
  $(opParser "|<|"),
  $(opParser "|+|"),
  $(opParser "|-|"),
  $(opParser "|*|"),
  $(opParser "|/|")
  ]

specificControlPatterns :: Parser ControlPattern
specificControlPatterns = choice [
  try $ parens specificControlPatterns,
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

genericComplexPattern :: MiniTidal a => Parser (Pattern a)
genericComplexPattern = choice [
  try $ parens genericComplexPattern,
  lp_p <*> listPatternArg,
  l_p <*> listLiteralArg
  ]

p_p_noArgs :: Parser (Pattern a -> Pattern a)
p_p_noArgs  = choice [
  function "brak" >> return T.brak,
  function "rev" >> return T.rev,
  function "palindrome" >> return T.palindrome,
  function "stretch" >> return T.stretch,
  function "loopFirst" >> return T.loopFirst,
  function "degrade" >> return T.degrade
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

lt_p_p = choice [
  try $ parens lt_p_p,
  spreads <*> (nestedParens $ reservedOp "$" >> return ($))
  ]

l_p :: MiniTidal a => Parser ([a] -> Pattern a)
l_p = choice [
  function "listToPat" >> return T.listToPat,
  function "choose" >> return T.choose,
  function "cycleChoose" >> return T.cycleChoose
  ]

lp_p :: MiniTidal a => Parser ([Pattern a] -> Pattern a)
lp_p = choice [
  function "stack" >> return T.stack,
  function "fastcat" >> return T.fastcat,
  function "slowcat" >> return T.slowcat,
  function "cat" >> return T.cat,
  function "randcat" >> return T.randcat
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
  function "overlay" >> return T.overlay,
  function "append" >> return T.append,
  vTime_p_p_p <*> literalArg,
  pInt_p_p_p <*> patternArg
  ]

pTime_p_p = choice [
  try $ parens pTime_p_p,
  function "fast" >> return T.fast,
  function "fastGap" >> return T.fastGap,
  function "density" >> return T.density,
  function "slow" >> return T.slow,
  function "trunc" >> return T.trunc,
  function "fastGap" >> return T.fastGap,
  function "densityGap" >> return T.densityGap,
  function "sparsity" >> return T.sparsity,
  function "trunc" >> return T.trunc,
  function "linger" >> return T.linger,
  function "segment" >> return T.segment,
  function "discretise" >> return T.discretise,
  function "timeLoop" >> return T.timeLoop,
  function "swing" >> return T.swing,
  pTime_pTime_p_p <*> patternArg
  ]

lTime_p_p = choice [
  try $ parens lTime_p_p,
  spreads <*> parens vTime_p_p -- re: spread
  ]

spreads = choice [
  function "spread" >> return T.spread,
  function "slowspread" >> return T.slowspread,
  function "fastspread" >> return T.fastspread
  ]

pInt_p_p = choice [
  try $ parens pInt_p_p,
  function "iter" >> return T.iter,
  function "iter'" >> return T.iter',
  function "ply" >> return T.ply,
  function "substruct'" >> return T.substruct',
  function "slowstripe" >> return T.slowstripe,
  function "shuffle" >> return T.shuffle,
  function "scramble" >> return T.scramble,
  pInt_pInt_p_p <*> patternArg
  ]

pString_p_p = function "substruct" >> return T.substruct

pDouble_p_p = choice [
  try $ parens pDouble_p_p,
  function "degradeBy" >> return T.degradeBy,
  function "unDegradeBy" >> return T.unDegradeBy,
  vInt_pDouble_p_p <*> literalArg
  ]

vTime_p_p = choice [
  try $ parens vTime_p_p,
  function "rotL" >> return T.rotL,
  function "rotR" >> return T.rotR,
  vTime_vTime_p_p <*> literalArg
  ]

vInt_p_p = choice [
  function "repeatCycles" >> return T.repeatCycles
  ]

vTimeTime_p_p = choice [
  function "compress" >> return T.compressArc,
  function "zoom" >> return T.zoomArc,
  function "compressTo" >> return T.compressArcTo
  ]

t_p_p = choice [
  try $ parens t_p_p,
  function "sometimes" >> return T.sometimes,
  function "often" >> return T.often,
  function "rarely" >> return T.rarely,
  function "almostNever" >> return T.almostNever,
  function "almostAlways" >> return T.almostAlways,
  function "never" >> return T.never,
  function "always" >> return T.always,
  function "superimpose" >> return T.superimpose,
  function "someCycles" >> return T.someCycles,
  function "somecycles" >> return T.somecycles,
  pInt_t_p_p <*> patternArg,
  pDouble_t_p_p <*> patternArg,
  lvInt_t_p_p <*> listLiteralArg,
  vInt_t_p_p <*> literalArg,
  vDouble_t_p_p <*> literalArg
  ]

lvTime_p_p = function "spaceOut" >> return T.spaceOut

lpInt_p_p = function "distrib" >> return T.distrib

lp_p_p :: MiniTidal a => Parser ([Pattern a] -> Pattern a -> Pattern a)
lp_p_p = choice [
  try $ parens lp_p_p,
  try $ spreads <*> parens p_p_p
  ]

l_pInt_p = choice [
  try $ parens l_pInt_p,
  vInt_l_pInt_p <*> literalArg
  ]

vInt_l_pInt_p = function "fit" >> return T.fit

vTime_p_p_p = function "wedge" >> return T.wedge

vInt_pDouble_p_p = function "degradeOverBy" >> return T.degradeOverBy

pInt_t_p_p = choice [
  try $ parens pInt_t_p_p,
  function "every" >> return T.every,
  pInt_pInt_t_p_p <*> patternArg
  ]

pDouble_t_p_p = function "sometimesBy" >> return T.sometimesBy

lvInt_t_p_p = function "foldEvery" >> return T.foldEvery

vTime_vTime_p_p = function "playFor" >> return T.playFor

vTimeTime_t_p_p = function "within" >> return T.withinArc

vInt_t_p_p = choice [
  try $ parens vInt_t_p_p,
  function "chunk" >> return T.chunk,
  vInt_vInt_t_p_p <*> literalArg
  ]

vDouble_t_p_p = choice [
  function "someCyclesBy" >> return T.someCyclesBy,
  function "somecyclesBy" >> return T.somecyclesBy
  ]

pInt_pInt_p_p = choice [
  try $ parens pInt_pInt_p_p,
  function "euclid" >> return T.euclid,
  function "euclidInv" >> return T.euclidInv,
  vInt_pInt_pInt_p_p <*> literalArg
  ]

pTime_pTime_p_p = function "swingBy" >> return T.swingBy

pInt_pInt_t_p_p = function "every'" >> return T.every'

vInt_vInt_t_p_p = function "whenmod" >> return T.whenmod

pInt_p_p_p = choice [
  try $ parens pInt_p_p_p,
  pInt_pInt_p_p_p <*> patternArg
  ]

pInt_pInt_p_p_p = function "euclidFull" >> return T.euclidFull

vInt_pInt_pInt_p_p = choice [
  try $ parens vInt_pInt_pInt_p_p,
  pTime_vInt_pInt_pInt_p_p <*> patternArg
  ]

pTime_vInt_pInt_pInt_p_p = function "fit'" >> return T.fit'

pControl_pControl = choice [
  try $ parens pControl_pControl,
  pInt_pControl_pControl <*> patternArg,
  pDouble_pControl_pControl <*> patternArg,
  pTime_pControl_pControl <*> patternArg,
  tControl_pControl_pControl <*> transformationArg
  ]

tControl_pControl_pControl = function "jux" >> return T.jux

pInt_pControl_pControl = choice [
  function "chop" >> return T.chop,
  function "striate" >> return T.striate
  ]

pDouble_pControl_pControl = choice [
  try $ parens pDouble_pControl_pControl,
  pInt_pDouble_pControl_pControl <*> patternArg
  ]

pInt_pDouble_pControl_pControl = function "striate'" >> return T.striate'

pTime_pControl_pControl = choice [
  try $ parens pTime_pControl_pControl,
  pDouble_pTime_pControl_pControl <*> patternArg
  ]

pDouble_pTime_pControl_pControl = choice [
  try $ parens pDouble_pTime_pControl_pControl,
  pInteger_pDouble_pTime_pControl_pControl <*> patternArg
  ]

pInteger_pDouble_pTime_pControl_pControl = function "stut" >> return T.stut

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

instance MiniTidal String where
  literal = stringLiteral
  simplePattern = parseBP'
  transformationWithArguments = p_p_noArgs
  transformationWithoutArguments = p_p
  complexPattern = atom <*> stringLiteral
  mergeOperator = parserZero
  binaryFunctions = parserZero

fractionalMergeOperator :: Fractional a => Parser (Pattern a -> Pattern a -> Pattern a)
fractionalMergeOperator = op "/" >> return (/)

numMergeOperator :: Num a => Parser (Pattern a -> Pattern a -> Pattern a)
numMergeOperator = choice [
  op "+" >> return (+),
  op "-" >> return (-),
  op "*" >> return (*)
  ]

enumComplexPatterns :: (Enum a, Num a, MiniTidal a) => Parser (Pattern a)
enumComplexPatterns = choice [
  (function "run" >> return T.run) <*> patternArg,
  (function "scan" >> return T.scan) <*> patternArg
  ]

numComplexPatterns :: (Num a, MiniTidal a) => Parser (Pattern a)
numComplexPatterns = choice [
  (function "irand" >> return T.irand) <*> literal,
  (function "toScale'" >> return T.toScale') <*> literalArg <*> listLiteralArg <*> patternArg,
  (function "toScale" >> return T.toScale) <*> listLiteralArg <*> patternArg
  ]

intComplexPatterns :: Parser (Pattern Int)
intComplexPatterns = choice [
  (function "randStruct" >> return T.randStruct) <*> literalArg
  ]

atom :: Applicative m => Parser (a -> m a)
atom = (function "pure" <|> function "atom" <|> function "return") >> return (pure)




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

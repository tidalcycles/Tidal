module Sound.Tidal.MiniTidal.Token where

import           Data.Functor.Identity (Identity)
import           Text.Parsec.Prim (ParsecT,parserZero)
import           Text.ParserCombinators.Parsec
import           Text.Parsec.Language (haskellDef)
import qualified Text.ParserCombinators.Parsec.Token as P

tokenParser :: P.TokenParser a
tokenParser = P.makeTokenParser $ haskellDef {
  P.reservedNames = ["chop","striate","striate'","stut","jux","brak","rev",
    "palindrome","fast","density","slow","iter","iter'","trunc","swingBy","every","whenmod",
    "append","append'","silence","s","sound","n","up","speed","vowel","pan","shape","gain",
    "accelerate","bandf","bandq","begin","coarse","crush","cut","cutoff","delayfeedback",
    "delaytime","delay","end","hcutoff","hresonance","loop","resonance","shape","unit",
    "sine","saw","isaw","fit","irand","tri","square","rand",
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
    "cps","xfadeIn","note","spread","slowspread","fastspread"],
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

functionParser :: String -> Parser ()
functionParser x = reserved x <|> try (parens (functionParser x))

opParser :: String -> Parser ()
opParser x = reservedOp x <|> try (parens (opParser x))

double :: Parser Double
double = choice [
  parens $ symbol "-" >> float >>= return . (* (-1)),
  parens double,
  try float,
  try $ fromIntegral <$> integer
  ]

int :: Parser Int
int = try $ parensOrNot $ fromIntegral <$> integer

parensOrNot :: Parser a -> Parser a
parensOrNot p = p <|> try (parens (parensOrNot p))

nestedParens :: Parser a -> Parser a
nestedParens p = try (parens p) <|> try (parens (nestedParens p))

applied :: Parser a -> Parser a
applied p = opParser "$" >> p

appliedOrNot :: Parser a -> Parser a
appliedOrNot p = applied p <|> p

parensOrApplied :: Parser a -> Parser a
parensOrApplied p = try (parens p) <|> try (applied p)

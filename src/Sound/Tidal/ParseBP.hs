{-# LANGUAGE OverloadedStrings, FlexibleInstances, CPP, DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans -fno-warn-unused-do-bind #-}

module Sound.Tidal.ParseBP where

{-
    ParseBP.hs - Parser for Tidal's "mini-notation", inspired by
      Bernard Bel's BP2 (Bol Processor 2) system.
    Copyright (C) 2020, Alex McLean and contributors

    This library is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this library.  If not, see <http://www.gnu.org/licenses/>.
-}

import           Control.Applicative ()
import qualified Control.Exception as E
import           Data.Bifunctor (first)
import           Data.Colour
import           Data.Colour.Names
import           Data.Functor.Identity (Identity)
import           Data.List (intercalate)
import           Data.Maybe
import           Data.Ratio
import           Data.Typeable (Typeable)
import           GHC.Exts ( IsString(..) )
import           Text.Parsec.Error
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Language ( haskellDef )
import qualified Text.ParserCombinators.Parsec.Token as P
import qualified Text.Parsec.Prim
import           Sound.Tidal.Pattern
import           Sound.Tidal.UI
import           Sound.Tidal.Core
import           Sound.Tidal.Chords (chordTable)
import           Sound.Tidal.Utils (fromRight)

data TidalParseError = TidalParseError {parsecError :: ParseError,
                                        code :: String
                                       }
  deriving (Eq, Typeable)

instance E.Exception TidalParseError

instance Show TidalParseError where
  show err = "Syntax error in sequence:\n  \"" ++ code err ++ "\"\n  " ++ pointer ++ "  " ++ message
    where pointer = replicate (sourceColumn $ errorPos perr) ' ' ++ "^"
          message = showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" $ errorMessages perr
          perr = parsecError err

type MyParser = Text.Parsec.Prim.Parsec String Int

-- | AST representation of patterns

data TPat a = TPat_Atom (Maybe ((Int, Int), (Int, Int))) a
            | TPat_Fast (TPat Time) (TPat a)
            | TPat_Slow (TPat Time) (TPat a)
            | TPat_DegradeBy Int Double (TPat a)
            | TPat_CycleChoose Int [TPat a]
            | TPat_Euclid (TPat Int) (TPat Int) (TPat Int) (TPat a)
            | TPat_Stack [TPat a]
            | TPat_Polyrhythm (Maybe (TPat Rational)) [TPat a]
            | TPat_Seq [TPat a]
            | TPat_Silence
            | TPat_Foot
            | TPat_Elongate Rational (TPat a)
            | TPat_Repeat Int (TPat a)
            | TPat_EnumFromTo (TPat a) (TPat a)
            | TPat_Var String
            deriving (Show, Functor)

tShowList :: (Show a) => [TPat a] -> String
tShowList vs = "[" ++ intercalate "," (map tShow vs) ++ "]"

tShow :: (Show a) => TPat a -> String
tShow (TPat_Atom _ v) = "pure " ++ show v
tShow (TPat_Fast t v) = "fast " ++ show t ++ " $ " ++ tShow v
tShow (TPat_Slow t v) = "slow " ++ show t ++ " $ " ++ tShow v
-- TODO - should be _degradeByUsing, but needs a simplified version..
tShow (TPat_DegradeBy _ r v) = "degradeBy " ++ show r ++ " $ " ++ tShow v
-- TODO - ditto
tShow (TPat_CycleChoose _ vs) = "cycleChoose " ++ tShowList vs

tShow (TPat_Euclid a b c v) = "doEuclid (" ++ intercalate ") (" (map tShow [a,b,c])  ++ ") $ " ++ tShow v
tShow (TPat_Stack vs) = "stack " ++ tShowList vs

tShow (TPat_Polyrhythm mSteprate vs) = "stack [" ++ intercalate ", " (map adjust_speed pats) ++ "]"
  where adjust_speed (sz, pat) = "(fast (" ++ (steprate ++ "/" ++ show sz) ++ ") $ " ++ pat ++ ")"
        steprate :: String
        steprate = maybe base_first tShow mSteprate
        base_first | null pats = "0"
                   | otherwise = show $ fst $ head pats
        pats = map steps_tpat vs

tShow (TPat_Seq vs) = snd $ steps_seq vs

tShow TPat_Silence = "silence"
tShow (TPat_EnumFromTo a b) = "unwrap $ fromTo <$> (" ++ tShow a ++ ") <*> (" ++ tShow b ++ ")"
tShow (TPat_Var s) = "getControl " ++ s
tShow a = "can't happen? " ++ show a


toPat :: (Parseable a, Enumerable a) => TPat a -> Pattern a
toPat = \case
   TPat_Atom (Just loc) x -> setContext (Context [loc]) $ pure x
   TPat_Atom Nothing x -> pure x
   TPat_Fast t x -> fast (toPat t) $ toPat x
   TPat_Slow t x -> slow (toPat t) $ toPat x
   TPat_DegradeBy seed amt x -> _degradeByUsing (rotL (0.0001 * fromIntegral seed) rand) amt $ toPat x
   TPat_CycleChoose seed xs -> unwrap $ segment 1 $ chooseBy (rotL (0.0001 * fromIntegral seed) rand) $ map toPat xs
   TPat_Euclid n k s thing -> doEuclid (toPat n) (toPat k) (toPat s) (toPat thing)
   TPat_Stack xs -> stack $ map toPat xs
   TPat_Silence -> silence
   TPat_EnumFromTo a b -> unwrap $ fromTo <$> toPat a <*> toPat b
   TPat_Foot -> error "Can't happen, feet are pre-processed."
   TPat_Polyrhythm mSteprate ps -> stack $ map adjust_speed pats
     where adjust_speed (sz, pat) = fast ((/sz) <$> steprate) pat
           pats = map resolve_tpat ps
           steprate :: Pattern Rational
           steprate = (maybe base_first toPat mSteprate)
           base_first | null pats = pure 0
                      | otherwise = pure $ fst $ head pats
   TPat_Seq xs -> snd $ resolve_seq xs
   TPat_Var s -> getControl s
   _ -> silence

resolve_tpat :: (Enumerable a, Parseable a) => TPat a -> (Rational, Pattern a)
resolve_tpat (TPat_Seq xs) = resolve_seq xs
resolve_tpat a = (1, toPat a)

resolve_seq :: (Enumerable a, Parseable a) => [TPat a] -> (Rational, Pattern a)
resolve_seq xs = (total_size, timeCat sized_pats)
  where sized_pats = map (toPat <$>) $ resolve_size xs
        total_size = sum $ map fst sized_pats

resolve_size :: [TPat a] -> [(Rational, TPat a)]
resolve_size [] = []
resolve_size ((TPat_Elongate r p):ps) = (r, p):resolve_size ps
resolve_size ((TPat_Repeat n p):ps) = replicate n (1,p) ++ resolve_size ps
resolve_size (p:ps) = (1,p):resolve_size ps


steps_tpat :: (Show a) => TPat a -> (Rational, String)
steps_tpat (TPat_Seq xs) = steps_seq xs
steps_tpat a = (1, tShow a)

steps_seq :: (Show a) => [TPat a] -> (Rational, String)
steps_seq xs = (total_size, "timeCat [" ++ intercalate "," (map (\(r,s) -> "(" ++ show r ++ ", " ++ s ++ ")") sized_pats) ++ "]")
  where sized_pats = steps_size xs
        total_size = sum $ map fst sized_pats

steps_size :: Show a => [TPat a] -> [(Rational, String)]
steps_size [] = []
steps_size ((TPat_Elongate r p):ps) = (r, tShow p):steps_size ps
steps_size ((TPat_Repeat n p):ps) = replicate n (1, tShow p) ++ steps_size ps
steps_size (p:ps) = (1,tShow p):steps_size ps

parseBP :: (Enumerable a, Parseable a) => String -> Either ParseError (Pattern a)
parseBP s = toPat <$> parseTPat s

parseBP_E :: (Enumerable a, Parseable a) => String -> Pattern a
parseBP_E s = toE parsed
  where
    parsed = parseTPat s
    -- TODO - custom error
    toE (Left e) = E.throw $ TidalParseError {parsecError = e, code = s}
    toE (Right tp) = toPat tp

parseTPat :: Parseable a => String -> Either ParseError (TPat a)
parseTPat = parseRhythm tPatParser

cP :: (Enumerable a, Parseable a) => String -> Pattern a
cP s = innerJoin $ parseBP_E <$> _cX_ getS s

class Parseable a where
  tPatParser :: MyParser (TPat a)
  doEuclid :: Pattern Int -> Pattern Int -> Pattern Int -> Pattern a -> Pattern a
  getControl :: String -> Pattern a
  getControl _ = silence

class Enumerable a where
  fromTo :: a -> a -> Pattern a
  fromThenTo :: a -> a -> a -> Pattern a

instance Parseable Char where
  tPatParser = pChar
  doEuclid = euclidOff

instance Enumerable Char where
  fromTo = enumFromTo'
  fromThenTo a b c = fastFromList [a,b,c]

instance Parseable Double where
  tPatParser = pDouble
  doEuclid = euclidOff
  getControl = cF_

instance Enumerable Double where
  fromTo = enumFromTo'
  fromThenTo = enumFromThenTo'

instance Parseable Note where
  tPatParser = pNote
  doEuclid = euclidOff
  getControl = cN_

instance Enumerable Note where
  fromTo = enumFromTo'
  fromThenTo = enumFromThenTo'

instance Parseable String where
  tPatParser = pVocable
  doEuclid = euclidOff
  getControl = cS_

instance Enumerable String where
  fromTo a b = fastFromList [a,b]
  fromThenTo a b c = fastFromList [a,b,c]

instance Parseable Bool where
  tPatParser = pBool
  doEuclid = euclidOffBool
  getControl = cB_

instance Enumerable Bool where
  fromTo a b = fastFromList [a,b]
  fromThenTo a b c = fastFromList [a,b,c]

instance Parseable Int where
  tPatParser = pIntegral
  doEuclid = euclidOff
  getControl = cI_

instance Enumerable Int where
  fromTo = enumFromTo'
  fromThenTo = enumFromThenTo'

instance Parseable Integer where
  tPatParser = pIntegral
  doEuclid = euclidOff
  getControl = fmap fromIntegral . cI_

instance Enumerable Integer where
  fromTo = enumFromTo'
  fromThenTo = enumFromThenTo'

instance Parseable Rational where
  tPatParser = pRational
  doEuclid = euclidOff
  getControl = cR_

instance Enumerable Rational where
  fromTo = enumFromTo'
  fromThenTo = enumFromThenTo'

enumFromTo' :: (Ord a, Enum a) => a -> a -> Pattern a
enumFromTo' a b | a > b = fastFromList $ reverse $ enumFromTo b a
                | otherwise = fastFromList $ enumFromTo a b

enumFromThenTo' :: (Ord a, Enum a, Num a) => a -> a -> a -> Pattern a
enumFromThenTo' a b c | a > c = fastFromList $ reverse $ enumFromThenTo c (c + (a-b)) a
                      | otherwise = fastFromList $ enumFromThenTo a b c

type ColourD = Colour Double

instance Parseable ColourD where
  tPatParser = pColour
  doEuclid = euclidOff

instance Enumerable ColourD where
  fromTo a b = fastFromList [a,b]
  fromThenTo a b c = fastFromList [a,b,c]

instance (Enumerable a, Parseable a) => IsString (Pattern a) where
  fromString = parseBP_E

lexer :: P.GenTokenParser String u Data.Functor.Identity.Identity
lexer   = P.makeTokenParser haskellDef

braces, brackets, parens, angles:: MyParser a -> MyParser a
braces  = P.braces lexer
brackets = P.brackets lexer
parens = P.parens lexer
angles = P.angles lexer

symbol :: String -> MyParser String
symbol  = P.symbol lexer

natural, integer, decimal :: MyParser Integer
natural = P.natural lexer
integer = P.integer lexer
decimal = P.integer lexer

float :: MyParser Double
float = P.float lexer

naturalOrFloat :: MyParser (Either Integer Double)
naturalOrFloat = P.naturalOrFloat lexer

data Sign      = Positive | Negative

applySign          :: Num a => Sign -> a -> a
applySign Positive =  id
applySign Negative =  negate

sign  :: MyParser Sign
sign  =  do char '-'
            return Negative
         <|> do char '+'
                return Positive
         <|> return Positive

intOrFloat :: MyParser Double
intOrFloat = do num <- naturalOrFloat
                return (case num of
                  Right x -> x
                  Left  x -> fromIntegral x
                  )

parseRhythm :: Parseable a => MyParser (TPat a) -> String -> Either ParseError (TPat a)
parseRhythm f = runParser (pSequence f' Prelude.<* eof) (0 :: Int) ""
  where f' = do f
                <|> do symbol "~" <?> "rest"
                       return TPat_Silence

pSequence :: Parseable a => MyParser (TPat a) -> GenParser Char Int (TPat a)
pSequence f = do
  spaces
  s <- many $ do
    a <- pPart f
    spaces
    do
      try $ symbol ".."
      b <- pPart f
      return $ TPat_EnumFromTo a b
      <|> pElongate a
      <|> pRepeat a
      <|> return a
    <|> do
      symbol "."
      return TPat_Foot
  pRand $ resolve_feet s
  where resolve_feet ps | length ss > 1 = TPat_Seq $ map TPat_Seq ss
                        | otherwise = TPat_Seq ps
          where ss = splitFeet ps
        splitFeet :: [TPat t] -> [[TPat t]]
        splitFeet [] = []
        splitFeet pats = foot : splitFeet pats'
          where (foot, pats') = takeFoot pats
                takeFoot [] = ([], [])
                takeFoot (TPat_Foot:pats'') = ([], pats'')
                takeFoot (pat:pats'') = first (pat:) $ takeFoot pats''

pRepeat :: TPat a -> MyParser (TPat a)
pRepeat a = do es <- many1 $ do char '!'
                                n <- (subtract 1 . read <$> many1 digit) <|> return 1
                                spaces
                                return n
               return $ TPat_Repeat (1 + sum es) a

pElongate :: TPat a -> MyParser (TPat a)
pElongate a = do rs <- many1 $ do oneOf "@_"
                                  r <- (subtract 1 <$> pRatio) <|> return 1
                                  spaces
                                  return r
                 return $ TPat_Elongate (1 + sum rs) a

pSingle :: MyParser (TPat a) -> MyParser (TPat a)
pSingle f = f >>= pRand >>= pMult

pVar :: MyParser (TPat a)
pVar = wrapPos $ do char '^'
                    name <- many (letter <|> oneOf "0123456789:.-_") <?> "string"
                    return $ TPat_Var name

pPart :: Parseable a => MyParser (TPat a) -> MyParser (TPat a)
pPart f = (pSingle f <|> pPolyIn f <|> pPolyOut f <|> pVar) >>= pE >>= pRand

newSeed :: MyParser Int
newSeed = do seed <- Text.Parsec.Prim.getState
             Text.Parsec.Prim.modifyState (+1)
             return seed

pPolyIn :: Parseable a => MyParser (TPat a) -> MyParser (TPat a)
pPolyIn f = do x <- brackets $ do s <- pSequence f <?> "sequence"
                                  stackTail s <|> chooseTail s <|> return s
               pMult x
  where stackTail s = do symbol ","
                         ss <- pSequence f `sepBy` symbol ","
                         return $ TPat_Stack (s:ss)
        chooseTail s = do symbol "|"
                          ss <- pSequence f `sepBy` symbol "|"
                          seed <- newSeed
                          return $ TPat_CycleChoose seed (s:ss)

pPolyOut :: Parseable a => MyParser (TPat a) -> MyParser (TPat a)
pPolyOut f = do ss <- braces (pSequence f `sepBy` symbol ",")
                base <- do char '%'
                           r <- pSequence pRational <?> "rational number"
                           return $ Just r
                        <|> return Nothing
                pMult $ TPat_Polyrhythm base ss
             <|>
             do ss <- angles (pSequence f `sepBy` symbol ",")
                pMult $ TPat_Polyrhythm (Just $ TPat_Atom Nothing 1) ss

pCharNum :: MyParser Char
pCharNum = (letter <|> oneOf "0123456789") <?> "letter or number"

pString :: MyParser String
pString = do c <- pCharNum <?> "charnum"
             cs <- many (letter <|> oneOf "0123456789:.-_") <?> "string"
             return (c:cs)

wrapPos :: MyParser (TPat a) -> MyParser (TPat a)
wrapPos p = do b <- getPosition
               tpat <- p
               e <- getPosition
               let addPos (TPat_Atom _ v') =
                     TPat_Atom (Just ((sourceColumn b, sourceLine b), (sourceColumn e, sourceLine e))) v'
                   addPos x = x -- shouldn't happen..
               return $ addPos tpat

pVocable :: MyParser (TPat String)
pVocable = wrapPos $ TPat_Atom Nothing <$> pString

pChar :: MyParser (TPat Char)
pChar = wrapPos $ TPat_Atom Nothing <$> pCharNum

pDouble :: MyParser (TPat Double)
pDouble = wrapPos $ do s <- sign
                       f <- choice [intOrFloat, pRatioChar, parseNote] <?> "float"
                       let v = applySign s f
                       do TPat_Stack . map (TPat_Atom Nothing . (+ v)) <$> parseChord
                         <|> return (TPat_Atom Nothing v)
                      <|>
                         do TPat_Stack . map (TPat_Atom Nothing) <$> parseChord

pNote :: MyParser (TPat Note)
pNote = wrapPos $ fmap (fmap Note) $ do s <- sign
                                        f <- choice [intOrFloat, parseNote] <?> "float"
                                        let v = applySign s f
                                        do TPat_Stack . map (TPat_Atom Nothing . (+ v)) <$> parseChord
                                           <|> return (TPat_Atom Nothing v)
                                     <|> do TPat_Stack . map (TPat_Atom Nothing) <$> parseChord
                                     <|> do TPat_Atom Nothing <$> pRatioChar

pBool :: MyParser (TPat Bool)
pBool = wrapPos $ do oneOf "t1"
                     return $ TPat_Atom Nothing True
                  <|>
                  do oneOf "f0"
                     return $ TPat_Atom Nothing False

parseIntNote  :: Integral i => MyParser i
parseIntNote = do s <- sign
                  i <- choice [integer, parseNote]
                  return $ applySign s $ fromIntegral i

parseInt :: MyParser Int
parseInt = do s <- sign
              i <- integer
              return $ applySign s $ fromIntegral i

pIntegral :: Integral a => MyParser (TPat a)
pIntegral = wrapPos $ do i <- parseIntNote
                         do TPat_Stack . map (TPat_Atom Nothing . (+i)) <$> parseChord
                           <|> return (TPat_Atom Nothing i)
                      <|>
                         do TPat_Stack . map (TPat_Atom Nothing) <$> parseChord

parseChord :: (Enum a, Num a) => MyParser [a]
parseChord = do char '\''
                name <- many1 $ letter <|> digit
                let chord = fromMaybe [0] $ lookup name chordTable
                do char '\''
                   notFollowedBy space <?> "chord range or 'i' or 'o'"
                   let n = length chord
                   i <- option n (fromIntegral <$> integer)
                   j <- length <$> many (char 'i')
                   o <- length <$> many (char 'o')
                   let chord' = take i $ drop j $ concatMap (\x -> map (+ x) chord) [0,12..]
                   -- open voiced chords
                   let chordo' = if o > 0 then
                                     [ (chord' !! 0 - 12), (chord' !! 2 - 12), (chord' !! 1) ] ++ reverse (take (length chord' - 3) (reverse chord'))
                                 else chord'
                   return chordo'
                  <|> return chord

parseNote :: Num a => MyParser a
parseNote = do n <- notenum
               modifiers <- many noteModifier
               octave <- option 5 natural
               let n' = foldr (+) n modifiers
               return $ fromIntegral $ n' + ((octave-5)*12)
  where
        notenum :: MyParser Integer
        notenum = choice [char 'c' >> return 0,
                          char 'd' >> return 2,
                          char 'e' >> return 4,
                          char 'f' >> return 5,
                          char 'g' >> return 7,
                          char 'a' >> return 9,
                          char 'b' >> return 11
                         ]
        noteModifier :: MyParser Integer
        noteModifier = choice [char 's' >> return 1,
                               char 'f' >> return (-1),
                               char 'n' >> return 0
                              ]

fromNote :: Num a => Pattern String -> Pattern a
fromNote pat = fromRight 0 . runParser parseNote 0 "" <$> pat

pColour :: MyParser (TPat ColourD)
pColour = wrapPos $ do name <- many1 letter <?> "colour name"
                       colour <- readColourName name <?> "known colour"
                       return $ TPat_Atom Nothing colour

pMult :: TPat a -> MyParser (TPat a)
pMult thing = do char '*'
                 spaces
                 r <- pRational <|> pPolyIn pRational <|> pPolyOut pRational
                 return $ TPat_Fast r thing
              <|>
              do char '/'
                 spaces
                 r <- pRational <|> pPolyIn pRational <|> pPolyOut pRational
                 return $ TPat_Slow r thing
              <|>
              return thing

pRand :: TPat a -> MyParser (TPat a)
pRand thing = do char '?'
                 r <- float <|> return 0.5
                 spaces
                 seed <- newSeed
                 return $ TPat_DegradeBy seed r thing
              <|> return thing

pE :: TPat a -> MyParser (TPat a)
pE thing = do (n,k,s) <- parens pair
              pure $ TPat_Euclid n k s thing
            <|> return thing
   where pair :: MyParser (TPat Int, TPat Int, TPat Int)
         pair = do a <- pSequence pIntegral
                   spaces
                   symbol ","
                   spaces
                   b <- pSequence pIntegral
                   c <- do symbol ","
                           spaces
                           pSequence pIntegral
                        <|> return (TPat_Atom Nothing 0)
                   return (a, b, c)

pRatio :: MyParser Rational
pRatio = do 
  s <- sign
  r <- do n <- read <$> many1 digit
          result <- do char '%'
                       d <- decimal
                       return (n%d)
                    <|>
                    do char '.'
                       frac <- many1 digit
                       -- A hack, but not sure if doing this
                       -- numerically would be any faster..
                       return (toRational ((read $ show n ++ "." ++ frac) :: Double))
                    <|>
                    return (n%1)
          c <- pRatioChar <|> return 1
          return (result * c)
        <|> 
        pRatioChar
  return $ applySign s r

pRatioChar :: Fractional a => MyParser a
pRatioChar = do char 'w'
                return 1
             <|> do char 'h'
                    return 0.5
             <|> do char 'q'
                    return 0.25
             <|> do char 'e'
                    return 0.125
             <|> do char 's'
                    return 0.0625
             <|> do char 't'
                    return (1/3)
             <|> do char 'f'
                    return 0.2
             <|> do char 'x'
                    return (1/6)

pRational :: MyParser (TPat Rational)
pRational = wrapPos $ TPat_Atom Nothing <$> pRatio

{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Sound.Tidal.Parse where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language ( haskellDef )
import Data.Ratio
import Data.Colour
import Data.Colour.Names
import Data.Colour.SRGB
import GHC.Exts( IsString(..) )
import Data.Monoid
import Control.Exception as E
import Control.Applicative ((<$>), (<*>), pure)
import Data.Maybe
import Data.List

import Sound.Tidal.Pattern
import Sound.Tidal.Time (Arc, Time)

-- | AST representation of patterns
data TPat a
   = TPat_Atom a
   | TPat_Density Time (TPat a)
      -- We keep this distinct from 'density' because of divide-by-zero:
   | TPat_Slow Time (TPat a)
   | TPat_Zoom Arc (TPat a)
   | TPat_DegradeBy Double (TPat a)
   | TPat_Silence
   | TPat_Cat [TPat a]
   | TPat_Overlay (TPat a) (TPat a)
   | TPat_ShiftL Time (TPat a)
   -- | TPat_E Int Int (TPat a)
   | TPat_pE (TPat Int) (TPat Int) (TPat Integer) (TPat a)
 deriving (Show)

instance Monoid (TPat a) where
   mempty = TPat_Silence
   mappend = TPat_Overlay

toPat :: TPat a -> Pattern a
toPat = \case
   TPat_Atom x -> atom x
   TPat_Density t x -> density t $ toPat x
   TPat_Slow t x -> slow t $ toPat x
   TPat_Zoom arc x -> zoom arc $ toPat x
   TPat_DegradeBy amt x -> degradeBy amt $ toPat x
   TPat_Silence -> silence
   TPat_Cat xs -> cat $ map toPat xs
   TPat_Overlay x0 x1 -> overlay (toPat x0) (toPat x1)
   TPat_ShiftL t x -> t <~ toPat x
   TPat_pE n k s thing ->
      unwrap $ eoff <$> toPat n <*> toPat k <*> toPat s <*> pure (toPat thing)

parsePat :: Parseable a => String -> Pattern a
parsePat = toPat . p

class Parseable a where
  p :: String -> TPat a

instance Parseable Double where
  p = parseRhythm pDouble

instance Parseable String where
  p = parseRhythm pVocable

instance Parseable Bool where
  p = parseRhythm pBool

instance Parseable Int where
  p = parseRhythm pIntegral

instance Parseable Integer where
  p s = parseRhythm pIntegral s

instance Parseable Rational where
  p = parseRhythm pRational

type ColourD = Colour Double

instance Parseable ColourD where
  p = parseRhythm pColour

instance (Parseable a) => IsString (Pattern a) where
  fromString = toPat . p

--instance (Parseable a, Pattern p) => IsString (p a) where
--  fromString = p :: String -> p a

lexer   = P.makeTokenParser haskellDef
braces  = P.braces lexer
brackets = P.brackets lexer
parens = P.parens lexer
angles = P.angles lexer
symbol  = P.symbol lexer
natural = P.natural lexer
integer = P.integer lexer
float = P.float lexer
naturalOrFloat = P.naturalOrFloat lexer

data Sign      = Positive | Negative

applySign          :: Num a => Sign -> a -> a
applySign Positive =  id
applySign Negative =  negate

sign  :: Parser Sign
sign  =  do char '-'
            return Negative
         <|> do char '+'
                return Positive
         <|> return Positive

intOrFloat :: Parser (Either Integer Double)
intOrFloat =  do s   <- sign
                 num <- naturalOrFloat
                 return (case num of
                            Right x -> Right (applySign s x)
                            Left  x -> Left  (applySign s x)
                        )

r :: Parseable a => String -> Pattern a -> IO (Pattern a)
r s orig = do E.handle 
                (\err -> do putStrLn (show (err :: E.SomeException))
                            return orig 
                )
                (return $ toPat $ p s)

parseRhythm :: Parser (TPat a) -> String -> TPat a
parseRhythm f input = either (const TPat_Silence) id $ parse (pSequence f') "" input
  where f' = f
             <|> do symbol "~" <?> "rest"
                    return TPat_Silence

pSequenceN :: Parser (TPat a) -> GenParser Char () (Int, TPat a)
pSequenceN f = do spaces
                  d <- pDensity
                  ps <- many $ pPart f
                  return (length ps, TPat_Density d $ TPat_Cat $ concat ps)
                 
pSequence :: Parser (TPat a) -> GenParser Char () (TPat a)
pSequence f = do (_, p) <- pSequenceN f
                 return p

pSingle :: Parser (TPat a) -> Parser (TPat a)
pSingle f = f >>= pRand >>= pMult

pPart :: Parser (TPat a) -> Parser [TPat a]
pPart f = do -- part <- parens (pSequence f) <|> pSingle f <|> pPolyIn f <|> pPolyOut f
             part <- pSingle f <|> pPolyIn f <|> pPolyOut f
             part <- pE part
             part <- pRand part
             spaces
             parts <- pStretch part
                      <|> pReplicate part
             spaces
             return $ parts

pPolyIn :: Parser (TPat a) -> Parser (TPat a)
pPolyIn f = do ps <- brackets (pSequence f `sepBy` symbol ",")
               spaces
               pMult $ mconcat ps

pPolyOut :: Parser (TPat a) -> Parser (TPat a)
pPolyOut f = do ps <- braces (pSequenceN f `sepBy` symbol ",")
                spaces
                base <- do char '%'
                           spaces
                           i <- integer <?> "integer"
                           return $ Just (fromIntegral i)
                        <|> return Nothing
                pMult $ mconcat $ scale base ps
  where scale _ [] = []
        scale base (ps@((n,_):_)) = map (\(n',p) -> TPat_Density (fromIntegral (fromMaybe n base)/ fromIntegral n') p) ps

pString :: Parser (String)
pString = many1 (letter <|> oneOf "0123456789:.-_") <?> "string"

pVocable :: Parser (TPat String)
pVocable = do v <- pString
              return $ TPat_Atom v

pDouble :: Parser (TPat Double)
pDouble = do nf <- intOrFloat <?> "float"
             let f = either fromIntegral id nf
             return $ TPat_Atom f

pBool :: Parser (TPat Bool)
pBool = do oneOf "t1"
           return $ TPat_Atom True
        <|>
        do oneOf "f0"
           return $ TPat_Atom False

parseIntNote :: Integral i => Parser i
parseIntNote = do s <- sign
                  i <- choice [integer, parseNote]
                  return $ applySign s $ fromIntegral i

parseInt :: Parser Int
parseInt = do s <- sign
              i <- integer
              return $ applySign s $ fromIntegral i

pIntegral :: Integral i => Parser (TPat i)
pIntegral = TPat_Atom <$> parseIntNote

parseNote :: Integral a => Parser a
parseNote = do n <- notenum
               modifiers <- many noteModifier
               octave <- option 5 natural
               let n' = foldr (+) n modifiers
               return $ fromIntegral $ n' + ((octave-5)*12)
  where
        notenum :: Parser Integer
        notenum = choice [char 'c' >> return 0,
                          char 'd' >> return 2,
                          char 'e' >> return 4,
                          char 'f' >> return 5,
                          char 'g' >> return 7,
                          char 'a' >> return 9,
                          char 'b' >> return 11
                         ]
        noteModifier :: Parser Integer
        noteModifier = choice [char 's' >> return 1,
                               char 'f' >> return (-1),
                               char 'n' >> return 0
                              ]

fromNote :: Integral c => Pattern String -> Pattern c
fromNote p = (\s -> either (const 0) id $ parse parseNote "" s) <$> p

pColour :: Parser (TPat ColourD)
pColour = do name <- many1 letter <?> "colour name"
             colour <- readColourName name <?> "known colour"
             return $ TPat_Atom colour

pMult :: TPat a -> Parser (TPat a)
pMult thing = do char '*'
                 spaces
                 r <- pRatio
                 return $ TPat_Density r thing
              <|>
              do char '/'
                 spaces
                 r <- pRatio
                 return $ TPat_Slow r thing
              <|>
              return thing



pRand :: TPat a -> Parser (TPat a)
pRand thing = do char '?'
                 spaces
                 return $ TPat_DegradeBy 0.5 thing
              <|> return thing

pE :: TPat a -> Parser (TPat a)
pE thing = do (n,k,s) <- parens (pair)
              pure $ TPat_pE n k s thing
            <|> return thing
   where pair :: Parser (TPat Int, TPat Int, TPat Integer)
         pair = do a <- pSequence pIntegral
                   spaces
                   symbol ","
                   spaces
                   b <- pSequence pIntegral
                   c <- do symbol ","
                           spaces
                           pSequence pIntegral
                        <|> return (TPat_Atom 0)
                   return (a, b, c)

eoff :: Int -> Int -> Integer -> Pattern a -> Pattern a
eoff n k s p = ((s%(fromIntegral k)) <~) (e n k p)
   -- TPat_ShiftL (s%(fromIntegral k)) (TPat_E n k p)

pReplicate :: TPat a -> Parser [TPat a]
pReplicate thing =
  do extras <- many $ do char '!'
                         -- if a number is given (without a space)
                         -- replicate that number of times
                         n <- ((read <$> many1 digit) <|> return 2)
                         spaces
                         thing' <- pRand thing
                         -- -1 because we already have parsed the original one
                         return $ replicate (fromIntegral (n-1)) thing'
     return (thing:concat extras)


pStretch :: TPat a -> Parser [TPat a]
pStretch thing =
  do char '@'
     n <- ((read <$> many1 digit) <|> return 1)
     return $ map (\x -> TPat_Zoom (x%n,(x+1)%n) thing) [0 .. (n-1)]

pRatio :: Parser (Rational)
pRatio = do n <- natural <?> "numerator"
            d <- do oneOf "/%"
                    natural <?> "denominator"
                 <|>
                 return 1
            return $ n % d

pRational :: Parser (TPat Rational)
pRational = do r <- pRatio
               return $ TPat_Atom r

pDensity :: Parser (Rational)
pDensity = angles (pRatio <?> "ratio")
           <|>
           return (1 % 1)


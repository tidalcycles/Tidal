{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, OverlappingInstances, IncoherentInstances, FlexibleInstances #-}

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
import Control.Applicative ((<$>), (<*>))
import Data.Maybe

import Sound.Tidal.Pattern

class Parseable a where
  p :: String -> Pattern a

instance Parseable Double where
  p = parseRhythm pDouble

instance Parseable String where
  p = parseRhythm pVocable

instance Parseable Bool where
  p = parseRhythm pBool

instance Parseable Int where
  p = parseRhythm pInt

instance Parseable Rational where
  p = parseRhythm pRational

type ColourD = Colour Double

instance Parseable ColourD where
  p = parseRhythm pColour

instance (Parseable a) => IsString (Pattern a) where
  fromString = p

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
                (return $ p s)

parseRhythm :: Parser (Pattern a) -> String -> (Pattern a)
parseRhythm f input = either (const silence) id $ parse (pSequence f') "" input
  where f' = f
             <|> do symbol "~" <?> "rest"
                    return silence

pSequenceN :: Parser (Pattern a) -> GenParser Char () (Int, Pattern a)
pSequenceN f = do spaces
                  d <- pDensity
                  ps <- many $ pPart f
                  return $ (length ps, density d $ cat $ concat ps)
                 
pSequence :: Parser (Pattern a) -> GenParser Char () (Pattern a)
pSequence f = do (_, p) <- pSequenceN f
                 return p

pSingle :: Parser (Pattern a) -> Parser (Pattern a)
pSingle f = f >>= pRand >>= pMult

pPart :: Parser (Pattern a) -> Parser ([Pattern a])
pPart f = do -- part <- parens (pSequence f) <|> pSingle f <|> pPolyIn f <|> pPolyOut f
             part <- pSingle f <|> pPolyIn f <|> pPolyOut f
             part <- pE part
             part <- pRand part
             spaces
             parts <- pReplicate part
             spaces
             return $ parts

pPolyIn :: Parser (Pattern a) -> Parser (Pattern a)
pPolyIn f = do ps <- brackets (pSequence f `sepBy` symbol ",")
               spaces
               pMult $ mconcat ps

pPolyOut :: Parser (Pattern a) -> Parser (Pattern a)
pPolyOut f = do ps <- braces (pSequenceN f `sepBy` symbol ",")
                spaces
                base <- do char '%'
                           spaces
                           i <- integer <?> "integer"
                           return $ Just (fromIntegral i)
                        <|> return Nothing
                pMult $ mconcat $ scale base ps
  where scale _ [] = []
        scale base (ps@((n,_):_)) = map (\(n',p) -> density (fromIntegral (fromMaybe n base)/ fromIntegral n') p) ps

pString :: Parser (String)
pString = many1 (letter <|> oneOf "0123456789:") <?> "string"

pVocable :: Parser (Pattern String)
pVocable = do v <- pString
              return $ atom v

pDouble :: Parser (Pattern Double)
pDouble = do nf <- intOrFloat <?> "float"
             let f = either fromIntegral id nf
             return $ atom f

pBool :: Parser (Pattern Bool)
pBool = do oneOf "t1"
           return $ atom True
        <|>
        do oneOf "f0"
           return $ atom False

pInt :: Parser (Pattern Int)
pInt = do s <- sign
          i <- integer <?> "integer"
          return $ atom (applySign s $ fromIntegral i)

pColour :: Parser (Pattern ColourD)
pColour = do name <- many1 letter <?> "colour name"
             colour <- readColourName name <?> "known colour"
             return $ atom colour

pMult :: Pattern a -> Parser (Pattern a)
pMult thing = do char '*'
                 spaces
                 r <- pRatio
                 return $ density r thing
              <|>
              do char '/'
                 spaces
                 r <- pRatio
                 return $ slow r thing
              <|>
              return thing



pRand :: Pattern a -> Parser (Pattern a)
pRand thing = do char '?'
                 spaces
                 return $ degrade thing
              <|> return thing

pE :: Pattern a -> Parser (Pattern a)
pE thing = do (n,k,s) <- parens (pair)
              return $ unwrap $ eoff <$> n <*> k <*> s <*> atom thing
            <|> return thing
   where pair = do a <- pSequence pInt
                   spaces
                   symbol ","
                   spaces
                   b <- pSequence pInt
                   c <- do symbol ","
                           spaces
                           pSequence pInt
                        <|> return (atom 0)
                   return (fromIntegral <$> a, fromIntegral <$> b, fromIntegral <$> c)
         eoff n k s p = ((s%(fromIntegral k)) <~) (e n k p)
                   

pReplicate :: Pattern a -> Parser ([Pattern a])
pReplicate thing = do extras <- many $ do char '!'
                                          spaces
                                          pRand thing
                      return (thing:extras)

pRatio :: Parser (Rational)
pRatio = do n <- natural <?> "numerator"
            d <- do oneOf "/%"
                    natural <?> "denominator"
                 <|>
                 return 1
            return $ n % d

pRational :: Parser (Pattern Rational)
pRational = do r <- pRatio
               return $ atom r

pDensity :: Parser (Rational)
pDensity = angles (pRatio <?> "ratio")
           <|>
           return (1 % 1)


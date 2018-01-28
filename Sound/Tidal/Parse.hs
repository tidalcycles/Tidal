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

data TPat a = TPat_Atom a
            | TPat_Density (TPat Time) (TPat a)
            | TPat_Slow (TPat Time) (TPat a)
            | TPat_Zoom Arc (TPat a)
            | TPat_DegradeBy Double (TPat a)
            | TPat_Silence
            | TPat_Foot
            | TPat_Elongate Int
            | TPat_EnumFromTo (TPat a) (TPat a)
            | TPat_Cat [TPat a]
            | TPat_TimeCat [TPat a]
            | TPat_Overlay (TPat a) (TPat a)
            | TPat_ShiftL Time (TPat a)
              -- TPat_E Int Int (TPat a)
            | TPat_pE (TPat Int) (TPat Int) (TPat Integer) (TPat a)
            deriving (Show)

instance Parseable a => Monoid (TPat a) where
   mempty = TPat_Silence
   mappend = TPat_Overlay

toPat :: Parseable a => TPat a -> Pattern a
toPat = \case
   TPat_Atom x -> atom x
   TPat_Density t x -> density (toPat t) $ toPat x
   TPat_Slow t x -> slow (toPat t) $ toPat x
   TPat_Zoom arc x -> zoom arc $ toPat x
   TPat_DegradeBy amt x -> _degradeBy amt $ toPat x
   TPat_Silence -> silence
   TPat_Cat xs -> fastcat $ map toPat xs
   TPat_TimeCat xs -> timeCat $ map (\(n, p) -> (toRational n, toPat p)) $ durations xs
   TPat_Overlay x0 x1 -> overlay (toPat x0) (toPat x1)
   TPat_ShiftL t x -> t `rotL` toPat x
   TPat_pE n k s thing ->
      unwrap $ eoff <$> toPat n <*> toPat k <*> toPat s <*> pure (toPat thing)
   TPat_Foot -> error "Can't happen, feet (.'s) only used internally.."
   TPat_EnumFromTo a b -> unwrap $ fromTo <$> (toPat a) <*> (toPat b)
   -- TPat_EnumFromThenTo a b c -> unwrap $ fromThenTo <$> (toPat a) <*> (toPat b) <*> (toPat c)

durations :: [TPat a] -> [(Int, TPat a)]
durations [] = []
durations ((TPat_Elongate n):xs) = (n, TPat_Silence):(durations xs)
durations (a:(TPat_Elongate n):xs) = (n+1,a):(durations xs)
durations (a:xs) = (1,a):(durations xs)

p :: Parseable a => String -> Pattern a
p = toPat . parseTPat

class Parseable a where
  parseTPat :: String -> TPat a
  fromTo :: a -> a -> Pattern a
  fromThenTo :: a -> a -> a -> Pattern a

instance Parseable Double where
  parseTPat = parseRhythm pDouble
  fromTo a b = enumFromTo' a b
  fromThenTo a b c = enumFromThenTo' a b c

instance Parseable String where
  parseTPat = parseRhythm pVocable
  fromTo a b = listToPat [a,b]
  fromThenTo a b c = listToPat [a,b,c]

instance Parseable Bool where
  parseTPat = parseRhythm pBool
  fromTo a b = listToPat [a,b]
  fromThenTo a b c = listToPat [a,b,c]

instance Parseable Int where
  parseTPat = parseRhythm pIntegral
  fromTo a b = enumFromTo' a b
  fromThenTo a b c = enumFromThenTo' a b c

instance Parseable Integer where
  parseTPat s = parseRhythm pIntegral s
  fromTo a b = enumFromTo' a b
  fromThenTo a b c = enumFromThenTo' a b c

instance Parseable Rational where
  parseTPat = parseRhythm pRational
  fromTo a b = enumFromTo' a b
  fromThenTo a b c = enumFromThenTo' a b c

enumFromTo' a b | a > b = listToPat $ reverse $ enumFromTo b a
                | otherwise = listToPat $ enumFromTo a b

enumFromThenTo' a b c | a > c = listToPat $ reverse $ enumFromThenTo c (c + (a-b)) a
                      | otherwise = listToPat $ enumFromThenTo a b c

type ColourD = Colour Double 

instance Parseable ColourD where
  parseTPat = parseRhythm pColour
  fromTo a b = listToPat [a,b]
  fromThenTo a b c = listToPat [a,b,c]

instance (Parseable a) => IsString (Pattern a) where
  fromString = toPat . parseTPat

--instance (Parseable a, Pattern p) => IsString (p a) where
--  fromString = p :: String -> p a

lexer   = P.makeTokenParser haskellDef

braces, brackets, parens, angles:: Parser a -> Parser a
braces  = P.braces lexer
brackets = P.brackets lexer
parens = P.parens lexer
angles = P.angles lexer

symbol :: String -> Parser String
symbol  = P.symbol lexer

natural, integer :: Parser Integer
natural = P.natural lexer
integer = P.integer lexer

float :: Parser Double
float = P.float lexer

naturalOrFloat :: Parser (Either Integer Double)
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

parseRhythm :: Parseable a => Parser (TPat a) -> String -> TPat a
parseRhythm f input = either (const TPat_Silence) id $ parse (pSequence f') "" input
  where f' = f
             <|> do symbol "~" <?> "rest"
                    return TPat_Silence

pSequenceN :: Parseable a => Parser (TPat a) -> GenParser Char () (Int, TPat a)
pSequenceN f = do spaces
                  -- d <- pDensity
                  ps <- many $ do a <- pPart f
                                  do Text.ParserCombinators.Parsec.try $ symbol ".."
                                     b <- pPart f
                                     return [TPat_EnumFromTo (TPat_Cat a) (TPat_Cat b)]
                                    <|> return a
                               <|> do symbol "."
                                      return [TPat_Foot]
                               <|> do es <- many1 (symbol "_")
                                      return [TPat_Elongate (length es)]
                  let ps' = TPat_Cat $ map elongate $ splitFeet $ concat ps
                  return (length ps, ps')

elongate xs | any (isElongate) xs = TPat_TimeCat xs
            | otherwise = TPat_Cat xs
  where isElongate (TPat_Elongate _) = True
        isElongate _ = False
{-
expandEnum :: Parseable t => Maybe (TPat t) -> [TPat t] -> [TPat t]
expandEnum a [] = [a]
expandEnum (Just a) (TPat_Enum:b:ps) = (TPat_EnumFromTo a b) : (expandEnum Nothing ps)
-- ignore ..s in other places
expandEnum a (TPat_Enum:ps) = expandEnum a ps
expandEnum (Just a) (b:ps) = a:(expandEnum b (Just c) ps)
expandEnum Nothing (c:ps) = expandEnum (Just c) ps
-}

-- could use splitOn here but `TPat a` isn't a member of `EQ`..
splitFeet :: [TPat t] -> [[TPat t]]
splitFeet [] = []
splitFeet ps = foot:(splitFeet ps')
  where (foot, ps') = takeFoot ps
        takeFoot [] = ([], [])
        takeFoot (TPat_Foot:ps) = ([], ps)
        takeFoot (p:ps) = (\(a,b) -> (p:a,b)) $ takeFoot ps

pSequence :: Parseable a => Parser (TPat a) -> GenParser Char () (TPat a)
pSequence f = do (_, p) <- pSequenceN f
                 return p

pSingle :: Parseable a => Parser (TPat a) -> Parser (TPat a)
pSingle f = f >>= pRand >>= pMult

pPart :: Parseable a => Parser (TPat a) -> Parser [TPat a]
pPart f = do part <- pSingle f <|> pPolyIn f <|> pPolyOut f
             part <- pE part
             part <- pRand part
             spaces
             parts <- pStretch part
                      <|> pReplicate part
             spaces
             return $ parts

pPolyIn :: Parseable a => Parser (TPat a) -> Parser (TPat a)
pPolyIn f = do ps <- brackets (pSequence f `sepBy` symbol ",")
               spaces
               pMult $ mconcat ps

pPolyOut :: Parseable a => Parser (TPat a) -> Parser (TPat a)
pPolyOut f = do ps <- braces (pSequenceN f `sepBy` symbol ",")
                spaces
                base <- do char '%'
                           spaces
                           i <- integer <?> "integer"
                           return $ Just (fromIntegral i)
                        <|> return Nothing
                pMult $ mconcat $ scale base ps
             <|>
             do ps <- angles (pSequenceN f `sepBy` symbol ",")
                spaces
                pMult $ mconcat $ scale (Just 1) ps
  where scale _ [] = []
        scale base (ps@((n,_):_)) = map (\(n',p) -> TPat_Density (TPat_Atom $ fromIntegral (fromMaybe n base)/ fromIntegral n') p) ps

pString :: Parser (String)
pString = do c <- (letter <|> oneOf "0123456789") <?> "charnum"
             cs <- many (letter <|> oneOf "0123456789:.-_") <?> "string"
             return (c:cs)

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

pIntegral :: Parseable a => Integral a => Parser (TPat a)
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

pMult :: Parseable a => TPat a -> Parser (TPat a)
pMult thing = do char '*'
                 spaces
                 r <- (pRational <|> pPolyIn pRational  <|> pPolyOut pRational)
                 return $ TPat_Density r thing
              <|>
              do char '/'
                 spaces
                 r <- (pRational <|> pPolyIn pRational  <|> pPolyOut pRational)
                 return $ TPat_Slow r thing
              <|>
              return thing



pRand :: Parseable a => TPat a -> Parser (TPat a)
pRand thing = do char '?'
                 spaces
                 return $ TPat_DegradeBy 0.5 thing
              <|> return thing

pE :: Parseable a => TPat a -> Parser (TPat a)
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
eoff n k s p = ((s%(fromIntegral k)) `rotL`) (e n k p)
   -- TPat_ShiftL (s%(fromIntegral k)) (TPat_E n k p)

pReplicate :: Parseable a => TPat a -> Parser [TPat a]
pReplicate thing =
  do extras <- many $ do char '!'
                         -- if a number is given (without a space)slow 2 $ fast
                         -- replicate that number of times
                         n <- ((read <$> many1 digit) <|> return 2)
                         spaces
                         thing' <- pRand thing
                         -- -1 because we already have parsed the original one
                         return $ replicate (fromIntegral (n-1)) thing'
     return (thing:concat extras)


pStretch :: Parseable a => TPat a -> Parser [TPat a]
pStretch thing =
  do char '@'
     n <- ((read <$> many1 digit) <|> return 1)
     return $ map (\x -> TPat_Zoom (x%n,(x+1)%n) thing) [0 .. (n-1)]

pRatio :: Parser (Rational)
pRatio = do s <- sign
            n <- natural
            result <- do char '%'
                         d <- natural
                         return (n%d)
                      <|>
                      do char '.'
                         s <- many1 digit
                         -- A hack, but not sure if doing this
                         -- numerically would be any faster..
                         return (toRational $ ((read $ show n ++ "." ++ s)  :: Double))
                      <|>
                      return (n%1)
            return $ applySign s result

pRational :: Parser (TPat Rational)
pRational = do r <- pRatio
               return $ TPat_Atom r

{-
pDensity :: Parser (Rational)
pDensity = angles (pRatio <?> "ratio")
           <|>
           return (1 % 1)
-}

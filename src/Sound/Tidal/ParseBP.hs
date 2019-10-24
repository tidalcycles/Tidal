{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans -fno-warn-unused-do-bind #-}

module Sound.Tidal.ParseBP where

import           Control.Applicative ((<$>), (<*>), pure)
import qualified Control.Exception as E
import           Data.Colour
import           Data.Colour.Names
import           Data.Functor.Identity (Identity)
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

data TPat a = TPat_Atom a
            | TPat_Density (TPat Time) (TPat a)
            | TPat_Slow (TPat Time) (TPat a)
            | TPat_Zoom Arc (TPat a)
            | TPat_DegradeBy Int Double (TPat a)
            | TPat_Silence
            | TPat_Foot
            | TPat_Elongate Int
            | TPat_EnumFromTo (TPat a) (TPat a)
            | TPat_Cat [TPat a]
            | TPat_TimeCat [TPat a]
            | TPat_Overlay (TPat a) (TPat a)
            | TPat_Stack [TPat a]
            | TPat_CycleChoose Int [TPat a]
            | TPat_ShiftL Time (TPat a)
              -- TPat_E Int Int (TPat a)
            | TPat_pE (TPat Int) (TPat Int) (TPat Int) (TPat a)
            deriving (Show)

toPat :: (Enumerable a, Parseable a) => TPat a -> Pattern a
toPat = \case
   TPat_Atom x -> pure x
   TPat_Density t x -> fast (toPat t) $ toPat x
   TPat_Slow t x -> slow (toPat t) $ toPat x
   TPat_Zoom a x -> zoomArc a $ toPat x
   TPat_DegradeBy seed amt x -> _degradeByUsing (rotL (0.0001 * (fromIntegral seed)) rand) amt $ toPat x
   TPat_Silence -> silence
   TPat_Cat xs -> fastcat $ map toPat xs
   TPat_TimeCat xs -> timeCat $ map (\(n, pat) -> (toRational n, toPat pat)) $ durations xs
   TPat_Overlay x0 x1 -> overlay (toPat x0) (toPat x1)
   TPat_Stack xs -> stack $ map toPat xs
   TPat_CycleChoose seed xs -> unwrap $ segment 1 $ chooseBy (rotL (0.0001 * (fromIntegral seed)) rand) $ map toPat xs
   TPat_ShiftL t x -> t `rotL` toPat x
   TPat_pE n k s thing ->
      doEuclid (toPat n) (toPat k) (toPat s) (toPat thing)
   TPat_Foot -> error "Can't happen, feet (.'s) only used internally.."
   TPat_EnumFromTo a b -> unwrap $ fromTo <$> toPat a <*> toPat b
   -- TPat_EnumFromThenTo a b c -> unwrap $ fromThenTo <$> (toPat a) <*> (toPat b) <*> (toPat c)
   _ -> silence

durations :: [TPat a] -> [(Int, TPat a)]
durations [] = []
durations (TPat_Elongate n : xs) = (n, TPat_Silence) : durations xs
durations (a : TPat_Elongate n : xs) = (n+1,a) : durations xs
durations (a:xs) = (1,a) : durations xs

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

class Parseable a where
  tPatParser :: MyParser (TPat a)
  doEuclid :: Pattern Int -> Pattern Int -> Pattern Int -> Pattern a -> Pattern a
  -- toEuclid :: a -> 

class Enumerable a where
  fromTo :: a -> a -> Pattern a
  fromThenTo :: a -> a -> a -> Pattern a

instance Parseable Double where
  tPatParser = pDouble
  doEuclid = euclidOff
  
instance Enumerable Double where
  fromTo = enumFromTo'
  fromThenTo = enumFromThenTo'

instance Parseable String where
  tPatParser = pVocable
  doEuclid = euclidOff

instance Enumerable String where
  fromTo a b = fastFromList [a,b]
  fromThenTo a b c = fastFromList [a,b,c]

instance Parseable Bool where
  tPatParser = pBool
  doEuclid = euclidOffBool

instance Enumerable Bool where
  fromTo a b = fastFromList [a,b]
  fromThenTo a b c = fastFromList [a,b,c]

instance Parseable Int where
  tPatParser = pIntegral
  doEuclid = euclidOff

instance Enumerable Int where
  fromTo = enumFromTo'
  fromThenTo = enumFromThenTo'

instance Parseable Integer where
  tPatParser = pIntegral
  doEuclid = euclidOff

instance Enumerable Integer where
  fromTo = enumFromTo'
  fromThenTo = enumFromThenTo'

instance Parseable Rational where
  tPatParser = pRational
  doEuclid = euclidOff

instance Enumerable Rational where
  fromTo = enumFromTo'
  fromThenTo = enumFromThenTo'

enumFromTo' :: (Ord a, Enum a) => a -> a -> Pattern a
enumFromTo' a b | a > b = fastFromList $ reverse $ enumFromTo b a
                | otherwise = fastFromList $ enumFromTo a b

enumFromThenTo'
  :: (Ord a, Enum a, Num a) => a -> a -> a -> Pattern a
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

--instance (Parseable a, Pattern p) => IsString (p a) where
--  fromString = p :: String -> p a

lexer :: P.GenTokenParser String u Data.Functor.Identity.Identity
lexer   = P.makeTokenParser haskellDef

braces, brackets, parens, angles:: MyParser a -> MyParser a
braces  = P.braces lexer
brackets = P.brackets lexer
parens = P.parens lexer
angles = P.angles lexer

symbol :: String -> MyParser String
symbol  = P.symbol lexer

natural, integer :: MyParser Integer
natural = P.natural lexer
integer = P.integer lexer

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
intOrFloat =  do s   <- sign
                 num <- naturalOrFloat
                 return (case num of
                            Right x -> applySign s x
                            Left  x -> fromIntegral $ applySign s x
                        )

{-
r :: (Enumerable a, Parseable a) => String -> Pattern a -> IO (Pattern a)
r s orig = do E.handle
                (\err -> do putStrLn (show (err :: E.SomeException))
                            return orig
                )
                (return $ p s)
-}

parseRhythm :: Parseable a => MyParser (TPat a) -> String -> Either ParseError (TPat a)
parseRhythm f = runParser (pSequence f') (0 :: Int) ""
  where f' = do f
                <|> do symbol "~" <?> "rest"
                       return TPat_Silence

pSequenceN :: Parseable a => MyParser (TPat a) -> GenParser Char Int (Int, TPat a)
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
                      extraElongate (TPat_Elongate n) = n-1
                      extraElongate _ = 0
                      sumElongates x = sum (map extraElongate x)
                  return (length ps + sumElongates (concat ps), ps')

elongate :: [TPat a] -> TPat a
elongate xs | any isElongate xs = TPat_TimeCat xs
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
splitFeet pats = foot : splitFeet pats'
  where (foot, pats') = takeFoot pats
        takeFoot [] = ([], [])
        takeFoot (TPat_Foot:pats'') = ([], pats'')
        takeFoot (pat:pats'') = (\(a,b) -> (pat:a,b)) $ takeFoot pats''

pSequence :: Parseable a => MyParser (TPat a) -> GenParser Char Int (TPat a)
pSequence f = do (_, pat) <- pSequenceN f
                 return pat

pSingle :: MyParser (TPat a) -> MyParser (TPat a)
pSingle f = f >>= pRand >>= pMult

pPart :: Parseable a => MyParser (TPat a) -> MyParser [TPat a]
pPart f = do pt <- pSingle f <|> pPolyIn f <|> pPolyOut f
             pt' <- pE pt
             pt'' <- pRand pt'
             spaces
             pts <- pStretch pt
                    <|> pReplicate pt''
             spaces
             return pts

newSeed :: MyParser Int
newSeed = do seed <- Text.Parsec.Prim.getState
             Text.Parsec.Prim.modifyState (+1)
             return seed


pPolyIn :: Parseable a => MyParser (TPat a) -> MyParser (TPat a)
pPolyIn f = do x <- brackets $ do p <- pSequence f <?> "sequence"
                                  stackTail p <|> chooseTail p <|> return p
               pMult x
  where stackTail p = do symbol ","
                         ps <- pSequence f `sepBy` symbol ","
                         spaces
                         return $ TPat_Stack (p:ps)
        chooseTail p = do symbol "|"
                          ps <- pSequence f `sepBy` symbol "|"
                          spaces
                          seed <- newSeed
                          return $ TPat_CycleChoose seed (p:ps)

pPolyOut :: Parseable a => MyParser (TPat a) -> MyParser (TPat a)
pPolyOut f = do ps <- braces (pSequenceN f `sepBy` symbol ",")
                spaces
                base <- do char '%'
                           spaces
                           i <- integer <?> "integer"
                           return $ Just (fromIntegral i)
                        <|> return Nothing
                pMult $ TPat_Stack $ scale' base ps
             <|>
             do ps <- angles (pSequenceN f `sepBy` symbol ",")
                spaces
                pMult $ TPat_Stack $ scale' (Just 1) ps
  where scale' _ [] = []
        scale' base pats@((n,_):_) = map (\(n',pat) -> TPat_Density (TPat_Atom $ fromIntegral (fromMaybe n base)/ fromIntegral n') pat) pats

pString :: MyParser String
pString = do c <- (letter <|> oneOf "0123456789") <?> "charnum"
             cs <- many (letter <|> oneOf "0123456789:.-_") <?> "string"
             return (c:cs)

pVocable :: MyParser (TPat String)
pVocable = TPat_Atom <$> pString

pDouble :: MyParser (TPat Double)
pDouble = do f <- choice [intOrFloat, parseNote] <?> "float"
             do c <- parseChord
                return $ TPat_Stack $ map (TPat_Atom . (+f)) c
               <|> return (TPat_Atom f)
            <|>
               do c <- parseChord
                  return $ TPat_Stack $ map TPat_Atom c


pBool :: MyParser (TPat Bool)
pBool = do oneOf "t1"
           return $ TPat_Atom True
        <|>
        do oneOf "f0"
           return $ TPat_Atom False

parseIntNote  :: Integral i => MyParser i
parseIntNote = do s <- sign
                  i <- choice [integer, parseNote]
                  return $ applySign s $ fromIntegral i

parseInt :: MyParser Int
parseInt = do s <- sign
              i <- integer
              return $ applySign s $ fromIntegral i

pIntegral :: Integral a => MyParser (TPat a)
pIntegral = do i <- parseIntNote
               do c <- parseChord
                  return $ TPat_Stack $ map (TPat_Atom . (+i)) c
                 <|> return (TPat_Atom i)
            <|>
               do c <- parseChord
                  return $ TPat_Stack $ map TPat_Atom c

parseChord :: (Enum a, Num a) => MyParser [a]
parseChord = do char '\''
                name <- many1 $ letter <|> digit
                let chord = fromMaybe [0] $ lookup name chordTable
                do char '\''
                   notFollowedBy space <?> "chord range or 'i'"
                   let n = length chord
                   i <- option n (fromIntegral <$> integer)
                   j <- length <$> many (char 'i')
                   let chord' = take i $ drop j $ concatMap (\x -> map (+ x) chord) [0,12..]
                   return chord'
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
fromNote pat = either (const 0) id . runParser parseNote 0 "" <$> pat

pColour :: MyParser (TPat ColourD)
pColour = do name <- many1 letter <?> "colour name"
             colour <- readColourName name <?> "known colour"
             return $ TPat_Atom colour

pMult :: TPat a -> MyParser (TPat a)
pMult thing = do char '*'
                 spaces
                 r <- pRational <|> pPolyIn pRational <|> pPolyOut pRational
                 return $ TPat_Density r thing
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
              pure $ TPat_pE n k s thing
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
                        <|> return (TPat_Atom 0)
                   return (a, b, c)

pReplicate :: TPat a -> MyParser [TPat a]
pReplicate thing =
  do extras <- many $ do char '!'
                         -- if a number is given (without a space)slow 2 $ fast
                         -- replicate that number of times
                         n <- (read <$> many1 digit) <|> return (2 :: Int)
                         spaces
                         thing' <- pRand thing
                         -- -1 because we already have parsed the original one
                         return $ replicate (fromIntegral (n-1)) thing'
     return (thing:concat extras)

pStretch :: TPat a -> MyParser [TPat a]
pStretch thing =
  do char '@'
     n <- (read <$> many1 digit) <|> return 1
     return $ map (\x -> TPat_Zoom (Arc (x%n) ((x+1)%n)) thing) [0 .. (n-1)]

pRatio :: MyParser Rational
pRatio = do s <- sign
            n <- natural
            result <- do char '%'
                         d <- natural
                         return (n%d)
                      <|>
                      do char '.'
                         frac <- many1 digit
                         -- A hack, but not sure if doing this
                         -- numerically would be any faster..
                         return (toRational ((read $ show n ++ "." ++ frac)  :: Double))
                      <|>
                      return (n%1)
            return $ applySign s result

pRational :: MyParser (TPat Rational)
pRational = TPat_Atom <$> pRatio

{-
pDensity :: MyParser (Rational)
pDensity = angles (pRatio <?> "ratio")
           <|>
           return (1 % 1)
-}


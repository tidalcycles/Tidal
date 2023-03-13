{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, UndecidableInstances #-}

module Sound.Tidal2.Parse where

import Control.Monad (void)
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Debug
import Control.Monad.State (evalState, State, get, put)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Ratio
import Control.Monad (unless)

import Sound.Tidal2.Pattern hiding ((*>),(<*))
import Sound.Tidal2.Types

-- ************************************************************ --
-- Parser
--

-- type Parser = ParsecT Void String (State Type)

type Parser = Parsec Void String

data RhythmT a = Sequence

sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment "//"
        blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

integer :: Parser Int
integer = lexeme L.decimal

signedInteger :: Parser Int
signedInteger = L.signed (return ()) $ lexeme L.decimal

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

symbol :: String -> Parser String
symbol = L.symbol sc

parens,braces,angles,brackets :: Parser a -> Parser a
parens    = between (symbol "(") (symbol ")")
braces    = between (symbol "{") (symbol "}")
angles    = between (symbol "<") (symbol ">")
brackets  = between (symbol "[") (symbol "]")

comma :: Parser String
comma = symbol ","

identifier :: Parser String
identifier = lexeme $ do x <- lowerChar
                         xs <- many $ choice [alphaNumChar,
                                              single '_',
                                              single '\''
                                             ]
                         return $ x:xs

-- ************************************************************ --

pSequence :: Parser R
pSequence = R_Subsequence <$> many pStep

pStep :: Parser R
pStep = do r <- pRhythm
           do void $ symbol "@"
              ratio <- pNumber
              return $ R_Duration ratio r
            <|> return r

pBracket :: Parser R
pBracket = R_Silence <$ symbol "~"
          <|> brackets (R_StackCycles <$> pSequence `sepBy` comma)
          <|> braces   (R_StackSteps  <$> pSequence `sepBy` comma)
          <|> angles   (R_StackStep   <$> pSequence `sepBy` comma)
          -- <|> parens

pRhythm :: Parser R
pRhythm = pBracket <|> R_Atom <$> identifier

pRatio :: Parser Rational
pRatio = do try $ (toRational <$> L.float)
            <|> do num <- L.decimal
                   denom <- do single '%'
                               L.decimal
                            <|> return 1
                   return $ num % denom

-- ************************************************************ --

pNumber :: Parser Code
pNumber = lexeme $ do i <- L.signed (return ()) L.decimal
                      do char '.'
                         d <- (read . ('0':) . ('.':)) <$> some digit
                         return $ Cd_Float $ if i >=0 then (fromIntegral i) + d else (fromIntegral i) - d
                       <|> do char '%'
                              d <- L.decimal
                              return $ Cd_Rational $ (fromIntegral i) % (fromIntegral d)
                       <|> (return $ Cd_Int i)
     where digit = oneOf ['0'..'9']

pInfix :: Parser Code
pInfix = do l <- Just <$> pClosed
                 <|> return Nothing
            o <- pOp
            r <- Just <$> pOpen
                 <|> return Nothing
            return $ Cd_Op l o r

pClosed :: Parser Code
pClosed = try pNumber -- use try to avoid consuming + / -
          <|> pName
          <|> parens pOpen
          <|> Cd_R <$> pBracket

pOpen :: Parser Code
pOpen = try pInfix <|> pFunc <|> pClosed 

pOp :: Parser Code
pOp =
  Cd_dollar <$ symbol "$"
  <|> Cd_multiply <$ symbol "*"
  <|> Cd_divide <$ symbol "/"
  <|> Cd_plus <$ symbol "+"
  <|> Cd_subtract <$ symbol "-"

pName :: Parser Code
pName = Cd_name <$> identifier

pFunc :: Parser Code
pFunc = do name <- pName
           args <- many pClosed
           return $ foldr Cd_App name args


class Parseable a where
  to :: Code -> Maybe a
  to _ = Nothing
  toType :: a -> Type
  reify :: String -> Maybe a
  reify _ = Nothing

instance Parseable String where
  to (Cd_String s) = Just s
  to _ = Nothing
  toType _ = T_String

instance Parseable Int where
  to (Cd_Int i) = Just i
--  to (Cd_App (Cd_name a) (Cd_Int i)) = do f <- reify a
--                                          return $ f i
--  to (Cd_App (Cd_App (Cd_name a) (Cd_Int i)) (Cd_Int j)) =
--    do f <- reify a
--       return $ f i j
--  to (Cd_App x (Cd_Int i)) = do f <- to x
--                                return $ f i
  to _ = Nothing
  toType _ = T_Int

--instance (Parseable a, Parseable b) => Parseable (a -> b) where
--  toType = t

args :: Code -> [Code]
args (Cd_App a b) = (args a) ++ [b]
args (a@(Cd_name _)) = [a]
args a = fail $ "unexpected arglist head:" ++ show a


{-
instance Parseable (Int -> Int) where
  reify "plusone" = Just (+1)
  reify _ = Nothing

instance Parseable (Int -> Int -> Int) where
  reify "plus" = Just (+)
  reify _ = Nothing
-}

{-

-- ************************************************************ --



pIdentifier :: Parser String
pIdentifier = lexeme $ do x <- lowerChar
                          xs <- many $ choice [alphaNumChar,
                                               single '_',
                                               single '\''
                                              ]
                          return $ x:xs

pOperator :: Parser String
pOperator = lexeme $ some (oneOf ("<>?!|-~+*%$'.#" :: [Char]))


pClosed :: Type -> Parser Code
pClosed need = case need of
                 T_Int      -> Cd_Int <$> signedInteger
                 T_Rational -> Cd_Rational <$> pRatio
                 T_String   -> Cd_String <$> stringLiteral
                 T_Bool     -> Cd_Bool True <$ symbol "True"
                   <|> Cd_Bool False <$ symbol "False"
                 _ -> fail "Not a value"
               <|> parens (pOpen need)

pOpen :: Type -> Parser Code
pOpen need = pFn need <|> pInfix need <|> pClosed need

pInfix :: Type -> Parser Code
pInfix need = do (tok, T_F a (T_F b c)) <- pPeekOp need
                 a' <- dbg "left" $ optional $ try $ pClosed a -- 'try' needed due to numerical + prefix..
                 pOp
                 b' <- optional $ try $ pClosed b
                 let t = case (a', b') of
                           (Nothing, Nothing) -> (T_F a $ T_F b c)
                           (Just _, Nothing) -> (T_F b c)
                           (Nothing, Just _) -> (T_F a c)
                           (Just _, Just _) -> c
                 unless (need == t) $ fail $ "Expected type " ++ show need
                 return $ Cd_Op a' tok b'


pFn :: Type -> Parser Code
pFn need =
  do ident <- dbg "identifier" pIdentifier
     (tok, t) <- case (lookup ident functions) of
                   Just (tok, Prefix, identSig) ->
                     case (fulfill need identSig) of
                       Just t -> return (tok, t)
                       Nothing -> fail $ "Bad type of " ++ ident ++ "\nfound: " ++ show identSig ++ "\ntarget: " ++ show need
                   Nothing -> fail "Unknown function"
                   _ -> fail "TODO - handle infix section"
     args t (arity t - arity need) tok

args :: Type -> Int -> Code -> Parser Code
args _ 0 tok = return tok
args need n tok | n < 0 = error "Internal error, negative args?"
                | otherwise = do let (T_F arg result) = need
                                 argtok <- pClosed arg
                                 args result (n - 1) (Cd_App tok argtok)

pNumber :: Parser ()
pNumber = lexeme $ do optional $ char '-'
                      some digit
                      optional $ try $ do oneOf ['.', '%']
                                          some digit
                      return ()
  where digit = oneOf ['0'..'9']


pSlurpTerm :: Parser ()
pSlurpTerm = do parens pSlurpAll
                  <|> brackets pSlurpAll
                  <|> braces pSlurpAll
                  <|> angles pSlurpAll
                  <|> (stringLiteral >> return ())
                  <|> (pIdentifier >> return ())
                  <|> try pNumber
                return ()

pSlurpAll :: Parser ()
pSlurpAll = do many (pSlurpTerm <|> (pOp >> return ()))
               return ()

pOp :: Parser String
pOp = lexeme $ some $ oneOf "!#$%&*+./<=>?@\\^|-~"

pPeekOp :: Type -> Parser (Code, Type)
pPeekOp need = dbg "pPeekOp" $ try $ lookAhead $
  do many pSlurpTerm
     op <- pOp
     case (lookup op functions) of
       Just (tok, Infix, opSig) ->
         case (fulfill need opSig) of
           Just t -> return (tok, t)
           Nothing -> fail $ "Bad type of op " ++ op ++ "\nfound: " ++ show opSig ++ "\ntarget: " ++ show need
       Nothing -> fail $ "Unknown operator " ++ op
       _ -> fail "Unexpected prefix function.."
             
-}

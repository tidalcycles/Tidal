module Sound.Tidal.MiniTidal.ParseExp where

import           Language.Haskell.Exts
import           Control.Applicative
import           Data.Either (isRight)


data ParseExp a = ParseExp { runParseExp :: Exp SrcSpanInfo -> Either String a }


instance Functor ParseExp where
  fmap f x = ParseExp (fmap f . runParseExp x)


instance Applicative ParseExp where
  pure x = ParseExp (const (Right x))
  f <*> x = ParseExp (\e -> do -- ie. in (Either String) monad
    (e1,e2) <- applicationExpressions e
    f' <- runParseExp f e1
    x' <- runParseExp x e2
    return (f' x')
    )

applicationExpressions :: Exp SrcSpanInfo -> Either String (Exp SrcSpanInfo,Exp SrcSpanInfo)
applicationExpressions (Paren _ x) = applicationExpressions x
applicationExpressions (App _ e1 e2) = Right (e1,e2)
applicationExpressions (InfixApp _ e1 (QVarOp _ (UnQual _ (Symbol _ "$"))) e2) = Right (e1,e2)
applicationExpressions _ = Left ""
-- ?? (InfixApp _ exp1 (QVarOp _ (UnQual _ (Symbol _ op))) exp2) -- ie. how are operators handled in this approach?


instance Alternative ParseExp where
  empty = ParseExp (const (Left ""))
  a <|> b = ParseExp (\e -> do
    let a' = runParseExp a e
    if isRight a' then a' else runParseExp b e
    )


join :: ParseExp (Either String a) -> ParseExp a
join x = ParseExp (\e -> do -- in (Either String)
  x' <- runParseExp x e
  x'
  )


-- Note that a Monad instance is not meaningful for our ParseExp type because it
-- is parsing abstract syntax trees rather than streams of characters - there is
-- no obvious application for the idea of sequencing inherent to monads.


identifier :: ParseExp String
identifier = ParseExp f
  where f (Var _ (UnQual _ (Ident _ x))) = Right x
        f _ = Left ""

reserved :: String -> ParseExp ()
reserved x = ParseExp (\e -> do -- in (Either String)
   e' <- runParseExp identifier e
   if e' == x then Right () else Left ""
   )

symbol :: ParseExp String
symbol = ParseExp f
  where f (Var _ (UnQual _ (Symbol _ x))) = Right x
        f _ = Left ""

string :: ParseExp String
string = ParseExp f
  where f (Lit _ (String _ x _)) = Right x
        f _ = Left ""

integer :: ParseExp Integer
integer = ParseExp f
  where f (NegApp _ (Lit _ (Int _ x _))) = Right (x * (-1))
        f (Lit _ (Int _ x _)) = Right x
        f _ = Left ""

rational :: ParseExp Rational
rational = ParseExp f
  where f (NegApp _ (Lit _ (Frac _ x _))) = Right (x * (-1))
        f (Lit _ (Frac _ x _)) = Right x
        f _ = Left ""

listOf :: ParseExp a -> ParseExp [a]
listOf p = ParseExp (\e -> f e >>= mapM (runParseExp p))
  where
    f (List _ xs) = Right xs
    f _ = Left ""


tupleOf :: ParseExp a -> ParseExp (a,a)
tupleOf p = ParseExp (\e -> do
  (a,b) <- f e
  a' <- runParseExp p a
  b' <- runParseExp p b
  return (a',b')
  )
  where
    f (Tuple _ Boxed (a:b:[])) = Right (a,b)
    f _ = Left ""

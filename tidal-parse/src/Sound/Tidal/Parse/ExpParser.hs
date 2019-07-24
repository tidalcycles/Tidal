module Sound.Tidal.Parse.ExpParser where

import           Language.Haskell.Exts
import           Control.Applicative
import           Data.Either (isRight)


data ExpParser a = ExpParser { runExpParser :: Exp SrcSpanInfo -> Either String a }


instance Functor ExpParser where
  fmap f x = ExpParser (fmap f . runExpParser x)


instance Applicative ExpParser where
  pure x = ExpParser (const (Right x))
  f <*> x = ExpParser (\e -> do -- ie. in (Either String) monad
    (e1,e2) <- applicationExpressions e
    f' <- runExpParser f e1
    x' <- runExpParser x e2
    return (f' x')
    )

applicationExpressions :: Exp SrcSpanInfo -> Either String (Exp SrcSpanInfo,Exp SrcSpanInfo)
applicationExpressions (Paren _ x) = applicationExpressions x
applicationExpressions (App _ e1 e2) = Right (e1,e2)
applicationExpressions (InfixApp _ e1 (QVarOp _ (UnQual _ (Symbol _ "$"))) e2) = Right (e1,e2)
applicationExpressions (InfixApp l e1 (QVarOp _ (UnQual _ (Symbol _ x))) e2) = Right (App l x' e1,e2)
  where x' = (Var l (UnQual l (Ident l x)))
applicationExpressions (LeftSection l e1 (QVarOp _ (UnQual _ (Symbol _ x)))) = Right (x',e1)
  where x' = (Var l (UnQual l (Ident l x)))
applicationExpressions _ = Left ""


instance Alternative ExpParser where
  empty = ExpParser (const (Left ""))
  a <|> b = ExpParser (\e -> do
    let a' = runExpParser a e
    if isRight a' then a' else runExpParser b e
    )


join :: ExpParser (Either String a) -> ExpParser a
join x = ExpParser (\e -> do -- in (Either String)
  x' <- runExpParser x e
  x'
  )


-- Note that a Monad instance is not meaningful for our ExpParser type because it
-- is parsing abstract syntax trees rather than streams of characters - there is
-- thus no obvious application for the idea of sequencing inherent to monads.


identifier :: ExpParser String -- note: we don't distinguish between identifiers and symbols
identifier = ExpParser f
  where f (Paren _ x) = f x
        f (Var _ (UnQual _ (Ident _ x))) = Right x
        f (Var _ (UnQual _ (Symbol _ x))) = Right x
        f _ = Left ""

reserved :: String -> ExpParser ()
reserved x = ExpParser (\e -> do -- in (Either String)
   e' <- runExpParser identifier e
   if e' == x then Right () else Left ""
   )

string :: ExpParser String
string = ExpParser f
  where f (Paren _ x) = f x
        f (Lit _ (String _ x _)) = Right x
        f _ = Left ""

integer :: ExpParser Integer
integer = ExpParser f
  where f (Paren _ x) = f x
        f (NegApp _ (Lit _ (Int _ x _))) = Right (x * (-1))
        f (Lit _ (Int _ x _)) = Right x
        f _ = Left ""

rational :: ExpParser Rational
rational = ExpParser f
  where f (Paren _ x) = f x
        f (NegApp _ (Lit _ (Frac _ x _))) = Right (x * (-1))
        f (Lit _ (Frac _ x _)) = Right x
        f _ = Left ""

listOf :: ExpParser a -> ExpParser [a]
listOf p = ExpParser (\e -> f e >>= mapM (runExpParser p))
  where
    f (Paren _ x) = f x
    f (List _ xs) = Right xs
    f _ = Left ""


tupleOf :: ExpParser a -> ExpParser (a,a)
tupleOf p = ExpParser (\e -> do
  (a,b) <- f e
  a' <- runExpParser p a
  b' <- runExpParser p b
  return (a',b')
  )
  where
    f (Paren _ x) = f x
    f (Tuple _ Boxed (a:b:[])) = Right (a,b)
    f _ = Left ""


asRightSection :: ExpParser (a -> b -> c) -> ExpParser b -> ExpParser (a -> c)
asRightSection opP bP = ExpParser (\e -> do
  (opExp,bExp) <- f e
  op <- runExpParser opP opExp
  b <- runExpParser bP bExp
  return $ flip op b
  )
  where
    f (Paren _ x) = f x
    f (RightSection _ (QVarOp l (UnQual _ (Symbol _ x))) e1) = Right (g l x,e1)
    f _ = Left ""
    g l x = (Var l (UnQual l (Ident l x)))

module Sound.Tidal2.Types where

import Data.List (intersectBy, nub, (\\), intercalate)
import Data.Maybe (isJust)
import Sound.Tidal2.Pattern

-- ************************************************************ --
-- Types of types

data Type =
  T_F Type Type
  | T_String
  | T_Float
  | T_Int
  | T_Rational
  | T_Bool
  | T_Map
  | T_Pattern Type
  | T_Constraint Int
  | T_List Type
  | T_SimpleList Type
  deriving Eq

data Constraint =
  C_OneOf [Type]
  | C_WildCard
  deriving Eq
  
instance Show Type where
 show (T_F a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"
 show T_String = "s"
 show T_Float = "f"
 show T_Int = "i"
 show T_Rational = "r"
 show T_Bool = "#"
 show T_Map = "map"
 show (T_Pattern t) = "p [" ++ (show t) ++ "]"
 show (T_Constraint n) = "constraint#" ++ (show n)
 show (T_List t) = "list [" ++ (show t) ++ "]"
 show (T_SimpleList t) = "simplelist [" ++ (show t) ++ "]"

instance Show Constraint where
 show (C_OneOf ts) = "?" ++ show ts
 show C_WildCard = "*"

-- Type signature
data Sig = Sig {constraints :: [Constraint],
                is :: Type
               }
           deriving Eq

instance Show Sig where
   show s = ps ++ (show $ is s)
     where ps | constraints s == [] = ""
              | otherwise = show (constraints s) ++ " => "

data Code =
  Cd_Int Int | Cd_Rational Rational | Cd_String String | Cd_Float Float | Cd_Bool Bool |
  Cd_App Code Code |
  Cd_Op (Maybe Code) Code (Maybe Code) |
  Cd_R R |
  Cd_every | Cd_fast |
  Cd_plus |
  Cd_multiply |
  Cd_divide |
  Cd_subtract |
  Cd_rev |
  Cd_hash |
  Cd_dollar |
  Cd_pure |
  Cd_name String
  deriving (Show, Eq)

data R = R_Atom String
       | R_Silence
       | R_Subsequence [R]
       | R_StackCycles [R]
       | R_StackStep   [R]
       | R_StackSteps  [R]
       | R_Duration Code R
       | R_Patterning Code R
  deriving (Show, Eq)

data Fix = Prefix | Infix

{-functions :: [(String, (Code, Fix, Sig))]
functions =
   [("+", (Tk_plus, Infix, numOp)),
    ("*", (Tk_multiply, Infix, numOp)),    
    ("/", (Tk_divide, Infix, numOp)),    
    ("-", (Tk_subtract, Infix, numOp)),    
    ("#", (Tk_hash, Infix, ppOp)),
    ("$", (Tk_dollar, Infix, Sig [C_WildCard, C_WildCard] $ T_F (T_F (T_Constraint 0) (T_Constraint 1)) (T_F (T_Constraint 0) (T_Constraint 1)))),
    ("every", (Tk_every, Prefix, i_pf_p)),
    ("rev", (Tk_rev, Prefix, pOp)),
    ("pure", (Tk_pure, Prefix, Sig [C_WildCard] $ T_F (T_Constraint 0) (T_Pattern $ T_Constraint 0)))
   ]
  where pi_pf_p = Sig [C_WildCard] $ T_F (T_Pattern T_Int)
                  (T_F (T_F (T_Pattern $ T_Constraint 0) (T_Pattern $ T_Constraint 0))
                    (T_F (T_Pattern $ T_Constraint 0) (T_Pattern $ T_Constraint 0))
                  )
        i_pf_p = Sig [C_WildCard] $ T_F T_Int
                 (T_F (T_F (T_Pattern $ T_Constraint 0) (T_Pattern $ T_Constraint 0))
                   (T_F (T_Pattern $ T_Constraint 0) (T_Pattern $ T_Constraint 0))
                 )
        numOp = Sig [C_OneOf[T_Float,T_Int,T_Rational]]
                $ T_F (T_Constraint 0) $ T_F (T_Constraint 0) (T_Constraint 0)
                -- $ T_F (T_Pattern $ T_Constraint 0) $ T_F (T_Pattern $ T_Constraint 0) (T_Pattern $ T_Constraint 0)
        sOp = Sig [] $ T_F (T_Pattern $ T_String) (T_Pattern $ T_String)
        pOp = Sig [C_WildCard] $ T_F (T_Pattern $ T_Constraint 0) (T_Pattern $ T_Constraint 0)
        ppOp = Sig [C_WildCard] $ T_F (T_Pattern $ T_Constraint 0) $ T_F (T_Pattern $ T_Constraint 0) (T_Pattern $ T_Constraint 0)
{-
        floatOp = Sig [] $ T_F (T_Pattern T_Float) (T_F (T_Pattern T_Float) (T_Pattern T_Float))
        floatPat = Sig [] $ T_Pattern T_Float
        mapper = Sig [T_WildCard, T_WildCard] $ T_F (T_F (T_Constraint 0) (T_Constraint 1)) $ T_F (T_Pattern (T_Constraint 0)) (T_Pattern (T_Constraint 1))
        stringToPatMap = Sig [] $ T_F (T_Pattern T_String) (T_Pattern T_Map)
        floatToPatMap = Sig [] $ T_F (T_Pattern T_Float) (T_Pattern T_Map)
        number = OneOf [Pattern Float, Pattern Int]
        number = T_Pattern (T_OneOf[T_Float,T_Int])
-}
-}

arity :: Type -> Int
arity (T_F _ b) = (arity b) + 1
arity _ = 0

isFn :: Type -> Bool
isFn (T_F _ _) = True
isFn _ = False

setAt :: [a] -> Int -> a -> [a]
setAt xs i x = take i xs ++ [x] ++ drop (i + 1) xs

fitsConstraint :: Type -> [Constraint]-> Int -> Bool
fitsConstraint t cs i | i >= length cs = error "Internal error - no such constraint"
                      | c == C_WildCard = True
                      | otherwise = or $ map (\t' -> fits' t $ Sig cs t') $ options c
   where c = cs !! i
         options (C_OneOf cs) = cs
         options _ = [] -- can't happen..


fits' :: Type -> Sig -> Bool
fits' t s = isJust $ fits t s

fits :: Type -> Sig -> Maybe ([(Int, Type)])
fits t (Sig cs (T_Constraint i)) = if (fitsConstraint t cs i)
                                   then Just [(i, t)]
                                   else Nothing
fits (T_F arg result) (Sig c (T_F arg' result')) = do as <- fits arg (Sig c arg')
                                                      bs <- fits result (Sig c result')
                                                      return $ as ++ bs
fits (T_Pattern a) (Sig c (T_Pattern b)) = fits a (Sig c b)
fits (T_List a) (Sig c (T_List b)) = fits a (Sig c b)
fits a (Sig _ b) = if a == b
                   then Just []
                   else Nothing

-- How can b produce target a?
-- Will either return the target need, or a function that can
-- return it, or nothing.
fulfill :: Type -> Sig -> Maybe Type
fulfill n c = do (cs, t) <- fulfill' n c
                 resolveConstraint cs t

fulfill' :: Type -> Sig -> Maybe ([(Int, Type)], Type)
fulfill' need contender@(Sig c (T_F arg result))
  | arityD == 0 = do cs <- fits need contender
                     return (cs, need)
  | arityD > 0 = (T_F arg <$>) <$> fulfill' need (Sig c result)
  | otherwise = Nothing
  where arityD = arity (is contender) - arity need
fulfill' need contender = do cs <- fits need contender
                             return (cs, need)

resolveConstraint :: [(Int, Type)] -> Type -> Maybe Type
resolveConstraint cs (T_Constraint n) = lookup n cs
resolveConstraint cs (T_F a b)
  = T_F <$> resolveConstraint cs a <*> resolveConstraint cs b
resolveConstraint cs (T_Pattern t) = T_Pattern <$> resolveConstraint cs t
resolveConstraint cs (T_List t) = T_List <$> resolveConstraint cs t
resolveConstraint _ t = Just t


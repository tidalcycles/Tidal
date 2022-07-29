{-
    Sequence.hs - core representation of Tidal sequences
    Copyright (C) 2022 Alex McLean and contributors

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

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}

module Sound.Tidal.Sequence where

import Prelude hiding (span)
import Data.Ratio

data Strategy = JustifyLeft
              | JustifyRight
              | JustifyBoth
              | Expand
              | TruncateMax
              | TruncateMin
              | RepeatLCM
              | Centre
              deriving Show

data Sequence a = Atom Rational a
                | Silence Rational
                | Sequence [Sequence a]
                | Stack Strategy [Sequence a]
              deriving Show

instance Functor Sequence where
  fmap f (Atom x y) = Atom x (f y)
  fmap f (Silence x) = Silence x
  fmap f (Sequence x) = Sequence $ map (fmap f) x
  fmap f (Stack x y) = Stack x (map (fmap f) y)

instance Applicative Sequence where
  pure x = Atom 1 x
  (Atom x f) <*> something = fmap f something
  Silence x <*> something = Silence x
  Sequence f <*> something = Sequence $ map (<*> something) f
  Stack x f <*> something = Stack x (map (<*> something) f)

instance Monad Sequence where
  return x = Atom 1 x
  Atom x y >>= f = f y
  Silence x >>= f = Silence x
  Sequence x >>= f = Sequence $ map (>>= f) x
  Stack x y >>= f = Stack x (map (>>= f) y)

rev :: Sequence a -> Sequence a
rev (Sequence bs) = Sequence $ reverse $ map rev bs
rev (Stack strategy bs) = Stack strategy $ map rev bs
rev b = b

cat :: [Sequence a] -> Sequence a
cat [] = Silence 0
cat [b] = b
cat bs = Sequence bs

ply :: Int -> Sequence a -> Sequence a
ply n (Atom d v) = Sequence $ replicate n $ Atom (d / (toRational n)) v

seqSpan :: Sequence a -> Rational
seqSpan (Atom s _) = s
seqSpan (Silence s) = s
seqSpan (Sequence bs) = sum $ map seqSpan bs
seqSpan (Stack _ []) = 0
seqSpan (Stack RepeatLCM [b]) = seqSpan b
seqSpan (Stack RepeatLCM (b:bs)) = foldr (lcmRational . seqSpan) (seqSpan b) (b:bs)
seqSpan (Stack TruncateMin (b:bs)) = minimum $ map seqSpan bs
seqSpan (Stack _ bs) = maximum $ map seqSpan bs

lcmRational :: Rational->Rational-> Rational
lcmRational a b = lcm (f a) (f b) % d
  where d = lcm (denominator a) (denominator b)
        f x = numerator x * (d `div` denominator x)

stratApply::Strategy -> [Sequence a] ->Sequence a
stratApply JustifyLeft bs =
  let a = maximum $ map seqSpan bs
      b = map (\x -> Sequence (x: [Silence (a - seqSpan x)])) bs
  in Stack JustifyLeft b

stratApply JustifyRight bs =
  let a = maximum $ map seqSpan bs
      b = map (\x -> Sequence (Silence (a - seqSpan x) : [x])) bs
  in Stack JustifyRight b

stratApply Centre bs =
  let a = maximum $ map seqSpan bs
      b = map( \x -> Sequence ([Silence ((a - seqSpan x)/2)] ++ [x] ++ [Silence ((a - seqSpan x)/2)])) bs
  in Stack Centre b

stratApply RepeatLCM bs@(x:xs) =
  let a = foldr (lcmRational . seqSpan) (seqSpan x) xs
      b = map (\x ->  Sequence $ replicate (fromIntegral $ numerator $ a/seqSpan x) x) bs
  in Stack RepeatLCM b

--JusttifyBoth

--Expand

--TruncateMax

--TruncateMin

-- Functor for sequences
-- Applicative
-- Monad 


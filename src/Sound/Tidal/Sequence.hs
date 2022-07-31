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

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Sound.Tidal.Sequence where

import Data.List(inits)
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
                | Gap Rational
                | Sequence [Sequence a]
                | Stack Strategy [Sequence a]
              deriving Show

instance Functor Sequence where
  fmap f (Atom x y) = Atom x (f y)
  fmap f (Gap x) = Gap x
  fmap f (Sequence x) = Sequence $ map (fmap f) x
  fmap f (Stack x y) = Stack x (map (fmap f) y)

instance Applicative Sequence where
  pure x = Atom 1 x
  (Atom x f) <*> something =
    let (p,q) = forTwo (Atom x f) something
        c = reAlign p q
        d = Sequence $ unwrap $ map (fmap f) c
    in d
  Gap x <*> something =
    let (p,q) = forTwo (Gap x) something
    in Sequence $ unwrap p
  Sequence f <*> something =
    let (p,q) = forTwo (Sequence f) something
        c = unwrap $ reAlign p q
        d = Sequence$ unwrap  $ funcMap p c
    in d
  Stack x f <*> something =
    let d = map (<*> something) f
    in Stack x d


funcMap :: [Sequence (a->b)] -> [Sequence a] -> [Sequence b]
funcMap [] _ = []
funcMap ((Atom x f):xs) y =
  let t = seqSpan (Atom x f)
      p = takeWhile (\q -> sum(map seqSpan q) <= t) $ inits y
      pTill = last p
      q = map (\x -> fmap f x) pTill
  in q ++ funcMap xs (drop (length pTill) y)

reAlign:: [Sequence a] -> [Sequence b] -> [Sequence b]
reAlign [] _ = []
reAlign a@((Gap x):xs) y =
  let t = seqSpan (Gap x)
      p  = takeWhile (\q -> sum (map seqSpan q) <=t ) $ inits y
      pTill = last p
      l = length pTill
      tTill = sum (map seqSpan pTill)
      (a,b) = if tTill == t then (Gap 0, drop l y)  else
        let (m,n) =  getPartition (y!!l) (t - tTill)
        in (m, n :drop (l+1) y)
  in if tTill == t then pTill ++ reAlign xs b else (pTill ++ [a]) ++ reAlign xs b
reAlign a@((Atom x s):xs) y =
  let t = seqSpan (Atom x s)
      p  = takeWhile (\q -> sum (map seqSpan q) <=t ) $ inits y
      pTill = last p
      l = length pTill
      tTill = sum (map seqSpan pTill)
      (a,b) = if tTill == t then (Gap 0, drop l y)  else
        let (m,n) =  getPartition (y!!l) (t - tTill)
        in (m, n :drop (l+1) y)
  in (pTill ++ [a]) ++ reAlign xs b

getPartition::Sequence a-> Rational -> (Sequence a, Sequence a)
getPartition (Atom x s) t = (Atom t s, Atom (x-t) s)
getPartition (Gap x) t = (Gap t, Gap (x - t))
getPartition (Sequence y) t =
  let p = takeWhile (\q -> sum (map seqSpan q) <=t ) $ inits y
      pTill = last p
      l = length pTill
      tTill = sum (map seqSpan pTill)
      (a,b) = if tTill == t then (Gap 0, drop l y)  else
        let (m,n) =  getPartition (y!!l) (t - tTill)
        in (m, n :drop (l+1) y)
  in (Sequence $ pTill ++ [a], Sequence b)

forTwo :: Sequence a1 -> Sequence a2 -> ([Sequence a1], [Sequence a2])
forTwo a b =
  let p = lcmRational (seqSpan a) (seqSpan b)
  in (unwrap $ replicate (fromIntegral  $ numerator $ p/seqSpan a) a, unwrap $ replicate (fromIntegral $ numerator $ p/seqSpan b) b)

unwrap:: [Sequence a] -> [Sequence a]
unwrap [] = []
unwrap [Sequence x] = unwrap x
unwrap (Sequence x :xs) = unwrap x ++ unwrap xs
unwrap (Atom x s:xs) = Atom x s : unwrap xs
unwrap (Gap x:xs) = if x ==0 then unwrap xs else Gap x: unwrap xs

instance Monad Sequence where
  return x = Atom 1 x
  Atom x y >>= f = f y
  Gap x >>= f = Gap x
  Sequence x >>= f = Sequence $ map (>>= f) x
  Stack x y >>= f = Stack x (map (>>= f) y)

rev :: Sequence a -> Sequence a
rev (Sequence bs) = Sequence $ reverse $ map rev bs
rev (Stack strategy bs) = Stack strategy $ map rev bs
rev b = b

cat :: [Sequence a] -> Sequence a
cat [] = Gap 0
cat [b] = b
cat bs = Sequence bs

ply :: Int -> Sequence a -> Sequence a
ply n (Atom d v) = Sequence $ replicate n $ Atom (d / toRational n) v

seqSpan :: Sequence a -> Rational
seqSpan (Atom s _) = s
seqSpan (Gap s) = s
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
      b = map (\x -> Sequence (x: [Gap (a - seqSpan x)])) bs
  in Stack JustifyLeft b

stratApply JustifyRight bs =
  let a = maximum $ map seqSpan bs
      b = map (\x -> Sequence (Gap (a - seqSpan x) : [x])) bs
  in Stack JustifyRight b

stratApply Centre bs =
  let a = maximum $ map seqSpan bs
      b = map( \x -> Sequence ([Gap ((a - seqSpan x)/2)] ++ [x] ++ [Gap ((a - seqSpan x)/2)])) bs
  in Stack Centre b

stratApply RepeatLCM bs@(x:xs) =
  let a = foldr (lcmRational . seqSpan) (seqSpan x) xs
      b = map (\x ->  Sequence $ unwrap $  replicate (fromIntegral $ numerator $ a/seqSpan x) x) bs
  in Stack RepeatLCM b


--JusttifyBoth

--Expand

--TruncateMax

--TruncateMin

-- Functor for sequences
-- Applicative
-- Monad 


-- When applying a strategy on sequences, return in standardized form?
-- Maybe return in form of the first sequence or something?
-- So when stacking it would be standardized to the first sequence in list w.r.t the duration 
-- Like for each event in the first sequence, we will have an event or a sequence of events in the second sequence.
-- This will make mapping these sequences really easy

-- Method to unwrap sequences as well -- Done




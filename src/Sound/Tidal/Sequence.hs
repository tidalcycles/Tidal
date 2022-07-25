
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

data Branch a = Atom Rational a
              | Silence Rational
              | Sequence [Branch a]
              | Stack Strategy [Branch a]
              deriving Show

rev :: Branch a -> Branch a
rev (Sequence bs) = Sequence $ reverse $ map rev bs
rev (Stack strategy bs) = Stack strategy $ map rev bs
rev b = b

cat :: [Branch a] -> Branch a
cat [] = Silence 0
cat [b] = b
cat bs = Sequence bs

seqSpan :: Branch a -> Rational
seqSpan (Atom s _) = s
seqSpan (Silence s) = s
seqSpan (Sequence bs) = sum $ map seqSpan bs
seqSpan (Stack _ []) = 0
seqSpan (Stack RepeatLCM [b]) = seqSpan b
seqSpan (Stack RepeatLCM (b:bs)) = foldr lcmRational (seqSpan b) $ map seqSpan bs
seqSpan (Stack TruncateMin (b:bs)) = minimum $ map seqSpan bs
seqSpan (Stack _ bs) = maximum $ map seqSpan bs

lcmRational a b = (lcm (f a) (f b)) % d
  where d = lcm (denominator a) (denominator b)
        f x = numerator x * (d `div` denominator x)
        

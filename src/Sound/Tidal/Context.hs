module Sound.Tidal.Context (module C) where

{-
    Context.hs - For exposing the core TidalCycles libraries
    Copyright (C) 2020, Alex McLean and contributors

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

import Prelude hiding ((<*), (*>))

import Data.Ratio as C

import Sound.Tidal.Config as C
import Sound.Tidal.Control as C
import Sound.Tidal.Core as C hiding (rev, cat, ply, stack, euclid, fast, slow)
import Sound.Tidal.Params as C
import Sound.Tidal.ParseBP as C
import Sound.Tidal.Pattern as C
import Sound.Tidal.Scales as C
import Sound.Tidal.Sequence as C hiding (rev, cat, ply, stack, 
                                         -- not yet implemented / namespace conflicts 
                                         euclid, unwrap, fast, slow
                                        )
import Sound.Tidal.Show as C
import Sound.Tidal.Simple as C
import Sound.Tidal.Stream as C
import Sound.Tidal.Transition as C
import Sound.Tidal.UI as C
import Sound.Tidal.Version as C

import Sound.Tidal.Pattern as Pat
import Sound.Tidal.Core as Pat
import Sound.Tidal.UI as Pat

import Sound.Tidal.Sequence as Seq

class Transformable f where
  rev :: f a -> f a
  cat :: [f a] -> f a
  stack :: [f a] -> f a
  euclid :: f Int -> f Int -> f String -> f String
  fast :: f Rational -> f a -> f a
  slow :: f Rational -> f a -> f a

instance Transformable Pattern where
  rev = Pat.rev
  cat = Pat.cat
  stack = Pat.stack
  euclid = Pat.euclid
  fast = Pat.fast
  slow = Pat.slow

instance Transformable Sequence where
  rev = Seq.rev
  cat = Seq.cat
  stack = Seq.stack
  -- euclid = Seq.euclid - doesn't yet match
  -- fast = Seq.fast
  -- slow = Seq.slow

{-

seqPat :: Seq.Sequence a -> Pat.Pattern a
seqPat (Seq.Atom _ a) = pure a
seqPat (Seq.Gap _) = Pat.silence
seqPat (Seq.Sequence bs) = Pat.timecat $ map (\b -> (seqSpan b, seqPat b)) bs
seqPat (Seq.Stack Expand bs) = Pat.stack $ map seqPat bs
seqPat b@(Seq.Stack JustifyLeft bs) =
  Pat.stack $ map (\b' -> _fastGap (seqSpan b / seqSpan b') $ seqPat b') bs
seqPat b@(Seq.Stack JustifyRight bs) =
  Pat.stack $
    map (\b' -> rotR (1- (1/(seqSpan b / seqSpan b'))) $ _fastGap (seqSpan b / seqSpan b') $ seqPat b') bs
seqPat b@(Seq.Stack Centre bs) = Pat.stack $
    map (\b' -> rotR (1.5/(seqSpan b / seqSpan b')) $ _fastGap (seqSpan b / seqSpan b') $ seqPat b') bs

data Strategy = JustifyBoth
              | Expand
              | TruncateMax
              | TruncateMin
              | RepeatLCM
-}

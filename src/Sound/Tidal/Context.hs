module Sound.Tidal.Context (module C,
                            Sound.Tidal.Context.rev,
                            Sound.Tidal.Context.cat,
                            Sound.Tidal.Context.ply,
                            Sound.Tidal.Context.stack,
                            Sound.Tidal.Context.euclid,
                            Sound.Tidal.Context.fast,
                            Sound.Tidal.Context.slow,
                            Sound.Tidal.Context.seqPat,
                            Sound.Tidal.Context._euclid,
                            Sound.Tidal.Context._slow,
                            Sound.Tidal.Context._fast,
                            Sound.Tidal.Context.timeCat,
                            Sound.Tidal.Context.timecat,
                            Sound.Tidal.Context.fastAppend,
                            Sound.Tidal.Context.fastappend,
                            Sound.Tidal.Context.slowAppend,
                            Sound.Tidal.Context.slowappend,
                            Sound.Tidal.Context.append,
                            Sound.Tidal.Context.fromList,
                            Sound.Tidal.Context.fastFromList,
                            Sound.Tidal.Context.fromMaybes,
                            Sound.Tidal.Context.run,
                            Sound.Tidal.Context._run,
                            Sound.Tidal.Context.scan,
                            Sound.Tidal.Context._scan,
                            Sound.Tidal.Context.every,
                            Sound.Tidal.Context._every,
                            Sound.Tidal.Context.listToPat,
                            Sound.Tidal.Context.fastcat,
                            Sound.Tidal.Context.fastCat,
                            Sound.Tidal.Context.slowcat,
                            Sound.Tidal.Context.slowCat,
                            Sound.Tidal.Context.density,
                            Sound.Tidal.Context.iter,
                            Sound.Tidal.Context.iter',
                            Sound.Tidal.Context._iter,
                            Sound.Tidal.Context._iter',
                            Sound.Tidal.Context.rotL,
                            Sound.Tidal.Context.rotR,
                            (Sound.Tidal.Context.<~),
                            (Sound.Tidal.Context.~>)
                           ) where

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
import Sound.Tidal.Core as C hiding (rev, cat, stack, fast, slow,_slow, slowcat, _fast, timeCat, timecat, fastAppend, fastappend,
                                      slowAppend, slowappend, append, fromList, fastFromList,  fromMaybes, run, _run, scan,
                                      _scan, every, _every,listToPat, fastCat, fastcat, slowCat, slowcat,density, (<~), (~>)
                                      )
import Sound.Tidal.Params as C
import Sound.Tidal.ParseBP as C
import Sound.Tidal.Pattern as C hiding (rotL, rotR)
import Sound.Tidal.Scales as C
import Sound.Tidal.Sequence as C hiding (rev, cat, ply, stack,
                                         unwrap, fast, slow, euclid, _euclid, _slow, _fast,
                                         timeCat, timecat,fastAppend, fastappend, slowAppend, slowappend, append,
                                         fromList, fastFromList, fromMaybes, run, _run, scan, _scan, every, _every, every',
                                         listToPat, fastcat, fastCat, slowcat, slowCat, density, iter, _iter, iter', _iter', 
                                         rotL, rotR, (<~), (~>)
                                        )
import Sound.Tidal.Show as C
import Sound.Tidal.Simple as C
import Sound.Tidal.Stream as C
import Sound.Tidal.Transition as C
import Sound.Tidal.UI as C hiding (_ply,ply, euclid,_euclid, iter, _iter, iter', _iter')
import Sound.Tidal.Version as C

import Sound.Tidal.Pattern as Pat
import Sound.Tidal.Core as Pat  
import Sound.Tidal.UI as Pat

import Sound.Tidal.Sequence as Seq

class Transformable f where
  rev :: f a -> f a
  cat :: [f a] -> f a
  ply :: f Rational -> f a -> f a
  _ply :: Rational ->f a-> f a
  stack :: [f a] -> f a
  euclid :: f Int -> f Int -> f String -> f String
  fast :: f Rational -> f a -> f a
  slow :: f Rational -> f a -> f a
  _euclid :: Int -> Int -> f a-> f a
  _slow :: Rational -> f a -> f a
  _fast :: Rational -> f a -> f a
  timeCat :: [(Rational, f a)] -> f a
  timecat :: [(Rational, f a)] -> f a
  fastAppend :: f a -> f a -> f a
  fastappend :: f a -> f a -> f a
  slowAppend :: f a -> f a -> f a
  slowappend :: f a -> f a -> f a
  append :: f a ->  f a -> f a
  fromList :: [a] -> f a
  fastFromList :: [a] -> f a
  fromMaybes :: [Maybe a] -> f a
  run :: (Enum a, Num a) => f a -> f a
  _run :: (Enum a, Num a) => a -> f a
  scan :: (Enum a, Num a) => f a -> f a
  _scan :: (Enum a, Num a) => a -> f a
  every :: f Int -> (f b -> f b) -> f b -> f b
  _every :: Int -> (f a -> f a) -> f a -> f a
  listToPat :: [a] -> f a
  fastcat :: [f a] -> f a
  fastCat :: [f a] -> f a
  slowcat :: [f a] -> f a
  slowCat :: [f a] -> f a
  density :: f Rational -> f a-> f a
  rotL :: Rational -> f a -> f a
  rotR :: Rational -> f a -> f a
  iter :: f Int -> f a -> f a
  iter' :: f Int -> f a -> f a
  _iter :: Int -> f a -> f a
  _iter' :: Int -> f a -> f a
  (<~) :: f Rational -> f a -> f a
  (~>) :: f Rational -> f a -> f a

instance Transformable Pattern where
  rev = Pat.rev
  cat = Pat.cat
  ply = Pat.ply
  _ply = Pat._ply
  stack = Pat.stack
  euclid = Pat.euclid
  fast = Pat.fast
  slow = Pat.slow
  _euclid = Pat._euclid
  _slow = Pat._slow
  _fast = Pat._fast
  timeCat = Pat.timeCat
  timecat = Pat.timecat
  fastAppend = Pat.fastAppend
  fastappend = Pat.fastappend
  slowAppend = Pat.slowAppend
  slowappend = Pat.slowappend
  append = Pat.append
  fromList = Pat.fromList
  fastFromList = Pat.fastFromList
  fromMaybes = Pat.fromMaybes
  run = Pat.run
  _run = Pat._run
  scan = Pat.scan
  _scan = Pat._scan
  every = Pat.every
  _every  = Pat._every
  listToPat = Pat.listToPat
  fastCat = Pat.fastCat
  fastcat = Pat.fastcat
  slowcat = Pat.slowcat
  slowCat = Pat.slowCat
  density = Pat.density
  rotL = Pat.rotL
  rotR = Pat.rotR
  iter = Pat.iter
  iter' = Pat.iter'
  _iter = Pat._iter
  _iter' = Pat._iter'
  (<~) = (Pat.<~)
  (~>) = (Pat.~>)

instance Transformable Sequence where
  rev = Seq.rev
  cat = Seq.cat
  ply = Seq.ply
  _ply = Seq._ply
  stack = Seq.stack
  euclid = Seq.euclid
  fast = Seq.fast
  slow = Seq.slow
  _euclid = Seq._euclid
  _slow = Seq._slow
  _fast = Seq._fast
  timeCat = Seq.timeCat
  timecat = Seq.timecat
  fastAppend = Seq.fastAppend
  fastappend = Seq.fastappend
  slowAppend = Seq.slowAppend
  slowappend = Seq.slowappend
  append = Seq.append
  fromList = Seq.fromList
  fastFromList = Seq.fastFromList
  fromMaybes = Seq.fromMaybes
  run = Seq.run
  _run = Seq._run
  scan = Seq.scan
  _scan = Seq._scan
  every = Seq.every
  _every = Seq._every
  listToPat = Seq.listToPat
  fastCat = Seq.fastCat
  fastcat = Seq.fastcat
  slowCat = Seq.slowCat
  slowcat = Seq.slowcat
  density = Seq.density
  rotL = Seq.rotL
  rotR = Seq.rotR
  iter = Seq.iter
  iter' = Seq.iter'
  _iter = Seq._iter
  _iter' = Seq._iter'
  (<~) = (Seq.<~)
  (~>) = (Seq.~>)


seqPat :: Seq.Sequence a  -> Pat.Pattern a
seqPat (Seq.Atom x a) =  Pat.slow (fromRational$ toRational x) (pure a)
seqPat (Seq.Gap x) =  Pat.slow (fromRational $ toRational x) (Pat.silence)
seqPat (Seq.Sequence bs) = let t = Seq.seqSpan (Seq.Sequence bs) in 
  Pat.slow (fromRational $ toRational t) $ Pat.timecat $ map (\b -> (seqSpan b, seqPatHelp b)) bs
seqPat (Seq.Stack bs) = let t = Seq.seqSpan (Seq.Stack bs) in
   Pat.slow (fromRational $ toRational t) $ Pat.stack $ map seqPat bs

seqPatHelp :: Seq.Sequence a -> Pat.Pattern a
seqPatHelp (Seq.Atom _ a) = pure a
seqPatHelp (Seq.Gap _) = Pat.silence
seqPatHelp (Seq.Sequence bs) = Pat.timecat $ map (\b -> (seqSpan b, seqPatHelp b)) bs
seqPatHelp (Seq.Stack bs) = Pat.stack $ map seqPat bs
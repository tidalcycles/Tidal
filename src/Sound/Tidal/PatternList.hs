{-# LANGUAGE OverloadedLists, TypeFamilies #-}

module Sound.Tidal.PatternList where

import Sound.Tidal.Pattern
import GHC.Exts( IsList(..) )

instance (Ord a) => IsList (Pattern a) where
  type Item (Pattern a) = a
  fromList = listToPat
  toList   = patToList


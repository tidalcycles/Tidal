{-# LANGUAGE FlexibleInstances #-}

-- (c) Alex McLean and contributors 2022
-- Shared under the terms of the GNU Public License v3.0

-- Additional ways to compose signals together that don't fit in the
-- what/how scheme in Compose.hs

module Sound.Tidal.Signal.ComposeExtra where

import           Prelude                 hiding ((*>), (<*))

import           Sound.Tidal.Pattern
import           Sound.Tidal.Signal.Base
import           Sound.Tidal.Types

-- | @wedge t p p'@ combines signals @p@ and @p'@ by squashing the
-- @p@ into the portion of each cycle given by @t@, and @p'@ into the
-- remainer of each cycle.
wedge :: Signal Time -> Signal a -> Signal a -> Signal a
wedge pt pa pb = innerJoin $ (\t -> _wedge t pa pb) <$> pt

_wedge :: Time -> Signal a -> Signal a -> Signal a
_wedge 0 _ p' = p'
_wedge 1 p _  = p
_wedge t p p' = overlay (_fastGap (1/t) p) (t `_late` _fastGap (1/(1-t)) p')

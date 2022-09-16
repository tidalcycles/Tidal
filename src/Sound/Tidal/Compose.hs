{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module Sound.Tidal.Compose where

import Prelude hiding ((<*), (*>))
import Language.Haskell.TH
import Control.Monad (forM)

import qualified Data.Map.Strict as Map

import Sound.Tidal.Signal

-- ************************************************************ --
-- Hack to allow 'union' to be used on any value

-- class for types that support a left-biased union
class Unionable a where
  union :: a -> a -> a

-- default union is just to take the left hand side..
instance Unionable a where
  union = const

instance {-# OVERLAPPING #-} Unionable ValueMap where
  union = Map.union

-- ************************************************************ --

opMix :: Applicative t => (a -> b -> c) -> t a -> t b -> t c
opMix f a b = f <$> a <*> b

opIn :: (a -> b -> c) -> Signal a -> Signal b -> Signal c
opIn f a b = f <$> a <* b
  
opOut :: (a -> b -> c) -> Signal a -> Signal b -> Signal c
opOut f a b = f <$> a *> b

opSqueeze :: (a -> b -> c) -> Signal a -> Signal b -> Signal c
opSqueeze f a b = squeezeJoin $ fmap (\a -> fmap (\b -> f a b)  b) a
  
opSqueezeOut :: (a -> b -> c) -> Signal a -> Signal b -> Signal c
opSqueezeOut f pata patb = squeezeJoin $ fmap (\a -> fmap (\b -> f b a)  pata) patb

opTrig :: (a -> b -> c) -> Signal a -> Signal b -> Signal c
opTrig f a b = trigJoin $ fmap (\a -> fmap (\b -> f a b)  b) a
  
opTrigZero :: (a -> b -> c) -> Signal a -> Signal b -> Signal c
opTrigZero f a b = trigZeroJoin $ fmap (\a -> fmap (\b -> f a b)  b) a

-- set

setMix, mix, (|<|) :: Unionable a => Signal a -> Signal a -> Signal a
setMix = opMix union
mix = setMix
(|<|) = setMix

setIn, (|<) :: (Unionable a, Num a) => Signal a -> Signal a -> Signal a
setIn = opIn union
(|<) = setIn

setOut, (<|) :: (Unionable a, Num a) => Signal a -> Signal a -> Signal a
setOut = opOut union
(<|) = setOut

setSqueeze, (||<) :: (Unionable a, Num a) => Signal a -> Signal a -> Signal a
setSqueeze = opSqueeze union
(||<) = setSqueeze

setSqueezeOut, (<||) :: (Unionable a, Num a) => Signal a -> Signal a -> Signal a
setSqueezeOut = opSqueezeOut union
(<||) = setSqueezeOut

setTrig, (!<) :: (Unionable a, Num a) => Signal a -> Signal a -> Signal a
setTrig = opTrig union
(!<) = setTrig

setTrigZero, (!!<) :: (Unionable a, Num a) => Signal a -> Signal a -> Signal a
setTrigZero = opTrigZero union
(!!<) = setTrig

infix 4 |<|, |<, <|, ||<, <||, !<, !!<

-- add

addMix, (|+|) :: Num a => Signal a -> Signal a -> Signal a
addMix = opMix (+)
(|+|) = addMix

addIn, (|+) :: Num a => Signal a -> Signal a -> Signal a
addIn = opIn (+)
(|+) = addIn

addOut, (+|) :: Num a => Signal a -> Signal a -> Signal a
addOut = opOut (+)
(+|) = addOut

addSqueeze, (||+) :: Num a => Signal a -> Signal a -> Signal a
addSqueeze = opSqueeze (+)
(||+) = addSqueeze

addSqueezeOut, (+||) :: Num a => Signal a -> Signal a -> Signal a
addSqueezeOut = opSqueezeOut (+)
(+||) = addSqueezeOut

addTrig, (!+) :: Num a => Signal a -> Signal a -> Signal a
addTrig = opTrig (+)
(!+) = addTrig

addTrigZero, (!!+) :: Num a => Signal a -> Signal a -> Signal a
addTrigZero = opTrigZero (+)
(!!+) = addTrig

infix 4 |+|, |+, +|, ||+, +||, !+, !!+

-- sub

subMix, (|-|) :: Num a => Signal a -> Signal a -> Signal a
subMix = opMix (-)
(|-|) = subMix

subIn, (|-) :: Num a => Signal a -> Signal a -> Signal a
subIn = opIn (-)
(|-) = subIn

subOut, (-|) :: Num a => Signal a -> Signal a -> Signal a
subOut = opOut (-)
(-|) = subOut

subSqueeze, (||-) :: Num a => Signal a -> Signal a -> Signal a
subSqueeze = opSqueeze (-)
(||-) = subSqueeze

subSqueezeOut, (-||) :: Num a => Signal a -> Signal a -> Signal a
subSqueezeOut = opSqueezeOut (-)
(-||) = subSqueezeOut

subTrig, (!-) :: Num a => Signal a -> Signal a -> Signal a
subTrig = opTrig (-)
(!-) = subTrig

subTrigZero, (!!-) :: Num a => Signal a -> Signal a -> Signal a
subTrigZero = opTrigZero (-)
(!!-) = subTrig

infix 4 |-|, |-, -|, ||-, -||, !-, !!-

-- div

divMix, (|/|) :: Fractional a => Signal a -> Signal a -> Signal a
divMix = opMix (/)
(|/|) = divMix

divIn, (|/) :: Fractional a => Signal a -> Signal a -> Signal a
divIn = opIn (/)
(|/) = divIn

divOut, (/|) :: Fractional a => Signal a -> Signal a -> Signal a
divOut = opOut (/)
(/|) = divOut

divSqueeze, (||/) :: Fractional a => Signal a -> Signal a -> Signal a
divSqueeze = opSqueeze (/)
(||/) = divSqueeze

divSqueezeOut, (/||) :: Fractional a => Signal a -> Signal a -> Signal a
divSqueezeOut = opSqueezeOut (/)
(/||) = divSqueezeOut

divTrig, (!/) :: Fractional a => Signal a -> Signal a -> Signal a
divTrig = opTrig (/)
(!/) = divTrig

divTrigZero, (!!/) :: Fractional a => Signal a -> Signal a -> Signal a
divTrigZero = opTrigZero (/)
(!!/) = divTrig

infix 4 |/|, |/, /|, ||/, /||, !/, !!/

-- mul

mulMix, (|*|) :: Num a => Signal a -> Signal a -> Signal a
mulMix = opMix (*)
(|*|) = mulMix

mulIn, (|*) :: Num a => Signal a -> Signal a -> Signal a
mulIn = opIn (*)
(|*) = mulIn

mulOut, (*|) :: Num a => Signal a -> Signal a -> Signal a
mulOut = opOut (*)
(*|) = mulOut

mulSqueeze, (||*) :: Num a => Signal a -> Signal a -> Signal a
mulSqueeze = opSqueeze (*)
(||*) = mulSqueeze

mulSqueezeOut, (*||) :: Num a => Signal a -> Signal a -> Signal a
mulSqueezeOut = opSqueezeOut (*)
(*||) = mulSqueezeOut

mulTrig, (!*) :: Num a => Signal a -> Signal a -> Signal a
mulTrig = opTrig (*)
(!*) = mulTrig

mulTrigZero, (!!*) :: Num a => Signal a -> Signal a -> Signal a
mulTrigZero = opTrigZero (*)
(!!*) = mulTrig

infix 4 |*|, |*, *|, ||*, *||, !*, !!*

-- mod

modMix, (|%|) :: Integral a => Signal a -> Signal a -> Signal a
modMix = opMix mod
(|%|) = modMix

modIn, (|%) :: Integral a => Signal a -> Signal a -> Signal a
modIn = opIn mod
(|%) = modIn

modOut, (%|) :: Integral a => Signal a -> Signal a -> Signal a
modOut = opOut mod
(%|) = modOut

modSqueeze, (||%) :: Integral a => Signal a -> Signal a -> Signal a
modSqueeze = opSqueeze mod
(||%) = modSqueeze

modSqueezeOut, (%||) :: Integral a => Signal a -> Signal a -> Signal a
modSqueezeOut = opSqueezeOut mod
(%||) = modSqueezeOut

modTrig, (!%) :: Integral a => Signal a -> Signal a -> Signal a
modTrig = opTrig mod
(!%) = modTrig

modTrigZero, (!!%) :: Integral a => Signal a -> Signal a -> Signal a
modTrigZero = opTrigZero mod
(!!%) = modTrig

infix 4 |%|, |%, %|, ||%, %||, !%, !!%

-- pow

powMix, (|^|) :: Integral a => Signal a -> Signal a -> Signal a
powMix = opMix (^)
(|^|) = powMix

powIn, (|^) :: Integral a => Signal a -> Signal a -> Signal a
powIn = opIn (^)
(|^) = powIn

powOut, (^|) :: Integral a => Signal a -> Signal a -> Signal a
powOut = opOut (^)
(^|) = powOut

powSqueeze, (||^) :: Integral a => Signal a -> Signal a -> Signal a
powSqueeze = opSqueeze (^)
(||^) = powSqueeze

powSqueezeOut, (^||) :: Integral a => Signal a -> Signal a -> Signal a
powSqueezeOut = opSqueezeOut (^)
(^||) = powSqueezeOut

powTrig, (!^) :: Integral a => Signal a -> Signal a -> Signal a
powTrig = opTrig (^)
(!^) = powTrig

powTrigZero, (!!^) :: Integral a => Signal a -> Signal a -> Signal a
powTrigZero = opTrigZero (^)
(!!^) = powTrig

infix 4 |^|, |^, ^|, ||^, ^||, !^, !!^

-- ************************************************************ --

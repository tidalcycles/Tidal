{-# LANGUAGE FlexibleInstances #-}

module Sound.Tidal.Compose where

import Prelude hiding ((<*), (*>))
import Control.Monad (forM)
import Data.Bits

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

setMix, mix, (|=|) :: Unionable a => Signal a -> Signal a -> Signal a
setMix = opMix (flip union)
mix = setMix
(|=|) = setMix

setIn, (|=) :: (Unionable a, Num a) => Signal a -> Signal a -> Signal a
setIn = opIn (flip union)
(|=) = setIn

setOut, (=|) :: (Unionable a, Num a) => Signal a -> Signal a -> Signal a
setOut = opOut (flip union)
(=|) = setOut

setSqueeze, (||=) :: (Unionable a, Num a) => Signal a -> Signal a -> Signal a
setSqueeze = opSqueeze (flip union)
(||=) = setSqueeze

setSqueezeOut, (=||) :: (Unionable a, Num a) => Signal a -> Signal a -> Signal a
setSqueezeOut = opSqueezeOut (flip union)
(=||) = setSqueezeOut

setTrig, (!=) :: (Unionable a, Num a) => Signal a -> Signal a -> Signal a
setTrig = opTrig (flip union)
(!=) = setTrig

setTrigZero, (!!=) :: (Unionable a, Num a) => Signal a -> Signal a -> Signal a
setTrigZero = opTrigZero (flip union)
(!!=) = setTrig

infix 4 |=|, |=, =|, ||=, =||, !=, !!=

-- keep

keepMix, (|.|) :: Unionable a => Signal a -> Signal a -> Signal a
keepMix = opMix union
(|.|) = keepMix

keepIn, (|.) :: (Unionable a, Num a) => Signal a -> Signal a -> Signal a
keepIn = opIn union
(|.) = keepIn

keepOut, (.|) :: (Unionable a, Num a) => Signal a -> Signal a -> Signal a
keepOut = opOut union
(.|) = keepOut

keepSqueeze, (||.) :: (Unionable a, Num a) => Signal a -> Signal a -> Signal a
keepSqueeze = opSqueeze union
(||.) = keepSqueeze

keepSqueezeOut, (.||) :: (Unionable a, Num a) => Signal a -> Signal a -> Signal a
keepSqueezeOut = opSqueezeOut union
(.||) = keepSqueezeOut

keepTrig, (!.) :: (Unionable a, Num a) => Signal a -> Signal a -> Signal a
keepTrig = opTrig union
(!.) = keepTrig

keepTrigZero, (!!.) :: (Unionable a, Num a) => Signal a -> Signal a -> Signal a
keepTrigZero = opTrigZero union
(!!.) = keepTrig

infix 4 |.|, |., .|, ||., .||, !., !!.

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

-- and (bitwise)

bandMix, (|.&.|) :: Bits a => Signal a -> Signal a -> Signal a
bandMix = opMix (.&.)
(|.&.|) = bandMix

bandIn, (|.&.) :: Bits a => Signal a -> Signal a -> Signal a
bandIn = opIn (.&.)
(|.&.) = bandIn

bandOut, (.&.|) :: Bits a => Signal a -> Signal a -> Signal a
bandOut = opOut (.&.)
(.&.|) = bandOut

bandSqueeze, (||.&.) :: Bits a => Signal a -> Signal a -> Signal a
bandSqueeze = opSqueeze (.&.)
(||.&.) = bandSqueeze

bandSqueezeOut, (.&.||) :: Bits a => Signal a -> Signal a -> Signal a
bandSqueezeOut = opSqueezeOut (.&.)
(.&.||) = bandSqueezeOut

bandTrig, (!.&.) :: Bits a => Signal a -> Signal a -> Signal a
bandTrig = opTrig (.&.)
(!.&.) = bandTrig

bandTrigZero, (!!.&.) :: Bits a => Signal a -> Signal a -> Signal a
bandTrigZero = opTrigZero (.&.)
(!!.&.) = bandTrig

infix 4 |.&.|, |.&., .&.|, ||.&., .&.||, !.&., !!.&.

-- or (bitwise)

borMix, (|.|.|) :: Bits a => Signal a -> Signal a -> Signal a
borMix = opMix (.|.)
(|.|.|) = borMix

borIn, (|.|.) :: Bits a => Signal a -> Signal a -> Signal a
borIn = opIn (.|.)
(|.|.) = borIn

borOut, (.|.|) :: Bits a => Signal a -> Signal a -> Signal a
borOut = opOut (.|.)
(.|.|) = borOut

borSqueeze, (||.|.) :: Bits a => Signal a -> Signal a -> Signal a
borSqueeze = opSqueeze (.|.)
(||.|.) = borSqueeze

borSqueezeOut, (.|.||) :: Bits a => Signal a -> Signal a -> Signal a
borSqueezeOut = opSqueezeOut (.|.)
(.|.||) = borSqueezeOut

borTrig, (!.|.) :: Bits a => Signal a -> Signal a -> Signal a
borTrig = opTrig (.|.)
(!.|.) = borTrig

borTrigZero, (!!.|.) :: Bits a => Signal a -> Signal a -> Signal a
borTrigZero = opTrigZero (.|.)
(!!.|.) = borTrig

infix 4 |.|.|, |.|., .|.|, ||.|., .|.||, !.|., !!.|.

-- xor

xorMix, (|.^.|) :: Bits a => Signal a -> Signal a -> Signal a
xorMix = opMix xor
(|.^.|) = xorMix

xorIn, (|.^.) :: Bits a => Signal a -> Signal a -> Signal a
xorIn = opIn xor
(|.^.) = xorIn

xorOut, (.^.|) :: Bits a => Signal a -> Signal a -> Signal a
xorOut = opOut xor
(.^.|) = xorOut

xorSqueeze, (||.^.) :: Bits a => Signal a -> Signal a -> Signal a
xorSqueeze = opSqueeze xor
(||.^.) = xorSqueeze

xorSqueezeOut, (.^.||) :: Bits a => Signal a -> Signal a -> Signal a
xorSqueezeOut = opSqueezeOut xor
(.^.||) = xorSqueezeOut

xorTrig, (!.^.) :: Bits a => Signal a -> Signal a -> Signal a
xorTrig = opTrig xor
(!.^.) = xorTrig

xorTrigZero, (!!.^.) :: Bits a => Signal a -> Signal a -> Signal a
xorTrigZero = opTrigZero xor
(!!.^.) = xorTrig

infix 4 |.^.|, |.^., .^.|, ||.^., .^.||, !.^., !!.^.

-- lshift

lshiftMix, (|.<<.|) :: Bits a => Signal a -> Signal Int -> Signal a
lshiftMix = opMix shiftL
(|.<<.|) = lshiftMix

lshiftIn, (|.<<.) :: Bits a => Signal a -> Signal Int -> Signal a
lshiftIn = opIn shiftL
(|.<<.) = lshiftIn

lshiftOut, (.<<.|) :: Bits a => Signal a -> Signal Int -> Signal a
lshiftOut = opOut shiftL
(.<<.|) = lshiftOut

lshiftSqueeze, (||.<<.) :: Bits a => Signal a -> Signal Int -> Signal a
lshiftSqueeze = opSqueeze shiftL
(||.<<.) = lshiftSqueeze

lshiftSqueezeOut, (.<<.||) :: Bits a => Signal a -> Signal Int -> Signal a
lshiftSqueezeOut = opSqueezeOut shiftL
(.<<.||) = lshiftSqueezeOut

lshiftTrig, (!.<<.) :: Bits a => Signal a -> Signal Int -> Signal a
lshiftTrig = opTrig shiftL
(!.<<.) = lshiftTrig

lshiftTrigZero, (!!.<<.) :: Bits a => Signal a -> Signal Int -> Signal a
lshiftTrigZero = opTrigZero shiftL
(!!.<<.) = lshiftTrig

infix 4 |.<<.|, |.<<., .<<.|, ||.<<., .<<.||, !.<<., !!.<<.

-- rshift

rshiftMix, (|.>>.|) :: Bits a => Signal a -> Signal Int -> Signal a
rshiftMix = opMix shiftR
(|.>>.|) = rshiftMix

rshiftIn, (|.>>.) :: Bits a => Signal a -> Signal Int -> Signal a
rshiftIn = opIn shiftR
(|.>>.) = rshiftIn

rshiftOut, (.>>.|) :: Bits a => Signal a -> Signal Int -> Signal a
rshiftOut = opOut shiftR
(.>>.|) = rshiftOut

rshiftSqueeze, (||.>>.) :: Bits a => Signal a -> Signal Int -> Signal a
rshiftSqueeze = opSqueeze shiftR
(||.>>.) = rshiftSqueeze

rshiftSqueezeOut, (.>>.||) :: Bits a => Signal a -> Signal Int -> Signal a
rshiftSqueezeOut = opSqueezeOut shiftR
(.>>.||) = rshiftSqueezeOut

rshiftTrig, (!.>>.) :: Bits a => Signal a -> Signal Int -> Signal a
rshiftTrig = opTrig shiftR
(!.>>.) = rshiftTrig

rshiftTrigZero, (!!.>>.) :: Bits a => Signal a -> Signal Int -> Signal a
rshiftTrigZero = opTrigZero shiftR
(!!.>>.) = rshiftTrig

infix 4 |.>>.|, |.>>., .>>.|, ||.>>., .>>.||, !.>>., !!.>>.

-- lt

ltMix, (|<|) :: Ord a => Signal a -> Signal a -> Signal Bool
ltMix = opMix (<)
(|<|) = ltMix

ltIn, (|<) :: Ord a => Signal a -> Signal a -> Signal Bool
ltIn = opIn (<)
(|<) = ltIn

ltOut, (<|) :: Ord a => Signal a -> Signal a -> Signal Bool
ltOut = opOut (<)
(<|) = ltOut

ltSqueeze, (||<) :: Ord a => Signal a -> Signal a -> Signal Bool
ltSqueeze = opSqueeze (<)
(||<) = ltSqueeze

ltSqueezeOut, (<||) :: Ord a => Signal a -> Signal a -> Signal Bool
ltSqueezeOut = opSqueezeOut (<)
(<||) = ltSqueezeOut

ltTrig, (!<) :: Ord a => Signal a -> Signal a -> Signal Bool
ltTrig = opTrig (<)
(!<) = ltTrig

ltTrigZero, (!!<) :: Ord a => Signal a -> Signal a -> Signal Bool
ltTrigZero = opTrigZero (<)
(!!<) = ltTrig

infix 4 |<|, |<, <|, ||<, <||, !<, !!<

-- lte

lteMix, (|<=|) :: Ord a => Signal a -> Signal a -> Signal Bool
lteMix = opMix (<=)
(|<=|) = lteMix

lteIn, (|<=) :: Ord a => Signal a -> Signal a -> Signal Bool
lteIn = opIn (<=)
(|<=) = lteIn

lteOut, (<=|) :: Ord a => Signal a -> Signal a -> Signal Bool
lteOut = opOut (<=)
(<=|) = lteOut

lteSqueeze, (||<=) :: Ord a => Signal a -> Signal a -> Signal Bool
lteSqueeze = opSqueeze (<=)
(||<=) = lteSqueeze

lteSqueezeOut, (<=||) :: Ord a => Signal a -> Signal a -> Signal Bool
lteSqueezeOut = opSqueezeOut (<=)
(<=||) = lteSqueezeOut

lteTrig, (!<=) :: Ord a => Signal a -> Signal a -> Signal Bool
lteTrig = opTrig (<=)
(!<=) = lteTrig

lteTrigZero, (!!<=) :: Ord a => Signal a -> Signal a -> Signal Bool
lteTrigZero = opTrigZero (<=)
(!!<=) = lteTrig

infix 4 |<=|, |<=, <=|, ||<=, <=||, !<=, !!<=

-- gt

gtMix, (|>|) :: Ord a => Signal a -> Signal a -> Signal Bool
gtMix = opMix (>)
(|>|) = gtMix

gtIn, (|>) :: Ord a => Signal a -> Signal a -> Signal Bool
gtIn = opIn (>)
(|>) = gtIn

gtOut, (>|) :: Ord a => Signal a -> Signal a -> Signal Bool
gtOut = opOut (>)
(>|) = gtOut

gtSqueeze, (||>) :: Ord a => Signal a -> Signal a -> Signal Bool
gtSqueeze = opSqueeze (>)
(||>) = gtSqueeze

gtSqueezeOut, (>||) :: Ord a => Signal a -> Signal a -> Signal Bool
gtSqueezeOut = opSqueezeOut (>)
(>||) = gtSqueezeOut

gtTrig, (!>) :: Ord a => Signal a -> Signal a -> Signal Bool
gtTrig = opTrig (>)
(!>) = gtTrig

gtTrigZero, (!!>) :: Ord a => Signal a -> Signal a -> Signal Bool
gtTrigZero = opTrigZero (>)
(!!>) = gtTrig

infix 4 |>|, |>, >|, ||>, >||, !>, !!>

-- gte

gteMix, (|>=|) :: Ord a => Signal a -> Signal a -> Signal Bool
gteMix = opMix (>=)
(|>=|) = gteMix

gteIn, (|>=) :: Ord a => Signal a -> Signal a -> Signal Bool
gteIn = opIn (>=)
(|>=) = gteIn

gteOut, (>=|) :: Ord a => Signal a -> Signal a -> Signal Bool
gteOut = opOut (>=)
(>=|) = gteOut

gteSqueeze, (||>=) :: Ord a => Signal a -> Signal a -> Signal Bool
gteSqueeze = opSqueeze (>=)
(||>=) = gteSqueeze

gteSqueezeOut, (>=||) :: Ord a => Signal a -> Signal a -> Signal Bool
gteSqueezeOut = opSqueezeOut (>=)
(>=||) = gteSqueezeOut

gteTrig, (!>=) :: Ord a => Signal a -> Signal a -> Signal Bool
gteTrig = opTrig (>=)
(!>=) = gteTrig

gteTrigZero, (!!>=) :: Ord a => Signal a -> Signal a -> Signal Bool
gteTrigZero = opTrigZero (>=)
(!!>=) = gteTrig

infix 4 |>=|, |>=, >=|, ||>=, >=||, !>=, !!>=

-- eq

eqMix, (|==|) :: Eq a => Signal a -> Signal a -> Signal Bool
eqMix = opMix (==)
(|==|) = eqMix

eqIn, (|==) :: Eq a => Signal a -> Signal a -> Signal Bool
eqIn = opIn (==)
(|==) = eqIn

eqOut, (==|) :: Eq a => Signal a -> Signal a -> Signal Bool
eqOut = opOut (==)
(==|) = eqOut

eqSqueeze, (||==) :: Eq a => Signal a -> Signal a -> Signal Bool
eqSqueeze = opSqueeze (==)
(||==) = eqSqueeze

eqSqueezeOut, (==||) :: Eq a => Signal a -> Signal a -> Signal Bool
eqSqueezeOut = opSqueezeOut (==)
(==||) = eqSqueezeOut

eqTrig, (!==) :: Eq a => Signal a -> Signal a -> Signal Bool
eqTrig = opTrig (==)
(!==) = eqTrig

eqTrigZero, (!!==) :: Eq a => Signal a -> Signal a -> Signal Bool
eqTrigZero = opTrigZero (==)
(!!==) = eqTrig

infix 4 |==|, |==, ==|, ||==, ==||, !==, !!==

-- ne
-- TODO - define != aliases too
  
neMix, (|/=|) :: Eq a => Signal a -> Signal a -> Signal Bool
neMix = opMix (/=)
(|/=|) = neMix

neIn, (|/=) :: Eq a => Signal a -> Signal a -> Signal Bool
neIn = opIn (/=)
(|/=) = neIn

neOut, (/=|) :: Eq a => Signal a -> Signal a -> Signal Bool
neOut = opOut (/=)
(/=|) = neOut

neSqueeze, (||/=) :: Eq a => Signal a -> Signal a -> Signal Bool
neSqueeze = opSqueeze (/=)
(||/=) = neSqueeze

neSqueezeOut, (/=||) :: Eq a => Signal a -> Signal a -> Signal Bool
neSqueezeOut = opSqueezeOut (/=)
(/=||) = neSqueezeOut

neTrig, (!/=) :: Eq a => Signal a -> Signal a -> Signal Bool
neTrig = opTrig (/=)
(!/=) = neTrig

neTrigZero, (!!/=) :: Eq a => Signal a -> Signal a -> Signal Bool
neTrigZero = opTrigZero (/=)
(!!/=) = neTrig

infix 4 |/=|, |/=, /=|, ||/=, /=||, !/=, !!/=

-- and (non-bitwise)
  
andMix, (|&&|) :: Signal Bool -> Signal Bool -> Signal Bool
andMix = opMix (&&)
(|&&|) = andMix

andIn, (|&&) :: Signal Bool -> Signal Bool -> Signal Bool
andIn = opIn (&&)
(|&&) = andIn

andOut, (&&|) :: Signal Bool -> Signal Bool -> Signal Bool
andOut = opOut (&&)
(&&|) = andOut

andSqueeze, (||&&) :: Signal Bool -> Signal Bool -> Signal Bool
andSqueeze = opSqueeze (&&)
(||&&) = andSqueeze

andSqueezeOut, (&&||) :: Signal Bool -> Signal Bool -> Signal Bool
andSqueezeOut = opSqueezeOut (&&)
(&&||) = andSqueezeOut

andTrig, (!&&) :: Signal Bool -> Signal Bool -> Signal Bool
andTrig = opTrig (&&)
(!&&) = andTrig

andTrigZero, (!!&&) :: Signal Bool -> Signal Bool -> Signal Bool
andTrigZero = opTrigZero (&&)
(!!&&) = andTrig

infix 4 |&&|, |&&, &&|, ||&&, &&||, !&&, !!&&

-- or (non-bitwise)
-- TODO oh dear..

orMix, (|.||.|) :: Signal Bool -> Signal Bool -> Signal Bool
orMix = opMix (||)
(|.||.|) = orMix

orIn, (|.||.) :: Signal Bool -> Signal Bool -> Signal Bool
orIn = opIn (||)
(|.||.) = orIn

orOut, (.||.|) :: Signal Bool -> Signal Bool -> Signal Bool
orOut = opOut (||)
(.||.|) = orOut

orSqueeze, (||.||.) :: Signal Bool -> Signal Bool -> Signal Bool
orSqueeze = opSqueeze (||)
(||.||.) = orSqueeze

orSqueezeOut, (.||.||) :: Signal Bool -> Signal Bool -> Signal Bool
orSqueezeOut = opSqueezeOut (||)
(.||.||) = orSqueezeOut

orTrig, (!.||.) :: Signal Bool -> Signal Bool -> Signal Bool
orTrig = opTrig (||)
(!.||.) = orTrig

orTrigZero, (!!.||.) :: Signal Bool -> Signal Bool -> Signal Bool
orTrigZero = opTrigZero (||)
(!!.||.) = orTrig

infix 4 |.||.|, |.||., .||.|, ||.||., .||.||, !.||., !!.||.

-- ************************************************************ --

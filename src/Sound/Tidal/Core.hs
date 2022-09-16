{-# LANGUAGE FlexibleInstances, BangPatterns #-}

{-
    Core.hs - For functions judged to be 'core' to tidal functionality.
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

module Sound.Tidal.Core where

import           Prelude hiding ((<*), (*>))

import           Data.Fixed (mod')
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Sound.Tidal.Signal
import           Sound.Tidal.Compose



-- ** Constructing patterns

-- ** Manipulating time

-- | Slow down a pattern by the factors in the given time pattern, 'squeezing'
-- the pattern to fit the slot given in the time pattern
fastSqueeze :: Pattern Time -> Pattern a -> Pattern a
fastSqueeze = tParamSqueeze _fast

-- | Slow down a pattern by the factors in the given time pattern, 'squeezing'
-- the pattern to fit the slot given in the time pattern
slowSqueeze :: Pattern Time -> Pattern a -> Pattern a
slowSqueeze = tParamSqueeze _slow

{- | Plays a portion of a pattern, specified by a time arc (start and end time).
The new resulting pattern is played over the time period of the original pattern:

@
d1 $ zoom (0.25, 0.75) $ sound "bd*2 hh*3 [sn bd]*2 drum"
@

In the pattern above, `zoom` is used with an arc from 25% to 75%. It is equivalent to this pattern:

@
d1 $ sound "hh*3 [sn bd]*2"
@
-}
zoom :: (Time, Time) -> Pattern a -> Pattern a
zoom (s,e) = zoomArc (Arc s e)

zoomArc :: Arc -> Pattern a -> Pattern a
zoomArc (Arc s e) p = splitQueries $
  withResultArc (mapCycle ((/d) . subtract s)) $ withQueryArc (mapCycle ((+s) . (*d))) p
     where d = e-s

compress :: (Time,Time) -> Pattern a -> Pattern a
compress (s,e) = compressArc (Arc s e)

compressTo :: (Time,Time) -> Pattern a -> Pattern a
compressTo (s,e) = compressArcTo (Arc s e)

repeatCycles :: Pattern Int -> Pattern a -> Pattern a
repeatCycles = tParam _repeatCycles

_repeatCycles :: Int -> Pattern a -> Pattern a
_repeatCycles n p = cat (replicate n p)

fastRepeatCycles :: Int -> Pattern a -> Pattern a
fastRepeatCycles n p = cat (replicate n p)

-- | * Higher order functions

-- | Functions which work on other functions (higher order functions)

-- | @every n f p@ applies the function @f@ to @p@, but only affects
-- every @n@ cycles.
every :: Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
every tp f p = innerJoin $ (\t -> _every t f p) <$> tp

_every :: Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
_every 0 _ p = p
_every n f p = when ((== 0) . (`mod` n)) f p

-- | @every n o f'@ is like @every n f@ with an offset of @o@ cycles
every' :: Pattern Int -> Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
every' np op f p = do { n <- np; o <- op; _every' n o f p }

_every' :: Int -> Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
_every' n o = when ((== o) . (`mod` n))

-- | @foldEvery ns f p@ applies the function @f@ to @p@, and is applied for
-- each cycle in @ns@.
foldEvery :: [Int] -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
foldEvery ns f p = foldr (`_every` f) p ns

{-|
Only `when` the given test function returns `True` the given pattern
transformation is applied. The test function will be called with the
current cycle as a number.

@
d1 $ when ((elem '4').show)
  (striate 4)
  $ sound "hh hc"
@

The above will only apply `striate 4` to the pattern if the current
cycle number contains the number 4. So the fourth cycle will be
striated and the fourteenth and so on. Expect lots of striates after
cycle number 399.
-}
when :: (Int -> Bool) -> (Pattern a -> Pattern a) ->  Pattern a -> Pattern a
when test f p = splitQueries $ p {query = apply}
  where apply st | test (floor $ start $ arc st) = query (f p) st
                 | otherwise = query p st

-- | Like 'when', but works on continuous time values rather than cycle numbers.
whenT :: (Time -> Bool) -> (Pattern a -> Pattern a) ->  Pattern a -> Pattern a
whenT test f p = splitQueries $ p {query = apply}
  where apply st | test (start $ arc st) = query (f p) st
                 | otherwise = query p st

_getP_ :: (Value -> Maybe a) -> Pattern Value -> Pattern a
_getP_ f pat = filterJust $ f <$> pat

_getP :: a -> (Value -> Maybe a) -> Pattern Value -> Pattern a
_getP d f pat = fromMaybe d . f <$> pat

_cX :: a -> (Value -> Maybe a) -> String -> Pattern a
_cX d f s = Pattern $ \(State a m) -> queryArc (maybe (pure d) (_getP d f . valueToPattern) $ Map.lookup s m) a

_cX_ :: (Value -> Maybe a) -> String -> Pattern a
_cX_ f s = Pattern $ \(State a m) -> queryArc (maybe silence (_getP_ f . valueToPattern) $ Map.lookup s m) a

cF :: Double -> String -> Pattern Double
cF d = _cX d getF
cF_ :: String -> Pattern Double
cF_ = _cX_ getF
cF0 :: String -> Pattern Double
cF0 = _cX 0 getF

cN :: Note -> String -> Pattern Note
cN d = _cX d getN
cN_ :: String -> Pattern Note
cN_ = _cX_ getN
cN0 :: String -> Pattern Note
cN0 = _cX (Note 0) getN

cI :: Int -> String -> Pattern Int
cI d = _cX d getI
cI_ :: String -> Pattern Int
cI_ = _cX_ getI
cI0 :: String -> Pattern Int
cI0 = _cX 0 getI

cB :: Bool -> String -> Pattern Bool
cB d = _cX d getB
cB_ :: String -> Pattern Bool
cB_ = _cX_ getB
cB0 :: String -> Pattern Bool
cB0 = _cX False getB

cR :: Rational -> String -> Pattern Rational
cR d = _cX d getR
cR_ :: String -> Pattern Rational
cR_ = _cX_ getR
cR0 :: String -> Pattern Rational
cR0 = _cX 0 getR

cT :: Time -> String -> Pattern Time
cT = cR
cT0 :: String -> Pattern Time
cT0 = cR0
cT_ :: String -> Pattern Time
cT_ = cR_

cS :: String -> String -> Pattern String
cS d = _cX d getS
cS_ :: String -> Pattern String
cS_ = _cX_ getS
cS0 :: String -> Pattern String
cS0 = _cX "" getS

module Sound.Tidal.Signal.Input where

import qualified Data.Map.Strict    as Map
import           Data.Maybe         (fromMaybe)

import           Sound.Tidal.Signal
import           Sound.Tidal.Types
import           Sound.Tidal.Value

-- ************************************************************ --
-- Functions for getting control input/state as signals

valueToSignal :: Value -> Signal Value
valueToSignal (VSignal pat) = pat
valueToSignal v             = pure v

_getP_ :: (Value -> Maybe a) -> Signal Value -> Signal a
_getP_ f pat = filterJusts $ f <$> pat

_getP :: a -> (Value -> Maybe a) -> Signal Value -> Signal a
_getP d f pat = fromMaybe d . f <$> pat

_cX :: a -> (Value -> Maybe a) -> String -> Signal a
_cX d f s = Signal mempty $ \(State a m) -> querySpan (maybe (pure d) (_getP d f . valueToSignal) $ Map.lookup s m) a

_cX_ :: (Value -> Maybe a) -> String -> Signal a
_cX_ f s = Signal mempty $ \(State a m) -> querySpan (maybe silence (_getP_ f . valueToSignal) $ Map.lookup s m) a

cF :: Double -> String -> Signal Double
cF d = _cX d getF
cF_ :: String -> Signal Double
cF_ = _cX_ getF
cF0 :: String -> Signal Double
cF0 = _cX 0 getF

cN :: Note -> String -> Signal Note
cN d = _cX d getN
cN_ :: String -> Signal Note
cN_ = _cX_ getN
cN0 :: String -> Signal Note
cN0 = _cX (Note 0) getN

cI :: Int -> String -> Signal Int
cI d = _cX d getI
cI_ :: String -> Signal Int
cI_ = _cX_ getI
cI0 :: String -> Signal Int
cI0 = _cX 0 getI

cB :: Bool -> String -> Signal Bool
cB d = _cX d getB
cB_ :: String -> Signal Bool
cB_ = _cX_ getB
cB0 :: String -> Signal Bool
cB0 = _cX False getB

cR :: Rational -> String -> Signal Rational
cR d = _cX d getR
cR_ :: String -> Signal Rational
cR_ = _cX_ getR
cR0 :: String -> Signal Rational
cR0 = _cX 0 getR

cT :: Time -> String -> Signal Time
cT = cR
cT0 :: String -> Signal Time
cT0 = cR0
cT_ :: String -> Signal Time
cT_ = cR_

cS :: String -> String -> Signal String
cS d = _cX d getS
cS_ :: String -> Signal String
cS_ = _cX_ getS
cS0 :: String -> Signal String
cS0 = _cX "" getS

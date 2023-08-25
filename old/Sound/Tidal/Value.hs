-- (c) Alex McLean 2022
-- Shared under the terms of the GNU Public License v. 3.0

module Sound.Tidal.Value where 

import Data.Word (Word8)
import Sound.Tidal.Types

-- | Apply one of three functions to a Value, depending on its type
applyFIRS :: (Double -> Double) -> (Int -> Int) -> (Rational -> Rational) -> (String -> String) -> Value -> Value
applyFIRS f _ _ _ (VF f') = VF (f f')
applyFIRS f _ _ _ (VN (Note f')) = VN (Note $ f f')
applyFIRS _ f _ _ (VI i) = VI (f i)
applyFIRS _ _ f _ (VR i) = VR (f i)
applyFIRS _ _ _ f (VS s) = VS (f s)
-- applyFIS f f' f'' (VState x) = VState $ \cmap -> (applyFIS f f' f'') <$> (x cmap)
applyFIRS _ _ _ _ v = v

-- | Apply one of two functions to a pair of Values, depending on
-- their types (int or float; strings and rationals are ignored)
fNum2 :: (Int -> Int -> Int) -> (Double -> Double -> Double) -> Value -> Value -> Value
fNum2 fInt _      (VI a) (VI b)               = VI (fInt a b)
fNum2 _    fFloat (VF a) (VF b)               = VF (fFloat a b)
fNum2 _    fFloat (VN (Note a)) (VN (Note b)) = VN (Note $ fFloat a b)
fNum2 _    fFloat (VF a) (VN (Note b))        = VN (Note $ fFloat a b)
fNum2 _    fFloat (VN (Note a)) (VF b)        = VN (Note $ fFloat a b)
fNum2 _    fFloat (VI a) (VF b)               = VF (fFloat (fromIntegral a) b)
fNum2 _    fFloat (VF a) (VI b)               = VF (fFloat a (fromIntegral b))
-- fNum2 fInt fFloat (VState a) b = VState $ \cmap -> ((\a' -> fNum2 fInt fFloat a' b) <$> (a cmap))
-- fNum2 fInt fFloat a (VState b) = VState $ \cmap -> ((\b' -> fNum2 fInt fFloat a b') <$> (b cmap))
fNum2 _    _      x      _      = x




getI :: Value -> Maybe Int
getI (VI i) = Just i
getI (VR x) = Just $ floor x
getI (VF x) = Just $ floor x
getI _  = Nothing

getF :: Value -> Maybe Double
getF (VF f) = Just f
getF (VR x) = Just $ fromRational x
getF (VI x) = Just $ fromIntegral x
getF _  = Nothing

getN :: Value -> Maybe Note
getN (VF f) = Just $ Note f
getN (VR x) = Just $ Note $ fromRational x
getN (VI x) = Just $ Note $ fromIntegral x
getN _  = Nothing

getS :: Value -> Maybe String
getS (VS s) = Just s
getS _  = Nothing

getB :: Value -> Maybe Bool
getB (VB b) = Just b
getB _  = Nothing

getR :: Value -> Maybe Rational
getR (VR r) = Just r
getR (VF x) = Just $ toRational x
getR (VI x) = Just $ toRational x
getR _  = Nothing

getBlob :: Value -> Maybe [Word8]
getBlob (VX xs) = Just xs
getBlob _  = Nothing

getList :: Value -> Maybe [Value]
getList (VList vs) = Just vs
getList _  = Nothing

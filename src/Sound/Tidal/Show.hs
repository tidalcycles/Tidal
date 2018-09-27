{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}
module Sound.Tidal.Show where

import Data.Ratio
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import Sound.Tidal.Pattern

instance Show Arc where
  show (s,e) = prettyRat s ++ ">" ++ prettyRat e

instance Show Part where
  show (a@(s,e),(s',e')) = h ++ "(" ++ show (s',e') ++ ")" ++ t
    where h | s == s' = ""
            | otherwise = prettyRat s ++ "-"
          t | e == e' = ""
            | otherwise = "-" ++ prettyRat e

instance Show a => Show (Event a) where
  show (p,v) = show p ++ "|" ++ show v

showPattern :: Show a => Arc -> Pattern a -> String
showPattern a p = intercalate "\n" $ map show $ queryArc p a

instance (Show a) => Show (Pattern a) where
  show p = showPattern (0,1) p

prettyRat :: Rational -> String
prettyRat r | unit == 0 && frac > 0 = showFrac (numerator frac) (denominator frac)
            | otherwise =  show unit ++ showFrac (numerator frac) (denominator frac)
  where unit = floor r
        frac = (r - (toRational unit))

showFrac :: Integer -> Integer -> String
showFrac 0 _ = ""
showFrac 1 2 = "½"
showFrac 1 3 = "⅓"
showFrac 2 3 = "⅔"
showFrac 1 4 = "¼"
showFrac 3 4 = "¾"
showFrac 1 5 = "⅕"
showFrac 2 5 = "⅖"
showFrac 3 5 = "⅗"
showFrac 4 5 = "⅘"
showFrac 1 6 = "⅙"
showFrac 5 6 = "⅚"
showFrac 1 7 = "⅐"
showFrac 1 8 = "⅛"
showFrac 3 8 = "⅜"
showFrac 5 8 = "⅝"
showFrac 7 8 = "⅞"
showFrac 1 9 = "⅑"
showFrac 1 10 = "⅒"

showFrac n d = fromMaybe plain $ do n' <- up n
                                    d' <- down d
                                    return $ n' ++ d'
  where plain = " " ++ show n ++ "/" ++ show d
        up 1 = Just "¹"
        up 2 = Just "²"
        up 3 = Just "³"
        up 4 = Just "⁴"
        up 5 = Just "⁵"
        up 6 = Just "⁶"
        up 7 = Just "⁷"
        up 8 = Just "⁸"
        up 9 = Just "⁹"
        up 0 = Just "⁰"
        up x = Nothing
        down 1 = Just "₁"
        down 2 = Just "₂"
        down 3 = Just "₃"
        down 4 = Just "₄"
        down 5 = Just "₅"
        down 6 = Just "₆"
        down 7 = Just "₇"
        down 8 = Just "₈"
        down 9 = Just "₉"
        down 0 = Just "₀"
        down x = Nothing

{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sound.Tidal.Show where

{-
    Show.hs - Library for visualising Tidal patterns as text
    Copyright (C) 2023, Alex McLean and contributors

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

import           Sound.Tidal.Types

import           Sound.Tidal.Event  (eventHasOnset)
import           Sound.Tidal.Signal (querySpan)

import           Data.List          (intercalate, sortOn)
import           Data.Maybe         (fromMaybe)
import           Data.Ratio         (denominator, numerator)

import qualified Data.Map.Strict    as Map


instance Show Note where
  show n = (show . unNote $ n) ++ "n (" ++ pitchClass ++ octave ++ ")"
    where
      pitchClass = pcs !! mod noteInt 12
      octave = show $ div noteInt 12 + 5
      noteInt = round . unNote $ n
      pcs = ["c", "cs", "d", "ds", "e", "f", "fs", "g", "gs", "a", "as", "b"]

prettyRatio :: Rational -> String
prettyRatio r | denominator r == 1 = show $ numerator r
              | otherwise = show (numerator r) ++ "/" ++ show (denominator r)

instance (Show a) => Show (Sequence a) where
  show (Atom _ d _ _ Nothing) = "~" ++ "×" ++ prettyRatio d
  show (Atom _ d i o (Just v)) = show v ++ "×" ++ prettyRatio d ++ showio
    where showio | i == 0 && o == 0 = ""
                 | otherwise = "(" ++ prettyRatio i ++ "," ++ prettyRatio o ++ ")"
  show (Cat xs) = "[" ++ unwords (map show xs) ++ "]"
  show (Stack xs) = "[\n" ++ intercalate ", \n" (map show xs) ++ "\n]"

instance (Show a) => Show (Signal a) where
  show = showSignal (Span 0 1)

-- showStateful :: ControlSignal -> String
-- showStateful p = intercalate "\n" evStrings
--   where (_, evs) = resolveState (Map.empty) $ sortOn part $ querySpan (filterOnsets p) (Span 0 1)
--         evs' = map showEvent evs
--         maxPartLength :: Int
--         maxPartLength = maximum $ map (length . fst) evs'
--         evString :: (String, String) -> String
--         evString ev = ((replicate (maxPartLength - (length (fst ev))) ' ')
--                        ++ fst ev
--                        ++ snd ev
--                       )
--         evStrings = map evString evs'

showSignal :: Show a => Span -> Signal a -> String
showSignal a p = intercalate "\n" evStrings
  where evs = map showEvent $ sortOn active $ querySpan p a
        maxActiveLength :: Int
        maxActiveLength = maximum $ map (length . fst) evs
        evString :: (String, String) -> String
        evString ev = replicate (maxActiveLength - length (fst ev)) ' '
                       ++ uncurry (++) ev
        evStrings = map evString evs

showEvent :: Show a => Event a -> (String, String)
showEvent (Event _ (Just (Span ws we)) a@(Span ps pe) e) =
  (h ++ "(" ++ show a ++ ")" ++ t ++ "|", show e)
  where h | ws == ps = ""
          | otherwise = prettyRat ws ++ "-"
        t | we == pe = ""
          | otherwise = "-" ++ prettyRat we

showEvent (Event _ Nothing a e) =
  ("~" ++ show a ++ "~|", show e)

-- Show everything, including event metadata
showAll :: Show a => Span -> Signal a -> String
showAll a p = intercalate "\n" $ map showEventAll $ sortOn active $ querySpan p a

-- Show metadata of an event
showEventAll :: Show a => Event a -> String
showEventAll e = show (eventMetadata e) ++ uncurry (++) (showEvent e)

instance Show Val where
  show (VS s)      = ('"':s) ++ "\""
  show (VI i)      = show i
  show (VF f)      = show f ++ "f"
  show (VN n)      = show n
  show (VR r)      = prettyRat r ++ "r"
  show (VB b)      = show b
  show (VX xs)     = show xs
--  show (VPattern pat) = "(" ++ show pat ++ ")"
  show (VState f)  = show $ f Map.empty
  show (VList vs)  = show $ map show vs
  show (VSignal s) = show s

instance {-# OVERLAPPING #-} Show ValMap where
  show m = intercalate ", " $ map (\(name, v) -> name ++ ": " ++ show v) $ Map.toList m

instance {-# OVERLAPPING #-} Show Span where
  show (Span s e) = prettyRat s ++ ">" ++ prettyRat e

instance {-# OVERLAPPING #-} Show a => Show (Event a) where
  show e = uncurry (++) (showEvent e)

prettyRat :: Rational -> String
prettyRat r | unit == 0 && frac > 0 = showFrac (numerator frac) (denominator frac)
            | otherwise =  show unit ++ showFrac (numerator frac) (denominator frac)
  where unit = floor r :: Int
        frac = r - toRational unit

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
  where plain = show n ++ "/" ++ show d
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
        up _ = Nothing
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
        down _ = Nothing

stepcount :: Signal a -> Int
stepcount pat = fromIntegral $ eventSteps $ concatMap ((\ev -> [aBegin ev, aEnd ev]) . active) (filter eventHasOnset $ querySpan pat (Span 0 1))
  where eventSteps xs = foldr (lcm . denominator) 1 xs

{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sound.Tidal.Show where


{-
    Show.hs - Library for visualising Tidal patterns as text
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

import           Sound.Tidal.Types

import           Sound.Tidal.Signal.Base  (queryArc)
import           Sound.Tidal.Signal.Event (eventHasOnset)

import           Data.List                (intercalate, sortOn)
import           Data.Maybe               (fromMaybe)
import           Data.Ratio               (denominator, numerator)

import qualified Data.Map.Strict          as Map


prettyRatio :: Rational -> String
prettyRatio r | denominator r == 1 = show $ numerator r
              | otherwise = show (numerator r) ++ "/" ++ show (denominator r)

instance (Show a) => Show (Sequence a) where
  show (Atom _ d _ _ Nothing) = "~" ++ "×" ++ prettyRatio d
  show (Atom _ d i o (Just v)) = show v ++ "×" ++ prettyRatio d ++ showio
    where showio | i == 0 && o == 0 = ""
                 | otherwise = "(" ++ prettyRatio i ++ "," ++ prettyRatio o ++ ")"
  show (Cat xs) = "[" ++ unwords (map show xs) ++ "]"
  show (Stack xs) = "[" ++ intercalate ", " (map show xs) ++ "]"

instance (Show a) => Show (Signal a) where
  show = showSignal (Arc 0 1)

-- showStateful :: ControlSignal -> String
-- showStateful p = intercalate "\n" evStrings
--   where (_, evs) = resolveState (Map.empty) $ sortOn part $ queryArc (filterOnsets p) (Arc 0 1)
--         evs' = map showEvent evs
--         maxPartLength :: Int
--         maxPartLength = maximum $ map (length . fst) evs'
--         evString :: (String, String) -> String
--         evString ev = ((replicate (maxPartLength - (length (fst ev))) ' ')
--                        ++ fst ev
--                        ++ snd ev
--                       )
--         evStrings = map evString evs'

showSignal :: Show a => Arc -> Signal a -> String
showSignal a p = intercalate "\n" evStrings
  where evs = map showEvent $ sortOn active $ queryArc p a
        maxActiveLength :: Int
        maxActiveLength = maximum $ map (length . fst) evs
        evString :: (String, String) -> String
        evString ev = replicate (maxActiveLength - length (fst ev)) ' '
                       ++ uncurry (++) ev
        evStrings = map evString evs

showEvent :: Show a => Event a -> (String, String)
showEvent (Event _ (Just (Arc ws we)) a@(Arc ps pe) e) =
  (h ++ "(" ++ show a ++ ")" ++ t ++ "|", show e)
  where h | ws == ps = ""
          | otherwise = prettyRat ws ++ "-"
        t | we == pe = ""
          | otherwise = "-" ++ prettyRat we

showEvent (Event _ Nothing a e) =
  ("~" ++ show a ++ "~|", show e)

-- Show everything, including event metadata
showAll :: Show a => Arc -> Signal a -> String
showAll a p = intercalate "\n" $ map showEventAll $ sortOn active $ queryArc p a

-- Show metadata of an event
showEventAll :: Show a => Event a -> String
showEventAll e = show (metadata e) ++ uncurry (++) (showEvent e)

instance Show Metadata where
  show (Metadata cs) = show cs

instance Show Value where
  show (VS s)     = ('"':s) ++ "\""
  show (VI i)     = show i
  show (VF f)     = show f ++ "f"
  show (VN n)     = show n
  show (VR r)     = prettyRat r ++ "r"
  show (VB b)     = show b
  show (VX xs)    = show xs
--  show (VPattern pat) = "(" ++ show pat ++ ")"
  show (VState f) = show $ f Map.empty
  show (VList vs) = show $ map show vs

instance {-# OVERLAPPING #-} Show ValueMap where
  show m = intercalate ", " $ map (\(name, v) -> name ++ ": " ++ show v) $ Map.toList m

instance {-# OVERLAPPING #-} Show Arc where
  show (Arc s e) = prettyRat s ++ ">" ++ prettyRat e

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
stepcount pat = fromIntegral $ eventSteps $ concatMap ((\ev -> [aBegin ev, aEnd ev]) . active) (filter eventHasOnset $ queryArc pat (Arc 0 1))
  where eventSteps xs = foldr (lcm . denominator) 1 xs

data Render = Render Int Int String

instance Show Render where
  show (Render cyc i render) | i <= 1024 = "\n[" ++ show cyc ++ (if cyc == 1 then " cycle" else " cycles") ++ "]\n" ++ render
                             | otherwise = "That pattern is too complex to draw."


-- deconstruct :: Int -> Pattern String -> String
-- deconstruct n p = intercalate " " $ map showStep $ toList p
--   where
--     showStep :: [String] -> String
--     showStep [] = "~"
--     showStep [x] = x
--     showStep xs = "[" ++ (intercalate ", " xs) ++ "]"
--     toList :: Pattern a -> [[a]]
--     toList pat = map (\(s,e) -> map value $ queryArc (_segment n' pat) (Arc s e)) arcs
--       where breaks = [0, (1/n') ..]
--             arcs = zip (take n breaks) (drop 1 breaks)
--             n' = fromIntegral n

-- drawLine :: Signal Char -> Render
-- drawLine = drawLineSz 78

-- drawLineSz :: Int -> Signal Char -> Render
-- drawLineSz sz pat = joinCycles sz $ drawCycles pat
--   where
--     drawCycles :: Signal Char -> [Render]
--     drawCycles pat' = draw pat':drawCycles (rotL 1 pat')
--     joinCycles :: Int -> [Render] -> Render
--     joinCycles _ [] = Render 0 0 ""
--     joinCycles n ((Render cyc l s):cs) | l > n = Render 0 0 ""
--                                        | otherwise = Render (cyc+cyc') (l + l' + 1) $ intercalate "\n" $ map (uncurry (++)) lineZip
--       where
--         (Render cyc' l' s') = joinCycles (n-l-1) cs
--         linesN = max (length $ lines s) (length $ lines s')
--         lineZip = take linesN $
--           zip (lines s ++ repeat (replicate l ' '))
--               (lines s' ++ repeat (replicate l' ' '))

--       -- where maximum (map (length . head . (++ [""]) . lines) cs)


-- draw :: Signal Char -> Render
-- draw pat = Render 1 s (intercalate "\n" $ map (('|' :) .drawLevel) ls)
--   where ls = levels pat
--         s = stepcount pat
--         rs = toRational s
--         drawLevel :: [Event Char] -> String
--         drawLevel [] = replicate s '.'
--         drawLevel (e:es) = map f $ take s $ zip (drawLevel es ++ repeat '.') (drawEvent e ++ repeat '.')
--         f ('.', x) = x
--         f (x, _) = x
--         drawEvent :: Event Char -> String
--         drawEvent ev = replicate (floor $ rs * evStart) '.'
--                        ++ (value ev:replicate (floor (rs * (evStop - evStart)) - 1) '-')
--           where evStart = start $ wholeOrActive ev
--                 evStop = stop $ wholeOrActive ev

-- fits :: Event b -> [Event b] -> Bool
-- fits (Event _ _ part' _) events = not $ any (\Event{..} -> isJust $ subArc part' part) events

-- addEvent :: Event b -> [[Event b]] -> [[Event b]]
-- addEvent e [] = [[e]]
-- addEvent e (level:ls)
--     | fits e level = (e:level) : ls
--     | otherwise = level : addEvent e ls

-- arrangeEvents :: [Event b] -> [[Event b]]
-- arrangeEvents = foldr addEvent []

-- levels :: Eq a => Signal a -> [[Event a]]
-- -- levels pat = arrangeEvents $ sortOn' ((\Arc{..} -> stop - start) . part) (defragActives $ queryArc pat (Arc 0 1))
-- levels pat = arrangeEvents $ reverse $ defragActives $ queryArc pat (Arc 0 1)

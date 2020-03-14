{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, RecordWildCards #-}

module Sound.Tidal.Show (show, draw) where

import Sound.Tidal.Pattern

import Data.List (intercalate, sortOn)
import Data.Ratio (numerator, denominator)
import Data.Maybe (fromMaybe, isJust)

import qualified Data.Map.Strict as Map

instance (Show a) => Show (Pattern a) where
  show = showPattern (Arc 0 1)

showPattern :: Show a => Arc -> Pattern a -> String
showPattern a p = intercalate "\n" $ map show $ queryArc p a

instance Show Context where
  show (Context cs) = show cs

instance Show Value where
  show (VS s) = ('"':s) ++ "\""
  show (VI i) = show i
  show (VF f) = show f ++ "f"
  show (VR r) = show r ++ "r"
  show (VB b) = show b
  show (VX xs) = show xs

instance {-# OVERLAPPING #-} Show ControlMap where
  show m = intercalate ", " $ map (\(name, v) -> name ++ ": " ++ show v) $ Map.toList m

instance {-# OVERLAPPING #-} Show Arc where
  show (Arc s e) = prettyRat s ++ ">" ++ prettyRat e

instance {-# OVERLAPPING #-} Show a => Show (Event a) where
  show (Event c (Just (Arc ws we)) a@(Arc ps pe) e) =
    show c ++ h ++ "(" ++ show a ++ ")" ++ t ++ "|" ++ show e
    where h | ws == ps = ""
            | otherwise = prettyRat ws ++ "-"
          t | we == pe = ""
            | otherwise = "-" ++ prettyRat we
  show (Event c Nothing a e) =
    show c ++ "~" ++ show a ++ "~|" ++ show e

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

stepcount :: Pattern a -> Int
stepcount pat = fromIntegral $ eventSteps $ concatMap (\ev -> [start ev, stop ev]) $ map part $ filter eventHasOnset $ queryArc pat (Arc 0 1)
  where eventSteps xs = foldr lcm 1 $ map denominator xs

data Render = Render Int String

instance Show Render where
  show (Render i render) | i <= 1024 = show i ++ " steps per cycle \n" ++ render
                         | otherwise = show i ++ " steps per cycle, too many to render.."


draw :: Pattern Char -> Render
draw pat = Render s $ (intercalate "\n" $ map ((\x -> ('|':x) ++ "|") .drawLevel) ls)
  where ls = levelsWhole pat
        s = stepcount pat
        rs = toRational s
        drawLevel :: [Event Char] -> String
        drawLevel [] = replicate s ' '
        drawLevel (e:es) = map f $ take s $ zip (drawLevel es ++ repeat ' ') (drawEvent e ++ repeat ' ')
        f (' ', x) = x
        f (x, _) = x
        drawEvent :: Event Char -> String
        drawEvent ev = (replicate (floor $ rs * evStart) ' ')
                       ++ (value ev:(replicate ((floor $ rs * (evStop - evStart)) - 1) '-'))
          where evStart = start $ wholeOrPart ev
                evStop = stop $ wholeOrPart ev

fitsWhole :: Event b -> [Event b] -> Bool
fitsWhole event events =
  not $ any (\event' -> isJust $ subArc (wholeOrPart event) (wholeOrPart event')) events

addEventWhole :: Event b -> [[Event b]] -> [[Event b]]
addEventWhole e [] = [[e]]
addEventWhole e (level:ls)
    | isAnalog e = level:ls
    | fitsWhole e level = (e:level) : ls
    | otherwise = level : addEventWhole e ls

arrangeEventsWhole :: [Event b] -> [[Event b]]
arrangeEventsWhole = foldr addEventWhole []

sortOn' :: Ord a => (b -> a) -> [b] -> [b]
sortOn' f = map snd . sortOn fst . map (\x -> let y = f x in y `seq` (y, x))

fits :: Event b -> [Event b] -> Bool
fits (Event _ _ part' _) events = not $ any (\Event{..} -> isJust $ subArc part' part) events

levelsWhole :: Eq a => Pattern a -> [[Event a]]
levelsWhole pat = arrangeEventsWhole $ sortOn' ((\Arc{..} -> 0 - (stop - start)) . wholeOrPart) (defragParts $ queryArc pat (Arc 0 1))

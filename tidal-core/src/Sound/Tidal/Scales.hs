module Sound.Tidal.Scales (scale, scaleList, scaleTable, getScale, scaleWith, scaleWithList, raiseDegree, lowerDegree, raiseDegrees, lowerDegrees) where

{-
    Scale.hs - Scales for TidalCycles
    Copyright (C) 2020, lvm (Mauro) and contributors

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

import Data.Maybe (fromMaybe)
import Sound.Tidal.Core (slowcat)
import Sound.Tidal.Pattern (Pattern, (<*))
import Sound.Tidal.Utils ((!!!))
import Prelude hiding ((*>), (<*))

-- * Scale definitions

-- ** Five notes scales

minPent :: (Fractional a) => [a]
minPent = [0, 3, 5, 7, 10]

majPent :: (Fractional a) => [a]
majPent = [0, 2, 4, 7, 9]

-- | Another mode of major pentatonic
ritusen :: (Fractional a) => [a]
ritusen = [0, 2, 5, 7, 9]

-- | Another mode of major pentatonic
egyptian :: (Fractional a) => [a]
egyptian = [0, 2, 5, 7, 10]

-- *** Other scales

kumai :: (Fractional a) => [a]
kumai = [0, 2, 3, 7, 9]

hirajoshi :: (Fractional a) => [a]
hirajoshi = [0, 2, 3, 7, 8]

iwato :: (Fractional a) => [a]
iwato = [0, 1, 5, 6, 10]

chinese :: (Fractional a) => [a]
chinese = [0, 4, 6, 7, 11]

indian :: (Fractional a) => [a]
indian = [0, 4, 5, 7, 10]

pelog :: (Fractional a) => [a]
pelog = [0, 1, 3, 7, 8]

-- *** More scales

prometheus :: (Fractional a) => [a]
prometheus = [0, 2, 4, 6, 11]

scriabin :: (Fractional a) => [a]
scriabin = [0, 1, 4, 7, 9]

-- *** Han Chinese pentatonic scales

gong :: (Fractional a) => [a]
gong = [0, 2, 4, 7, 9]

shang :: (Fractional a) => [a]
shang = [0, 2, 5, 7, 10]

jiao :: (Fractional a) => [a]
jiao = [0, 3, 5, 8, 10]

zhi :: (Fractional a) => [a]
zhi = [0, 2, 5, 7, 9]

yu :: (Fractional a) => [a]
yu = [0, 3, 5, 7, 10]

-- ** 6 note scales

whole' :: (Fractional a) => [a]
whole' = [0, 2, 4, 6, 8, 10]

augmented :: (Fractional a) => [a]
augmented = [0, 3, 4, 7, 8, 11]

augmented2 :: (Fractional a) => [a]
augmented2 = [0, 1, 4, 5, 8, 9]

-- *** Hexatonic modes with no tritone

hexMajor7 :: (Fractional a) => [a]
hexMajor7 = [0, 2, 4, 7, 9, 11]

hexDorian :: (Fractional a) => [a]
hexDorian = [0, 2, 3, 5, 7, 10]

hexPhrygian :: (Fractional a) => [a]
hexPhrygian = [0, 1, 3, 5, 8, 10]

hexSus :: (Fractional a) => [a]
hexSus = [0, 2, 5, 7, 9, 10]

hexMajor6 :: (Fractional a) => [a]
hexMajor6 = [0, 2, 4, 5, 7, 9]

hexAeolian :: (Fractional a) => [a]
hexAeolian = [0, 3, 5, 7, 8, 10]

-- ** 7 note scales

major :: (Fractional a) => [a]
major = [0, 2, 4, 5, 7, 9, 11]

ionian :: (Fractional a) => [a]
ionian = [0, 2, 4, 5, 7, 9, 11]

dorian :: (Fractional a) => [a]
dorian = [0, 2, 3, 5, 7, 9, 10]

phrygian :: (Fractional a) => [a]
phrygian = [0, 1, 3, 5, 7, 8, 10]

lydian :: (Fractional a) => [a]
lydian = [0, 2, 4, 6, 7, 9, 11]

mixolydian :: (Fractional a) => [a]
mixolydian = [0, 2, 4, 5, 7, 9, 10]

aeolian :: (Fractional a) => [a]
aeolian = [0, 2, 3, 5, 7, 8, 10]

minor :: (Fractional a) => [a]
minor = [0, 2, 3, 5, 7, 8, 10]

locrian :: (Fractional a) => [a]
locrian = [0, 1, 3, 5, 6, 8, 10]

harmonicMinor :: (Fractional a) => [a]
harmonicMinor = [0, 2, 3, 5, 7, 8, 11]

harmonicMajor :: (Fractional a) => [a]
harmonicMajor = [0, 2, 4, 5, 7, 8, 11]

melodicMinor :: (Fractional a) => [a]
melodicMinor = [0, 2, 3, 5, 7, 9, 11]

melodicMinorDesc :: (Fractional a) => [a]
melodicMinorDesc = [0, 2, 3, 5, 7, 8, 10]

melodicMajor :: (Fractional a) => [a]
melodicMajor = [0, 2, 4, 5, 7, 8, 10]

bartok :: (Fractional a) => [a]
bartok = melodicMajor

hindu :: (Fractional a) => [a]
hindu = melodicMajor

-- *** Raga modes

todi :: (Fractional a) => [a]
todi = [0, 1, 3, 6, 7, 8, 11]

purvi :: (Fractional a) => [a]
purvi = [0, 1, 4, 6, 7, 8, 11]

marva :: (Fractional a) => [a]
marva = [0, 1, 4, 6, 7, 9, 11]

bhairav :: (Fractional a) => [a]
bhairav = [0, 1, 4, 5, 7, 8, 11]

ahirbhairav :: (Fractional a) => [a]
ahirbhairav = [0, 1, 4, 5, 7, 9, 10]

-- *** More modes

superLocrian :: (Fractional a) => [a]
superLocrian = [0, 1, 3, 4, 6, 8, 10]

romanianMinor :: (Fractional a) => [a]
romanianMinor = [0, 2, 3, 6, 7, 9, 10]

hungarianMinor :: (Fractional a) => [a]
hungarianMinor = [0, 2, 3, 6, 7, 8, 11]

neapolitanMinor :: (Fractional a) => [a]
neapolitanMinor = [0, 1, 3, 5, 7, 8, 11]

enigmatic :: (Fractional a) => [a]
enigmatic = [0, 1, 4, 6, 8, 10, 11]

spanish :: (Fractional a) => [a]
spanish = [0, 1, 4, 5, 7, 8, 10]

-- *** Modes of whole tones with added note ->

leadingWhole :: (Fractional a) => [a]
leadingWhole = [0, 2, 4, 6, 8, 10, 11]

lydianMinor :: (Fractional a) => [a]
lydianMinor = [0, 2, 4, 6, 7, 8, 10]

neapolitanMajor :: (Fractional a) => [a]
neapolitanMajor = [0, 1, 3, 5, 7, 9, 11]

locrianMajor :: (Fractional a) => [a]
locrianMajor = [0, 2, 4, 5, 6, 8, 10]

-- ** 8 note scales

diminished :: (Fractional a) => [a]
diminished = [0, 1, 3, 4, 6, 7, 9, 10]

diminished2 :: (Fractional a) => [a]
diminished2 = [0, 2, 3, 5, 6, 8, 9, 11]

-- ** Modes of limited transposition

messiaen1 :: (Fractional a) => [a]
messiaen1 = whole'

messiaen2 :: (Fractional a) => [a]
messiaen2 = diminished

messiaen3 :: (Fractional a) => [a]
messiaen3 = [0, 2, 3, 4, 6, 7, 8, 10, 11]

messiaen4 :: (Fractional a) => [a]
messiaen4 = [0, 1, 2, 5, 6, 7, 8, 11]

messiaen5 :: (Fractional a) => [a]
messiaen5 = [0, 1, 5, 6, 7, 11]

messiaen6 :: (Fractional a) => [a]
messiaen6 = [0, 2, 4, 5, 6, 8, 10, 11]

messiaen7 :: (Fractional a) => [a]
messiaen7 = [0, 1, 2, 3, 5, 6, 7, 8, 9, 11]

-- ** Arabic maqams taken from SuperCollider's Scale.sc

bayati :: (Fractional a) => [a]
bayati = [0, 1.5, 3, 5, 7, 8, 10]

hijaz :: (Fractional a) => [a]
hijaz = [0, 1, 4, 5, 7, 8.5, 10]

sikah :: (Fractional a) => [a]
sikah = [0, 1.5, 3.5, 5.5, 7, 8.5, 10.5]

rast :: (Fractional a) => [a]
rast = [0, 2, 3.5, 5, 7, 9, 10.5]

iraq :: (Fractional a) => [a]
iraq = [0, 1.5, 3.5, 5, 6.5, 8.5, 10.5]

saba :: (Fractional a) => [a]
saba = [0, 1.5, 3, 4, 6, 8, 10]

-- ** 12 note scales

chromatic :: (Fractional a) => [a]
chromatic = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]

-- |
--  Interprets a pattern of note numbers into a particular named scale. For example:
--
--  > d1
--  >   $ jux rev
--  >   $ chunk 4 (fast 2 . (|- n 12))
--  >   $ off 0.25 (|+ 7)
--  >   $ struct (iter 4 "t(5,8)")
--  >   $ n (scale "ritusen" "0 .. 7")
--  >   # sound "superpiano"
scale :: (Fractional a) => Pattern String -> Pattern Int -> Pattern a
scale = getScale scaleTable

-- |
--  Build a scale function, with additional scales if you wish. For example:
--
--  > let myscale =
--  >   getScale
--  >     ( scaleTable ++
--  >         [ ("techno", [0,2,3,5,7,8,10])
--  >         , ("broken", [0,1,4,7,8,10])
--  >         ]
--  >     )
--
--  The above takes the standard 'scaleTable' as a starting point and adds two custom scales to it. You’ll be able to use the new function in place of the normal one:
--
--  > d1 $ n (myscale "techno" "0 1 2 3 4 5 6 7") # sound "superpiano"
getScale :: (Fractional a) => [(String, [a])] -> Pattern String -> Pattern Int -> Pattern a
getScale table sp p =
  ( \n scaleName ->
      noteInScale (fromMaybe [0] $ lookup scaleName table) n
  )
    <$> p
      <* sp
  where
    octave s x = x `div` length s
    noteInScale s x = (s !!! x) + fromIntegral (12 * octave s x)

{- Variant of @scale@ allowing to modify the current scale (seen as a list) with an [a] -> [a] function.

These are equivalent:

> d1 $ up (scaleWith "major" (insert 1) $ run 8) # s "superpiano"
> d1 $ up "0 1 2 4 5 7 9 11" # s "superpiano"

-}
scaleWith :: (Eq a, Fractional a) => Pattern String -> ([a] -> [a]) -> Pattern Int -> Pattern a
scaleWith = getScaleMod scaleTable

{- Variant of @scaleWith@ providing a list of modifier functions instead of a single function
-}
scaleWithList :: (Eq a, Fractional a) => Pattern String -> ([[a] -> [a]]) -> Pattern Int -> Pattern a
scaleWithList _ [] _ = silence
scaleWithList sp (f : []) p = scaleMod sp f p
scaleWithList sp fs p = Pattern q
  where
    n = length fs
    q st = concatMap (ff st)
      $ arcCyclesZW (arc st)
    ff st a = query pp $ st {arc = a}
      where
        f = fs !! i
        cyc = (floor $ start a) :: Int
        i = cyc `mod` n
        pp = (scaleMod sp f p)

{- Variant of @getScale@ used to build the @scaleWith@ function
-}
getScaleMod :: (Eq a, Fractional a) => [(String, [a])] -> Pattern String -> ([a] -> [a]) -> Pattern Int -> Pattern a
getScaleMod table sp f p =
  ( \n scaleName ->
      noteInScale (uniq $ f $ fromMaybe [0] $ lookup scaleName table) n
  )
    <$> p
      <* sp
  where
    octave s x = x `div` length s
    noteInScale s x = (s !!! x) + fromIntegral (12 * octave s x)

{- Eliminates duplicates in a sorted list
-}
uniq :: (Eq a) => [a] -> [a]
uniq (h1 : h2 : tl) = if (h1 == h2) then h1 : (uniq tl) else h1 : (uniq (h2 : tl))
uniq l = l

{- Raises a specified degree of a scale, provided as a numbers list.
Meant to be passed as an argument to @scaleWith@
-}
raiseDegree :: (Fractional a) => Int -> [a] -> [a]
raiseDegree _ (hd : []) = (hd + 1) : []
raiseDegree 0 (hd : tl) = (hd + 1) : tl
raiseDegree n (hd : tl) = hd : (raiseDegree (n - 1) tl)
raiseDegree _ [] = error "Degree is not present in the scale"

{- Lowers a specified degree of a scale, provided as a numbers list.
Meant to be passed as an argument to @scaleWith@
-}
lowerDegree :: (Fractional a) => Int -> [a] -> [a]
lowerDegree _ (hd : []) = (hd - 1) : []
lowerDegree 0 (hd : tl) = (hd - 1) : tl
lowerDegree n (hd : tl) = hd : (lowerDegree (n - 1) tl)
lowerDegree _ [] = error "Degree is not present in the scale"

{- Like @raiseDegree@, but raises a range of degrees instead of a single one
-}
raiseDegrees :: (Fractional a) => Int -> Int -> [a] -> [a]
raiseDegrees _ _ (hd : []) = (hd + 1) : []
raiseDegrees 0 0 (hd : tl) = (hd + 1) : tl
raiseDegrees 0 m (hd : tl) = (hd + 1) : (raiseDegrees 0 (m - 1) tl)
raiseDegrees n m (hd : tl) = hd : (raiseDegrees (n - 1) (m - 1) tl)
raiseDegrees _ _ [] = error "Degrees are out of the scale"

{- Like @lowerDegree@, but lowers a range of degrees instead of a single one
-}
lowerDegrees :: (Fractional a) => Int -> Int -> [a] -> [a]
lowerDegrees _ _ (hd : []) = (hd - 1) : []
lowerDegrees 0 0 (hd : tl) = (hd - 1) : tl
lowerDegrees 0 m (hd : tl) = (hd - 1) : (lowerDegrees 0 (m - 1) tl)
lowerDegrees n m (hd : tl) = hd : (lowerDegrees (n - 1) (m - 1) tl)
lowerDegrees _ _ [] = error "Degrees are out of the scale"

-- |
--  Outputs this list of all the available scales:
--
-- @
-- minPent majPent ritusen egyptian kumai hirajoshi iwato chinese indian pelog
-- prometheus scriabin gong shang jiao zhi yu whole wholetone augmented augmented2
-- hexMajor7 hexDorian hexPhrygian hexSus hexMajor6 hexAeolian major ionian dorian
-- phrygian lydian mixolydian aeolian minor locrian harmonicMinor harmonicMajor
-- melodicMinor melodicMinorDesc melodicMajor bartok hindu todi purvi marva bhairav
-- ahirbhairav superLocrian romanianMinor hungarianMinor neapolitanMinor enigmatic
-- spanish leadingWhole lydianMinor neapolitanMajor locrianMajor diminished
-- octatonic diminished2 octatonic2 messiaen1 messiaen2 messiaen3 messiaen4
-- messiaen5 messiaen6 messiaen7 chromatic bayati hijaz sikah rast saba iraq
-- @
scaleList :: String
scaleList = unwords $ map fst (scaleTable :: [(String, [Rational])])

-- |
--  Outputs a list of all available scales and their corresponding notes. For
--  example, its first entry is @("minPent",[0,3,5,7,10]@) which means that
--  a minor pentatonic scale is formed by the root (0), the minor third (3 semitones
--  above the root), the perfect fourth (5 semitones above the root), etc.
--
--  As the list is big, you can use the Haskell function lookup to look up a
--  specific scale: @lookup "phrygian" scaleTable@. This will output
--  @Just [0.0,1.0,3.0,5.0,7.0,8.0,10.0]@.
--
--  You can also do a reverse lookup into the scale table. For example:
--
--  > filter ( \(_, x) -> take 3 x == [0,2,4] ) scaleTable
--
--  The above example will output all scales of which the first three notes are
--  the root, the major second (2 semitones above the fundamental), and the major
--  third (4 semitones above the root).
scaleTable :: (Fractional a) => [(String, [a])]
scaleTable =
  [ ("minPent", minPent),
    ("majPent", majPent),
    ("ritusen", ritusen),
    ("egyptian", egyptian),
    ("kumai", kumai),
    ("hirajoshi", hirajoshi),
    ("iwato", iwato),
    ("chinese", chinese),
    ("indian", indian),
    ("pelog", pelog),
    ("prometheus", prometheus),
    ("scriabin", scriabin),
    ("gong", gong),
    ("shang", shang),
    ("jiao", jiao),
    ("zhi", zhi),
    ("yu", yu),
    ("whole", whole'),
    ("wholetone", whole'),
    ("augmented", augmented),
    ("augmented2", augmented2),
    ("hexMajor7", hexMajor7),
    ("hexDorian", hexDorian),
    ("hexPhrygian", hexPhrygian),
    ("hexSus", hexSus),
    ("hexMajor6", hexMajor6),
    ("hexAeolian", hexAeolian),
    ("major", major),
    ("ionian", ionian),
    ("dorian", dorian),
    ("phrygian", phrygian),
    ("lydian", lydian),
    ("mixolydian", mixolydian),
    ("aeolian", aeolian),
    ("minor", minor),
    ("locrian", locrian),
    ("harmonicMinor", harmonicMinor),
    ("harmonicMajor", harmonicMajor),
    ("melodicMinor", melodicMinor),
    ("melodicMinorDesc", melodicMinorDesc),
    ("melodicMajor", melodicMajor),
    ("bartok", bartok),
    ("hindu", hindu),
    ("todi", todi),
    ("purvi", purvi),
    ("marva", marva),
    ("bhairav", bhairav),
    ("ahirbhairav", ahirbhairav),
    ("superLocrian", superLocrian),
    ("romanianMinor", romanianMinor),
    ("hungarianMinor", hungarianMinor),
    ("neapolitanMinor", neapolitanMinor),
    ("enigmatic", enigmatic),
    ("spanish", spanish),
    ("leadingWhole", leadingWhole),
    ("lydianMinor", lydianMinor),
    ("neapolitanMajor", neapolitanMajor),
    ("locrianMajor", locrianMajor),
    ("diminished", diminished),
    ("octatonic", diminished),
    ("diminished2", diminished2),
    ("octatonic2", diminished2),
    ("messiaen1", messiaen1),
    ("messiaen2", messiaen2),
    ("messiaen3", messiaen3),
    ("messiaen4", messiaen4),
    ("messiaen5", messiaen5),
    ("messiaen6", messiaen6),
    ("messiaen7", messiaen7),
    ("chromatic", chromatic),
    ("bayati", bayati),
    ("hijaz", hijaz),
    ("sikah", sikah),
    ("rast", rast),
    ("saba", saba),
    ("iraq", iraq)
  ]

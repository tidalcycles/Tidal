module Sound.Tidal.Chords where

{-
    Chords.hs - For .. chords
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

import Data.Maybe

import Sound.Tidal.Pattern

-- major chords
major :: Num a => [a]
major = [0,4,7]
major7 :: Num a => [a]
major7 = [0,4,7,11]
six :: Num a => [a]
six = [0,4,7,9]
sixby9 :: Num a => [a]
sixby9 = [0,4,7,9,14]
maj9 :: Num a => [a]
maj9 = [0,4,7,11,14]
maj11 :: Num a => [a]
maj11 = [0,4,7,11,14,17]
maj13 :: Num a => [a]
maj13 = [0,4,7,11,14,17,21]
aug :: Num a => [a]
aug = [0,4,8]
add9 :: Num a => [a]
add9 = [0,4,7,14]
add11 :: Num a => [a]
add11 = [0,4,7,21]
add13 :: Num a => [a]
add13 = [0,4,7,21]
-- dominant chords
dom7 :: Num a => [a]
dom7 = [0,4,7,10]
dom9 :: Num a => [a]
dom9 = [0,4,7,10,14]
dom11 :: Num a => [a]
dom11 = [0,4,7,10,17]
dom13 :: Num a => [a]
dom13 = [0,4,7,10,21]
sevenFlat5 :: Num a => [a]
sevenFlat5 = [0,4,6,10]
sevenSharp5 :: Num a => [a]
sevenSharp5 = [0,4,8,10]
sevenFlat9 :: Num a => [a]
sevenFlat9 = [0,4,7,10,13]
sevenSharp5flat9 :: Num a => [a]
sevenSharp5flat9 = [0,4,8,10,13]
eleven :: Num a => [a]
eleven = [0,4,7,10,14,17]
elevenSharp :: Num a => [a]
elevenSharp = [0,4,7,10,14,18]
thirteen :: Num a => [a]
thirteen = [0,4,7,10,14,17,21]
-- minor chords
minor :: Num a => [a]
minor = [0,3,7]
minor7 :: Num a => [a]
minor7 = [0,3,7,10]
dim :: Num a => [a]
dim = [0,3,6]
dim7 :: Num a => [a]
dim7 = [0,3,6,9]
m6 :: Num a => [a]
m6 = [0,3,7,9]
m6by9 :: Num a => [a]
m6by9 = [0,3,9,7,14]
m7flat5 :: Num a => [a]
m7flat5 = [0,3,6,10]
m7flat9 :: Num a => [a]
m7flat9 = [0,3,7,10,14]
m7sharp9 :: Num a => [a]
m7sharp9 = [0,3,7,10,14]
m9 :: Num a => [a]
m9 = [0,3,7,10,14]
m11 :: Num a => [a]
m11 = [0,3,7,10,14,17]
m11sharp :: Num a => [a]
m11sharp = [0,3,7,10,14,18]
m13 :: Num a => [a]
m13 = [0,3,7,10,14,17,21]
-- other chords
one :: Num a => [a]
one = [0]
five :: Num a => [a]
five = [0,7]
sus2 :: Num a => [a]
sus2 = [0,2,7]
sus4 :: Num a => [a]
sus4 = [0,5,7]
sevenSus2 :: Num a => [a]
sevenSus2 = [0,2,7,10]
sevenSus4 :: Num a => [a]
sevenSus4 = [0,5,7,10]
nineSus4 :: Num a => [a]
nineSus4 = [0,5,7,10,14]

-- | @chordate cs m n@ selects the @n@th "chord" (a chord is a list of Ints)
-- from a list of chords @cs@ and transposes it by @m@
-- chordate :: Num b => [[b]] -> b -> Int -> [b]
-- chordate cs m n = map (+m) $ cs!!n

-- | @enchord chords pn pc@ turns every note in the note pattern @pn@ into
-- a chord, selecting from the chord lists @chords@ using the index pattern
-- @pc@.  For example, @Chords.enchord [Chords.major Chords.minor] "c g" "0 1"@
-- will create a pattern of a C-major chord followed by a G-minor chord.
-- enchord :: Num a => [[a]] -> Pattern a -> Pattern Int -> Pattern a
-- enchord chords pn pc = flatpat $ (chordate chords) <$> pn <*> pc

chordTable :: Num a => [(String, [a])]
chordTable = [("major", major),
              ("maj", major),
              ("minor", minor),
              ("min", minor),
              ("aug", aug),
              ("plus", aug),
              ("sharp5", aug),
              ("dim", dim),
              ("major7", major7),
              ("maj7", major7),
              ("add9", add9),
              ("add11", add11),
              ("add13", add13),
              ("dom7", dom7),
              ("dom9", dom9),
              ("dom11", dom11),
              ("dom13", dom13),
              ("minor7", minor7),
              ("min7", minor7),
              ("dim7", dim7),
              ("one", one),
              ("1", one),
              ("five", five),
              ("5", five),
              ("sharp5", sharp5),
              ("sus2", sus2),
              ("sus4", sus4),
              ("six", six),
              ("6", six),
              ("m6", m6),
              ("sevenSus2", sevenSus2),
              ("7sus2", sevenSus2),
              ("sevenSus4", sevenSus4),
              ("7sus4", sevenSus4),
              ("sevenFlat5", sevenFlat5),
              ("7f5", sevenFlat5),
              ("m7flat5", m7flat5),
              ("m7f5", m7flat5),
              ("sevenSharp5", sevenSharp5),
              ("7s5", sevenSharp5),
              ("nine", dom9),
              ("m9", m9),
              ("m7sharp9", m7sharp9),
              ("m7s9", m7sharp9),
              ("maj9", maj9),
              ("nineSus4", nineSus4),
              ("ninesus4", nineSus4),
              ("9sus4", nineSus4),
              ("sixby9", sixby9),
              ("6by9", sixby9),
              ("m6by9", m6by9),
              ("sevenFlat9", sevenFlat9),
              ("7f9", sevenFlat9),
              ("m7flat9", m7flat9),
              ("m7f9", m7flat9),
              ("sevenSharp5flat9", sevenSharp5flat9),
              ("7s5f9", sevenSharp5flat9),
              ("eleven", eleven),
              ("11", eleven),
              ("m11", m11),
              ("maj11", maj11),
              ("maj13", maj13),
              ("elevenSharp", elevenSharp),
              ("11s", elevenSharp),
              ("m11sharp", m11sharp),
              ("m11s", m11sharp),
              ("thirteen", thirteen),
              ("13", thirteen),
              ("m13", m13)
             ]

chordL :: Num a => Pattern String -> Pattern [a]
chordL p = (\name -> fromMaybe [] $ lookup name chordTable) <$> p

chordList :: String
chordList = unwords $ map fst (chordTable :: [(String, [Int])])


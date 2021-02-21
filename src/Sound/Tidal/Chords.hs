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
aug :: Num a => [a]
aug = [0,4,8]
six :: Num a => [a]
six = [0,4,7,9]
sixNine :: Num a => [a]
sixNine = [0,4,7,9,14]
major7 :: Num a => [a]
major7 = [0,4,7,11]
major9 :: Num a => [a]
major9 = [0,4,7,11,14]
add9 :: Num a => [a]
add9 = [0,4,7,14]
major11 :: Num a => [a]
major11 = [0,4,7,11,14,17]
add11 :: Num a => [a]
add11 = [0,4,7,17]
major13 :: Num a => [a]
major13 = [0,4,7,11,14,21]
add13 :: Num a => [a]
add13 = [0,4,7,21]
-- dominant chords
dom7 :: Num a => [a]
dom7 = [0,4,7,10]
dom9 :: Num a => [a]
dom9 = [0,4,7,14]
dom11 :: Num a => [a]
dom11 = [0,4,7,17]
dom13 :: Num a => [a]
dom13 = [0,4,7,21]
sevenFlat5 :: Num a => [a]
sevenFlat5 = [0,4,6,10]
sevenSharp5 :: Num a => [a]
sevenSharp5 = [0,4,8,10]
sevenFlat9 :: Num a => [a]
sevenFlat9 = [0,4,7,10,13]
nine :: Num a => [a]
nine = [0,4,7,10,14]
eleven :: Num a => [a]
eleven = [0,4,7,10,14,17]
thirteen :: Num a => [a]
thirteen = [0,4,7,10,14,17,21]
-- minor chords
minor :: Num a => [a]
minor = [0,3,7]
diminished :: Num a => [a]
diminished = [0,3,6]
minorSharp5 :: Num a => [a]
minorSharp5 = [0,3,8]
minor6 :: Num a => [a]
minor6 = [0,3,7,9]
minor69 :: Num a => [a]
minor69 = [0,3,9,7,14]
minor7flat5 :: Num a => [a]
minor7flat5 = [0,3,6,10]
minor7 :: Num a => [a]
minor7 = [0,3,7,10]
minor7sharp5 :: Num a => [a]
minor7sharp5 = [0,3,8,10]
minor7flat9 :: Num a => [a]
minor7flat9 = [0,3,7,10,13]
minor7sharp9 :: Num a => [a]
minor7sharp9 = [0,3,7,10,14]
diminished7 :: Num a => [a]
diminished7 = [0,3,6,9]
minor9 :: Num a => [a]
minor9 = [0,3,7,10,14]
minor11 :: Num a => [a]
minor11 = [0,3,7,10,14,17]
minor13 :: Num a => [a]
minor13 = [0,3,7,10,14,17,21]
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
-- questionable chords
sevenFlat10 :: Num a => [a]
sevenFlat10 = [0,4,7,10,15]
nineSharp5 :: Num a => [a]
nineSharp5 = [0,1,13]
m9sharp5 :: Num a => [a]
m9sharp5 = [0,1,14]
sevenSharp5flat9 :: Num a => [a]
sevenSharp5flat9 = [0,4,8,10,13]
m7sharp5flat9 :: Num a => [a]
m7sharp5flat9 = [0,3,8,10,13]
elevenSharp :: Num a => [a]
elevenSharp = [0,4,7,10,14,18]
m11sharp :: Num a => [a]
m11sharp = [0,3,7,10,14,18]

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
              ("aug", aug),
              ("plus", aug),
              ("sharp5", aug),
              ("six", six),
              ("6", six),
              ("sixNine", sixNine),
              ("six9", sixNine),
              ("sixby9", sixNine),
              ("6by9", sixNine),
              ("major7", major7),
              ("maj7", major7),
              ("major9", major9),
              ("maj9", major9),
              ("add9", add9),
              ("major11", major11),
              ("maj11", major11),
              ("add11", add11),
              ("major13", major13),
              ("maj13", major13),
              ("add13", add13),
              ("dom7", dom7),
              ("dom9", dom9),
              ("dom11", dom11),
              ("dom13", dom13),
              ("sevenFlat5", sevenFlat5),
              ("7f5", sevenFlat5),
              ("sevenSharp5", sevenSharp5),
              ("7s5", sevenSharp5),
              ("sevenFlat9", sevenFlat9),
              ("7f9", sevenFlat9),
              ("nine", nine),
              ("eleven", eleven),
              ("11", eleven),
              ("thirteen", thirteen),
              ("13", thirteen),
              ("minor", minor),
              ("min", minor),
              ("diminished", diminished),
              ("dim", diminished),
              ("minorSharp5", minorSharp5),
              ("msharp5", minorSharp5),
              ("mS5", minorSharp5),
              ("minor6", minor6),
              ("min6", minor6),
              ("m6", minor6),
              ("minor69", minor69),
              ("min69", minor69),
              ("m69", minor69),
              ("m6by9", minor69),
              ("minor7flat5", minor7flat5),
              ("min7flat5", minor7flat5),
              ("m7flat5", minor7flat5),
              ("m7f5", minor7flat5),
              ("minor7", minor7),
              ("min7", minor7),
              ("m7", minor7),
              ("minor7sharp5", minor7sharp5),
              ("min7sharp5", minor7sharp5),
              ("m7sharp5", minor7sharp5),
              ("m7s5", minor7sharp5),
              ("minor7flat9", minor7flat9),
              ("min7flat9", minor7flat9),
              ("m7flat9", minor7flat9),
              ("m7f9", minor7flat9),
              ("minor7sharp9", minor7sharp9),
              ("min7sharp9", minor7sharp9),
              ("m7sharp9", minor7sharp9),
              ("m7s9", minor7sharp9),
              ("diminished7", diminished7),
              ("dim7", diminished7),
              ("minor9", minor9),
              ("min9", minor9),
              ("m9", minor9),
              ("minor11", minor11),
              ("min11", minor11),
              ("m11", minor11),
              ("minor13", minor13),
              ("min13", minor13),
              ("m13", minor13),
              ("one", one),
              ("1", one),
              ("five", five),
              ("5", five),
              ("sus2", sus2),
              ("sus4", sus4),
              ("sevenSus2", sevenSus2),
              ("7sus2", sevenSus2),
              ("sevenSus4", sevenSus4),
              ("7sus4", sevenSus4),
              ("nineSus4", nineSus4),
              ("ninesus4", nineSus4),
              ("9sus4", nineSus4),
              ("sevenFlat10", sevenFlat10),
              ("7f10", sevenFlat10),
              ("nineSharp5", nineSharp5),
              ("9s5", nineSharp5),
              ("m9sharp5", m9sharp5),
              ("m9s5", m9sharp5),
              ("sevenSharp5flat9", sevenSharp5flat9),
              ("7s5f9", sevenSharp5flat9),
              ("m7sharp5flat9", m7sharp5flat9),
              ("elevenSharp", elevenSharp),
              ("11s", elevenSharp),
              ("m11sharp", m11sharp),
              ("m11s", m11sharp)
             ]

chordL :: Num a => Pattern String -> Pattern [a]
chordL p = (\name -> fromMaybe [] $ lookup name chordTable) <$> p

chordList :: String
chordList = unwords $ map fst (chordTable :: [(String, [Int])])


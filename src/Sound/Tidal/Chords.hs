module Sound.Tidal.Chords where

import Data.Maybe

import Sound.Tidal.Pattern

major :: Num a => [a]
major = [0,4,7]
minor :: Num a => [a]
minor = [0,3,7]
major7 :: Num a => [a]
major7 = [0,4,7,11]
dom7 :: Num a => [a]
dom7 = [0,4,7,10]
minor7 :: Num a => [a]
minor7 = [0,3,7,10]
aug :: Num a => [a]
aug = [0,4,8]
dim :: Num a => [a]
dim = [0,3,6]
dim7 :: Num a => [a]
dim7 = [0,3,6,9]
one :: Num a => [a]
one = [0]
five :: Num a => [a]
five = [0,7]
plus :: Num a => [a]
plus = [0,4,8]
sharp5 :: Num a => [a]
sharp5 = [0,4,8]
msharp5 :: Num a => [a]
msharp5 = [0,3,8]
sus2 :: Num a => [a]
sus2 = [0,2,7]
sus4 :: Num a => [a]
sus4 = [0,5,7]
six :: Num a => [a]
six = [0,4,7,9]
m6 :: Num a => [a]
m6 = [0,3,7,9]
sevenSus2 :: Num a => [a]
sevenSus2 = [0,2,7,10]
sevenSus4 :: Num a => [a]
sevenSus4 = [0,5,7,10]
sevenFlat5 :: Num a => [a]
sevenFlat5 = [0,4,6,10]
m7flat5 :: Num a => [a]
m7flat5 = [0,3,6,10]
sevenSharp5 :: Num a => [a]
sevenSharp5 = [0,4,8,10]
m7sharp5 :: Num a => [a]
m7sharp5 = [0,3,8,10]
nine :: Num a => [a]
nine = [0,4,7,10,14]
m9 :: Num a => [a]
m9 = [0,3,7,10,14]
m7sharp9 :: Num a => [a]
m7sharp9 = [0,3,7,10,14]
maj9 :: Num a => [a]
maj9 = [0,4,7,11,14]
nineSus4 :: Num a => [a]
nineSus4 = [0,5,7,10,14]
sixby9 :: Num a => [a]
sixby9 = [0,4,7,9,14]
m6by9 :: Num a => [a]
m6by9 = [0,3,9,7,14]
sevenFlat9 :: Num a => [a]
sevenFlat9 = [0,4,7,10,13]
m7flat9 :: Num a => [a]
m7flat9 = [0,3,7,10,13]
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
eleven :: Num a => [a]
eleven = [0,4,7,10,14,17]
m11 :: Num a => [a]
m11 = [0,3,7,10,14,17]
maj11 :: Num a => [a]
maj11 = [0,4,7,11,14,17]
elevenSharp :: Num a => [a]
elevenSharp = [0,4,7,10,14,18]
m11sharp :: Num a => [a]
m11sharp = [0,3,7,10,14,18]
thirteen :: Num a => [a]
thirteen = [0,4,7,10,14,17,21]
m13 :: Num a => [a]
m13 = [0,3,7,10,14,17,21]

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
              ("major7", major7),
              ("maj7", major7),
              ("dom7", dom7),
              ("minor7", minor7),
              ("min7", minor7),
              ("aug", aug),
              ("dim", dim),
              ("dim7", dim7),
              ("one", one),
              ("1", one),
              ("five", five),
              ("5", five),
              ("plus", plus),
              ("sharp5", sharp5),
              ("msharp5", msharp5),
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
              ("m7sharp5", m7sharp5),
              ("m7s5", m7sharp5),
              ("nine", nine),
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
              ("sevenFlat10", sevenFlat10),
              ("7f10", sevenFlat10),
              ("nineSharp5", nineSharp5),
              ("9s5", nineSharp5),
              ("m9sharp5", m9sharp5),
              ("m9s5", m9sharp5),
              ("sevenSharp5flat9", sevenSharp5flat9),
              ("7s5f9", sevenSharp5flat9),
              ("m7sharp5flat9", m7sharp5flat9),
              ("eleven", eleven),
              ("11", eleven),
              ("m11", m11),
              ("maj11", maj11),
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


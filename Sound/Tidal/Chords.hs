module Sound.Tidal.Chords where

import Sound.Tidal.Pattern
import Data.Maybe

major :: [Int]
major = [0,4,7]
minor :: [Int]
minor = [0,3,7]
major7 :: [Int]
major7 = [0,4,7,11]
dom7 :: [Int]
dom7 = [0,4,7,10]
minor7 :: [Int]
minor7 = [0,3,7,10]
aug :: [Int]
aug = [0,4,8]
dim :: [Int]
dim = [0,3,6]
dim7 :: [Int]
dim7 = [0,3,6,9]
one :: [Int]
one = [0]
five :: [Int]
five = [0,7]
plus :: [Int]
plus = [0,4,8]
sharp5 :: [Int]
sharp5 = [0,4,8]
msharp5 :: [Int]
msharp5 = [0,3,8]
sus2 :: [Int]
sus2 = [0,2,7]
sus4 :: [Int]
sus4 = [0,5,7]
six :: [Int]
six = [0,4,7,9]
m6 :: [Int]
m6 = [0,3,7,9]
sevenSus2 :: [Int]
sevenSus2 = [0,2,7,10]
sevenSus4 :: [Int]
sevenSus4 = [0,5,7,10]
sevenFlat5 :: [Int]
sevenFlat5 = [0,4,6,10]
m7flat5 :: [Int]
m7flat5 = [0,3,6,10]
sevenSharp5 :: [Int]
sevenSharp5 = [0,4,8,10]
m7sharp5 :: [Int]
m7sharp5 = [0,3,8,10]
nine :: [Int]
nine = [0,4,7,10,14]
m9 :: [Int]
m9 = [0,3,7,10,14]
m7sharp9 :: [Int]
m7sharp9 = [0,3,7,10,14]
maj9 :: [Int]
maj9 = [0,4,7,11,14]
nineSus4 :: [Int]
nineSus4 = [0,5,7,10,14]
sixby9 :: [Int]
sixby9 = [0,4,7,9,14]
m6by9 :: [Int]
m6by9 = [0,3,9,7,14]
sevenFlat9 :: [Int]
sevenFlat9 = [0,4,7,10,13]
m7flat9 :: [Int]
m7flat9 = [0,3,7,10,13]
sevenFlat10 :: [Int]
sevenFlat10 = [0,4,7,10,15]
nineSharp5 :: [Int]
nineSharp5 = [0,1,13]
m9sharp5 :: [Int]
m9sharp5 = [0,1,14]
sevenSharp5flat9 :: [Int]
sevenSharp5flat9 = [0,4,8,10,13]
m7sharp5flat9 :: [Int]
m7sharp5flat9 = [0,3,8,10,13]
eleven :: [Int]
eleven = [0,4,7,10,14,17]
m11 :: [Int]
m11 = [0,3,7,10,14,17]
maj11 :: [Int]
maj11 = [0,4,7,11,14,17]
evelenSharp :: [Int]
evelenSharp = [0,4,7,10,14,18]
m11sharp :: [Int]
m11sharp = [0,3,7,10,14,18]
thirteen :: [Int]
thirteen = [0,4,7,10,14,17,21]
m13 :: [Int]
m13 = [0,3,7,10,14,17,21]

-- | @chordate cs m n@ selects the @n@th "chord" (a chord is a list of Ints)
-- from a list of chords @cs@ and transposes it by @m@
chordate :: Num b => [[b]] -> b -> Int -> [b]
chordate cs m n = map (+m) $ cs!!n

-- | @flatpat@ takes a Pattern of lists and pulls the list elements as
-- separate Events
flatpat :: Pattern [a] -> Pattern a
flatpat p = stack [unMaybe $ fmap (`maybeInd` i) p | i <- [0..9]]
  where maybeInd xs i | i < length xs = Just $ xs!!i
                      | otherwise = Nothing
        unMaybe = (fromJust <$>) . filterValues isJust

-- | @enchord chords pn pc@ turns every note in the note pattern @pn@ into
-- a chord, selecting from the chord lists @chords@ using the index pattern
-- @pc@.  For example, @Chords.enchord [Chords.major Chords.minor] "c g" "0 1"@
-- will create a pattern of a C-major chord followed by a G-minor chord.
enchord :: Num a => [[a]] -> Pattern a -> Pattern Int -> Pattern a
enchord chords pn pc = flatpat $ (chordate chords) <$> pn <*> pc

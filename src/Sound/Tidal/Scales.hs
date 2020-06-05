module Sound.Tidal.Scales (scale, scaleList, scaleTable, getScale) where

import Prelude hiding ((<*), (*>))
import Data.Maybe
import Sound.Tidal.Pattern
import Sound.Tidal.Utils

-- five notes scales
minPent :: Fractional a => [a]
minPent = [0,3,5,7,10]
majPent :: Fractional a => [a]
majPent = [0,2,4,7,9]

--  another mode of major pentatonic
ritusen :: Fractional a => [a]
ritusen = [0,2,5,7,9]

-- another mode of major pentatonic
egyptian :: Fractional a => [a]
egyptian = [0,2,5,7,10]

--
kumai :: Fractional a => [a]
kumai = [0,2,3,7,9]
hirajoshi :: Fractional a => [a]
hirajoshi = [0,2,3,7,8]
iwato :: Fractional a => [a]
iwato = [0,1,5,6,10]
chinese :: Fractional a => [a]
chinese = [0,4,6,7,11]
indian :: Fractional a => [a]
indian = [0,4,5,7,10]
pelog :: Fractional a => [a]
pelog = [0,1,3,7,8]

--
prometheus :: Fractional a => [a]
prometheus = [0,2,4,6,11]
scriabin :: Fractional a => [a]
scriabin = [0,1,4,7,9]

-- han chinese pentatonic scales
gong :: Fractional a => [a]
gong = [0,2,4,7,9]
shang :: Fractional a => [a]
shang = [0,2,5,7,10]
jiao :: Fractional a => [a]
jiao = [0,3,5,8,10]
zhi :: Fractional a => [a]
zhi = [0,2,5,7,9]
yu :: Fractional a => [a]
yu = [0,3,5,7,10]

-- 6 note scales
whole' :: Fractional a => [a]
whole' = [0,2,4,6,8,10]
augmented :: Fractional a => [a]
augmented = [0,3,4,7,8,11]
augmented2 :: Fractional a => [a]
augmented2 = [0,1,4,5,8,9]

-- hexatonic modes with no tritone
hexMajor7 :: Fractional a => [a]
hexMajor7 = [0,2,4,7,9,11]
hexDorian :: Fractional a => [a]
hexDorian = [0,2,3,5,7,10]
hexPhrygian :: Fractional a => [a]
hexPhrygian = [0,1,3,5,8,10]
hexSus :: Fractional a => [a]
hexSus = [0,2,5,7,9,10]
hexMajor6 :: Fractional a => [a]
hexMajor6 = [0,2,4,5,7,9]
hexAeolian :: Fractional a => [a]
hexAeolian = [0,3,5,7,8,10]

-- 7 note scales
major :: Fractional a => [a]
major = [0,2,4,5,7,9,11]
ionian :: Fractional a => [a]
ionian = [0,2,4,5,7,9,11]
dorian :: Fractional a => [a]
dorian = [0,2,3,5,7,9,10]
phrygian :: Fractional a => [a]
phrygian = [0,1,3,5,7,8,10]
lydian :: Fractional a => [a]
lydian = [0,2,4,6,7,9,11]
mixolydian :: Fractional a => [a]
mixolydian = [0,2,4,5,7,9,10]
aeolian :: Fractional a => [a]
aeolian = [0,2,3,5,7,8,10]
minor :: Fractional a => [a]
minor = [0,2,3,5,7,8,10]
locrian :: Fractional a => [a]
locrian = [0,1,3,5,6,8,10]
harmonicMinor :: Fractional a => [a]
harmonicMinor = [0,2,3,5,7,8,11]
harmonicMajor :: Fractional a => [a]
harmonicMajor = [0,2,4,5,7,8,11]
melodicMinor :: Fractional a => [a]
melodicMinor = [0,2,3,5,7,9,11]
melodicMinorDesc :: Fractional a => [a]
melodicMinorDesc = [0,2,3,5,7,8,10]
melodicMajor :: Fractional a => [a]
melodicMajor = [0,2,4,5,7,8,10]
bartok :: Fractional a => [a]
bartok = melodicMajor
hindu :: Fractional a => [a]
hindu = melodicMajor

-- raga modes
todi :: Fractional a => [a]
todi = [0,1,3,6,7,8,11]
purvi :: Fractional a => [a]
purvi = [0,1,4,6,7,8,11]
marva :: Fractional a => [a]
marva = [0,1,4,6,7,9,11]
bhairav :: Fractional a => [a]
bhairav = [0,1,4,5,7,8,11]
ahirbhairav :: Fractional a => [a]
ahirbhairav = [0,1,4,5,7,9,10]

--
superLocrian :: Fractional a => [a]
superLocrian = [0,1,3,4,6,8,10]
romanianMinor :: Fractional a => [a]
romanianMinor = [0,2,3,6,7,9,10]
hungarianMinor :: Fractional a => [a]
hungarianMinor = [0,2,3,6,7,8,11]
neapolitanMinor :: Fractional a => [a]
neapolitanMinor = [0,1,3,5,7,8,11]
enigmatic :: Fractional a => [a]
enigmatic = [0,1,4,6,8,10,11]
spanish :: Fractional a => [a]
spanish = [0,1,4,5,7,8,10]

-- modes of whole tones with added note ->
leadingWhole :: Fractional a => [a]
leadingWhole = [0,2,4,6,8,10,11]
lydianMinor :: Fractional a => [a]
lydianMinor = [0,2,4,6,7,8,10]
neapolitanMajor :: Fractional a => [a]
neapolitanMajor = [0,1,3,5,7,9,11]
locrianMajor :: Fractional a => [a]
locrianMajor = [0,2,4,5,6,8,10]

-- 8 note scales
diminished :: Fractional a => [a]
diminished = [0,1,3,4,6,7,9,10]
diminished2 :: Fractional a => [a]
diminished2 = [0,2,3,5,6,8,9,11]

-- modes of limited transposition
messiaen1 :: Fractional a => [a]
messiaen1 = whole'
messiaen2 :: Fractional a => [a]
messiaen2 = diminished
messiaen3 :: Fractional a => [a]
messiaen3 = [0, 2, 3, 4, 6, 7, 8, 10, 11]
messiaen4 :: Fractional a => [a]
messiaen4 = [0, 1, 2, 5, 6, 7, 8, 11]
messiaen5 :: Fractional a => [a]
messiaen5 = [0, 1, 5, 6, 7, 11]
messiaen6 :: Fractional a => [a]
messiaen6 = [0, 2, 4, 5, 6, 8, 10, 11]
messiaen7 :: Fractional a => [a]
messiaen7 = [0, 1, 2, 3, 5, 6, 7, 8, 9, 11]

-- Arabic maqams taken from SuperCollider's Scale.sc
bayati :: Fractional a => [a]
bayati = [0, 1.5, 3, 5, 7, 8, 10]
hijaz :: Fractional a => [a]
hijaz = [0, 1, 4, 5, 7, 8.5, 10]
sikah :: Fractional a => [a]
sikah = [0, 1.5, 3.5, 5.5, 7, 8.5, 10.5]
rast :: Fractional a => [a]
rast = [0, 2, 3.5, 5, 7, 9, 10.5]
iraq :: Fractional a => [a]
iraq = [0, 1.5, 3.5, 5, 6.5, 8.5, 10.5]
saba :: Fractional a => [a]
saba = [0, 1.5, 3, 4, 6, 8, 10]

-- 12 note scales
chromatic :: Fractional a => [a]
chromatic = [0,1,2,3,4,5,6,7,8,9,10,11]

scale :: Fractional a => Pattern String -> Pattern Int -> Pattern a
scale = getScale scaleTable

getScale :: Fractional a => [(String, [a])] -> Pattern String -> Pattern Int -> Pattern a
getScale table sp p = (\n scaleName
              -> noteInScale (fromMaybe [0] $ lookup scaleName table) n) <$> p <* sp
  where octave s x = x `div` length s
        noteInScale s x = (s !!! x) + fromIntegral (12 * octave s x)

scaleList :: String
scaleList = unwords $ map fst (scaleTable :: [(String, [Rational])])

scaleTable :: Fractional a => [(String, [a])]
scaleTable = [("minPent", minPent),
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

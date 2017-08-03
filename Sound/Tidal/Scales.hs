module Sound.Tidal.Scales where

-- five notes scales
minPent :: [Int]
minPent = [0,3,5,7,10]
majPent :: [Int]
majPent = [0,2,4,7,9]

--  another mode of major pentatonic
ritusen :: [Int]
ritusen = [0,2,5,7,9]

-- another mode of major pentatonic
egyptian :: [Int]
egyptian = [0,2,5,7,10]

--
kumai :: [Int]
kumai = [0,2,3,7,9]
hirajoshi :: [Int]
hirajoshi = [0,2,3,7,8]
iwato :: [Int]
iwato = [0,1,5,6,10]
chinese :: [Int]
chinese = [0,4,6,7,11]
indian :: [Int]
indian = [0,4,5,7,10]
pelog :: [Int]
pelog = [0,1,3,7,8]

--
prometheus :: [Int]
prometheus = [0,2,4,6,11]
scriabin :: [Int]
scriabin = [0,1,4,7,9]

-- han chinese pentatonic scales
gong :: [Int]
gong = [0,2,4,7,9]
shang :: [Int]
shang = [0,2,5,7,10]
jiao :: [Int]
jiao = [0,3,5,8,10]
zhi :: [Int]
zhi = [0,2,5,7,9]
yu :: [Int]
yu = [0,3,5,7,10]

-- 6 note scales
whole :: [Int]
whole = [0,2,4,6,8,10]
augmented :: [Int]
augmented = [0,3,4,7,8,11]
augmented2 :: [Int]
augmented2 = [0,1,4,5,8,9]

-- hexatonic modes with no tritone
hexMajor7 :: [Int]
hexMajor7 = [0,2,4,7,9,11]
hexDorian :: [Int]
hexDorian = [0,2,3,5,7,10]
hexPhrygian :: [Int]
hexPhrygian = [0,1,3,5,8,10]
hexSus :: [Int]
hexSus = [0,2,5,7,9,10]
hexMajor6 :: [Int]
hexMajor6 = [0,2,4,5,7,9]
hexAeolian :: [Int]
hexAeolian = [0,3,5,7,8,10]

-- 7 note scales
major :: [Int]
major = [0,2,4,5,7,9,11]
ionian :: [Int]
ionian = [0,2,4,5,7,9,11]
dorian :: [Int]
dorian = [0,2,3,5,7,9,10]
phrygian :: [Int]
phrygian = [0,1,3,5,7,8,10]
lydian :: [Int]
lydian = [0,2,4,6,7,9,11]
mixolydian :: [Int]
mixolydian = [0,2,4,5,7,9,10]
aeolian :: [Int]
aeolian = [0,2,3,5,7,8,10]
minor :: [Int]
minor = [0,2,3,5,7,8,10]
locrian :: [Int]
locrian = [0,1,3,5,6,8,10]
harmonicMinor :: [Int]
harmonicMinor = [0,2,3,5,7,8,11]
harmonicMajor :: [Int]
harmonicMajor = [0,2,4,5,7,8,11]
melodicMinor :: [Int]
melodicMinor = [0,2,3,5,7,9,11]
melodicMinorDesc :: [Int]
melodicMinorDesc = [0,2,3,5,7,8,10]
melodicMajor :: [Int]
melodicMajor = [0,2,4,5,7,8,10]
bartok :: [Int]
bartok = [0,2,4,5,7,8,10]
hindu :: [Int]
hindu = [0,2,4,5,7,8,10]

-- raga modes
todi :: [Int]
todi = [0,1,3,6,7,8,11]
purvi :: [Int]
purvi = [0,1,4,6,7,8,11]
marva :: [Int]
marva = [0,1,4,6,7,9,11]
bhairav :: [Int]
bhairav = [0,1,4,5,7,8,11]
ahirbhairav :: [Int]
ahirbhairav = [0,1,4,5,7,9,10]

--
superLocrian :: [Int]
superLocrian = [0,1,3,4,6,8,10]
romanianMinor :: [Int]
romanianMinor = [0,2,3,6,7,9,10]
hungarianMinor :: [Int]
hungarianMinor = [0,2,3,6,7,8,11]
neapolitanMinor :: [Int]
neapolitanMinor = [0,1,3,5,7,8,11]
enigmatic :: [Int]
enigmatic = [0,1,4,6,8,10,11]
spanish :: [Int]
spanish = [0,1,4,5,7,8,10]

-- modes of whole tones with added note ->
leadingWhole :: [Int]
leadingWhole = [0,2,4,6,8,10,11]
lydianMinor :: [Int]
lydianMinor = [0,2,4,6,7,8,10]
neapolitanMajor :: [Int]
neapolitanMajor = [0,1,3,5,7,9,11]
locrianMajor :: [Int]
locrianMajor = [0,2,4,5,6,8,10]

-- 8 note scales
diminished :: [Int]
diminished = [0,1,3,4,6,7,9,10]
diminished2 :: [Int]
diminished2 = [0,2,3,5,6,8,9,11]

-- 12 note scales
chromatic :: [Int]
chromatic = [0..11]

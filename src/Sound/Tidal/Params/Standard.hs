module Sound.Tidal.Params.Standard where 

{-
    Generic.hs - Generated from Sound.Tidal.Params.Generate
    Copyright (C) 2020, Alex McLean and contributors
-}

import Data.Word (Word8)

import Sound.Tidal.Pattern
import Sound.Tidal.Core ((#))
import Sound.Tidal.Params



-- | Grouped params

sound :: Pattern String -> ControlPattern
sound = grp [mS "s", mF "n"]

cc :: Pattern String -> ControlPattern
cc = grp [mF "ccn", mF "ccv"]

nrpn :: Pattern String -> ControlPattern
nrpn = grp [mI "nrpn", mI "val"]

grain' :: Pattern String -> ControlPattern
grain' = grp [mF "begin", mF "end"]

midinote :: Pattern Note -> ControlPattern
midinote = note . (subtract 60 <$>)

drum :: Pattern String -> ControlPattern
drum = n . (subtract 60 . drumN <$>)

drumN :: Num a => String -> a
drumN "hq" = 27
drumN "sl" = 28
drumN "ps" = 29
drumN "pl" = 30
drumN "st" = 31
drumN "sq" = 32
drumN "ml" = 33
drumN "mb" = 34
drumN "ab" = 35
drumN "bd" = 36
drumN "rm" = 37
drumN "sn" = 38
drumN "cp" = 39
drumN "es" = 40
drumN "lf" = 41
drumN "ch" = 42
drumN "lt" = 43
drumN "hh" = 44
drumN "ft" = 45
drumN "oh" = 46
drumN "mt" = 47
drumN "hm" = 48
drumN "cr" = 49
drumN "ht" = 50
drumN "ri" = 51
drumN "cy" = 52
drumN "be" = 53
drumN "ta" = 54
drumN "sc" = 55
drumN "cb" = 56
drumN "cs" = 57
drumN "vi" = 58
drumN "rc" = 59
drumN "hb" = 60
drumN "lb" = 61
drumN "mh" = 62
drumN "hc" = 63
drumN "lc" = 64
drumN "he" = 65
drumN "le" = 66
drumN "ag" = 67
drumN "la" = 68
drumN "ca" = 69
drumN "ma" = 70
drumN "sw" = 71
drumN "lw" = 72
drumN "sg" = 73
drumN "lg" = 74
drumN "cl" = 75
drumN "hi" = 76
drumN "li" = 77
drumN "mc" = 78
drumN "oc" = 79
drumN "tr" = 80
drumN "ot" = 81
drumN "sh" = 82
drumN "jb" = 83
drumN "bt" = 84
drumN "ct" = 85
drumN "ms" = 86
drumN "os" = 87
drumN _ = 0

-- | 
xsdelay :: Pattern Double -> ControlPattern
xsdelay = pF "xsdelay"
xsdelaybus :: Pattern Int -> Pattern Double -> ControlPattern
xsdelaybus busid pat = (pF "xsdelay" pat) # (pI "^xsdelay" busid)

-- | 
waveloss :: Pattern Double -> ControlPattern
waveloss = pF "waveloss"
wavelossbus :: Pattern Int -> Pattern Double -> ControlPattern
wavelossbus busid pat = (pF "waveloss" pat) # (pI "^waveloss" busid)

-- | formant filter to make things sound like vowels, a pattern of either `a`, `e`, `i`, `o` or `u`. Use a rest (`~`) for no effect.
vowel :: Pattern String -> ControlPattern
vowel = pS "vowel"
vowelbus :: Pattern Int -> Pattern String -> ControlPattern
vowelbus busid pat = (pS "vowel" pat) # (pI "^vowel" busid)

-- | 
voice :: Pattern Double -> ControlPattern
voice = pF "voice"
voicebus :: Pattern Int -> Pattern Double -> ControlPattern
voicebus busid pat = (pF "voice" pat) # (pI "^voice" busid)

-- | 
velocity :: Pattern Double -> ControlPattern
velocity = pF "velocity"
velocitybus :: Pattern Int -> Pattern Double -> ControlPattern
velocitybus busid pat = (pF "velocity" pat) # (pI "^velocity" busid)

-- | 
vcoegint :: Pattern Double -> ControlPattern
vcoegint = pF "vcoegint"
vcoegintbus :: Pattern Int -> Pattern Double -> ControlPattern
vcoegintbus busid pat = (pF "vcoegint" pat) # (pI "^vcoegint" busid)

-- | 
vcfegint :: Pattern Double -> ControlPattern
vcfegint = pF "vcfegint"
vcfegintbus :: Pattern Int -> Pattern Double -> ControlPattern
vcfegintbus busid pat = (pF "vcfegint" pat) # (pI "^vcfegint" busid)

-- | 
val :: Pattern Double -> ControlPattern
val = pF "val"
valbus :: Pattern Int -> Pattern Double -> ControlPattern
valbus busid pat = (pF "val" pat) # (pI "^val" busid)

-- | used in conjunction with `speed`, accepts values of "r" (rate, default behavior), "c" (cycles), or "s" (seconds). Using `unit "c"` means `speed` will be interpreted in units of cycles, e.g. `speed "1"` means samples will be stretched to fill a cycle. Using `unit "s"` means the playback speed will be adjusted so that the duration is the number of seconds specified by `speed`.
unit :: Pattern String -> ControlPattern
unit = pS "unit"
unitbus :: Pattern Int -> Pattern String -> ControlPattern
unitbus _ _ = error $ "Control parameter 'unit' can't be sent to a bus."

-- | 
uid :: Pattern Double -> ControlPattern
uid = pF "uid"
uidbus :: Pattern Int -> Pattern Double -> ControlPattern
uidbus busid pat = (pF "uid" pat) # (pI "^uid" busid)

-- | 
tsdelay :: Pattern Double -> ControlPattern
tsdelay = pF "tsdelay"
tsdelaybus :: Pattern Int -> Pattern Double -> ControlPattern
tsdelaybus busid pat = (pF "tsdelay" pat) # (pI "^tsdelay" busid)

-- | tube distortion
triode :: Pattern Double -> ControlPattern
triode = pF "triode"
triodebus :: Pattern Int -> Pattern Double -> ControlPattern
triodebus busid pat = (pF "triode" pat) # (pI "^triode" busid)

-- | Tremolo Audio DSP effect | params are 'tremolorate' and 'tremolodepth'
tremolorate :: Pattern Double -> ControlPattern
tremolorate = pF "tremolorate"
tremoloratebus :: Pattern Int -> Pattern Double -> ControlPattern
tremoloratebus busid pat = (pF "tremolorate" pat) # (pI "^tremolorate" busid)

-- | Tremolo Audio DSP effect | params are 'tremolorate' and 'tremolodepth'
tremolodepth :: Pattern Double -> ControlPattern
tremolodepth = pF "tremolodepth"
tremolodepthbus :: Pattern Int -> Pattern Double -> ControlPattern
tremolodepthbus busid pat = (pF "tremolodepth" pat) # (pI "^tremolodepth" busid)

-- | 
tomdecay :: Pattern Double -> ControlPattern
tomdecay = pF "tomdecay"
tomdecaybus :: Pattern Int -> Pattern Double -> ControlPattern
tomdecaybus busid pat = (pF "tomdecay" pat) # (pI "^tomdecay" busid)

-- | for internal sound routing
toArg :: Pattern String -> ControlPattern
toArg = pS "toArg"
toArgbus :: Pattern Int -> Pattern String -> ControlPattern
toArgbus busid pat = (pS "toArg" pat) # (pI "^toArg" busid)

-- | for internal sound routing
to :: Pattern Double -> ControlPattern
to = pF "to"
tobus :: Pattern Int -> Pattern Double -> ControlPattern
tobus busid pat = (pF "to" pat) # (pI "^to" busid)

-- | 
sustainpedal :: Pattern Double -> ControlPattern
sustainpedal = pF "sustainpedal"
sustainpedalbus :: Pattern Int -> Pattern Double -> ControlPattern
sustainpedalbus busid pat = (pF "sustainpedal" pat) # (pI "^sustainpedal" busid)

-- | 
sustain :: Pattern Double -> ControlPattern
sustain = pF "sustain"
sustainbus :: Pattern Int -> Pattern Double -> ControlPattern
sustainbus _ _ = error $ "Control parameter 'sustain' can't be sent to a bus."

-- | 
stuttertime :: Pattern Double -> ControlPattern
stuttertime = pF "stuttertime"
stuttertimebus :: Pattern Int -> Pattern Double -> ControlPattern
stuttertimebus busid pat = (pF "stuttertime" pat) # (pI "^stuttertime" busid)

-- | 
stutterdepth :: Pattern Double -> ControlPattern
stutterdepth = pF "stutterdepth"
stutterdepthbus :: Pattern Int -> Pattern Double -> ControlPattern
stutterdepthbus busid pat = (pF "stutterdepth" pat) # (pI "^stutterdepth" busid)

-- | 
stepsPerOctave :: Pattern Double -> ControlPattern
stepsPerOctave = pF "stepsPerOctave"
stepsPerOctavebus :: Pattern Int -> Pattern Double -> ControlPattern
stepsPerOctavebus busid pat = (pF "stepsPerOctave" pat) # (pI "^stepsPerOctave" busid)

-- | 
squiz :: Pattern Double -> ControlPattern
squiz = pF "squiz"
squizbus :: Pattern Int -> Pattern Double -> ControlPattern
squizbus busid pat = (pF "squiz" pat) # (pI "^squiz" busid)

-- | a pattern of numbers which changes the speed of sample playback, i.e. a cheap way of changing pitch. Negative values will play the sample backwards!
speed :: Pattern Double -> ControlPattern
speed = pF "speed"
speedbus :: Pattern Int -> Pattern Double -> ControlPattern
speedbus _ _ = error $ "Control parameter 'speed' can't be sent to a bus."

-- | 
songPtr :: Pattern Double -> ControlPattern
songPtr = pF "songPtr"
songPtrbus :: Pattern Int -> Pattern Double -> ControlPattern
songPtrbus busid pat = (pF "songPtr" pat) # (pI "^songPtr" busid)

-- | Spectral smear
smear :: Pattern Double -> ControlPattern
smear = pF "smear"
smearbus :: Pattern Int -> Pattern Double -> ControlPattern
smearbus busid pat = (pF "smear" pat) # (pI "^smear" busid)

-- | 
slider9 :: Pattern Double -> ControlPattern
slider9 = pF "slider9"
slider9bus :: Pattern Int -> Pattern Double -> ControlPattern
slider9bus busid pat = (pF "slider9" pat) # (pI "^slider9" busid)

-- | 
slider8 :: Pattern Double -> ControlPattern
slider8 = pF "slider8"
slider8bus :: Pattern Int -> Pattern Double -> ControlPattern
slider8bus busid pat = (pF "slider8" pat) # (pI "^slider8" busid)

-- | 
slider7 :: Pattern Double -> ControlPattern
slider7 = pF "slider7"
slider7bus :: Pattern Int -> Pattern Double -> ControlPattern
slider7bus busid pat = (pF "slider7" pat) # (pI "^slider7" busid)

-- | 
slider63 :: Pattern Double -> ControlPattern
slider63 = pF "slider63"
slider63bus :: Pattern Int -> Pattern Double -> ControlPattern
slider63bus busid pat = (pF "slider63" pat) # (pI "^slider63" busid)

-- | 
slider62 :: Pattern Double -> ControlPattern
slider62 = pF "slider62"
slider62bus :: Pattern Int -> Pattern Double -> ControlPattern
slider62bus busid pat = (pF "slider62" pat) # (pI "^slider62" busid)

-- | 
slider61 :: Pattern Double -> ControlPattern
slider61 = pF "slider61"
slider61bus :: Pattern Int -> Pattern Double -> ControlPattern
slider61bus busid pat = (pF "slider61" pat) # (pI "^slider61" busid)

-- | 
slider60 :: Pattern Double -> ControlPattern
slider60 = pF "slider60"
slider60bus :: Pattern Int -> Pattern Double -> ControlPattern
slider60bus busid pat = (pF "slider60" pat) # (pI "^slider60" busid)

-- | 
slider6 :: Pattern Double -> ControlPattern
slider6 = pF "slider6"
slider6bus :: Pattern Int -> Pattern Double -> ControlPattern
slider6bus busid pat = (pF "slider6" pat) # (pI "^slider6" busid)

-- | 
slider59 :: Pattern Double -> ControlPattern
slider59 = pF "slider59"
slider59bus :: Pattern Int -> Pattern Double -> ControlPattern
slider59bus busid pat = (pF "slider59" pat) # (pI "^slider59" busid)

-- | 
slider58 :: Pattern Double -> ControlPattern
slider58 = pF "slider58"
slider58bus :: Pattern Int -> Pattern Double -> ControlPattern
slider58bus busid pat = (pF "slider58" pat) # (pI "^slider58" busid)

-- | 
slider57 :: Pattern Double -> ControlPattern
slider57 = pF "slider57"
slider57bus :: Pattern Int -> Pattern Double -> ControlPattern
slider57bus busid pat = (pF "slider57" pat) # (pI "^slider57" busid)

-- | 
slider56 :: Pattern Double -> ControlPattern
slider56 = pF "slider56"
slider56bus :: Pattern Int -> Pattern Double -> ControlPattern
slider56bus busid pat = (pF "slider56" pat) # (pI "^slider56" busid)

-- | 
slider55 :: Pattern Double -> ControlPattern
slider55 = pF "slider55"
slider55bus :: Pattern Int -> Pattern Double -> ControlPattern
slider55bus busid pat = (pF "slider55" pat) # (pI "^slider55" busid)

-- | 
slider54 :: Pattern Double -> ControlPattern
slider54 = pF "slider54"
slider54bus :: Pattern Int -> Pattern Double -> ControlPattern
slider54bus busid pat = (pF "slider54" pat) # (pI "^slider54" busid)

-- | 
slider53 :: Pattern Double -> ControlPattern
slider53 = pF "slider53"
slider53bus :: Pattern Int -> Pattern Double -> ControlPattern
slider53bus busid pat = (pF "slider53" pat) # (pI "^slider53" busid)

-- | 
slider52 :: Pattern Double -> ControlPattern
slider52 = pF "slider52"
slider52bus :: Pattern Int -> Pattern Double -> ControlPattern
slider52bus busid pat = (pF "slider52" pat) # (pI "^slider52" busid)

-- | 
slider51 :: Pattern Double -> ControlPattern
slider51 = pF "slider51"
slider51bus :: Pattern Int -> Pattern Double -> ControlPattern
slider51bus busid pat = (pF "slider51" pat) # (pI "^slider51" busid)

-- | 
slider50 :: Pattern Double -> ControlPattern
slider50 = pF "slider50"
slider50bus :: Pattern Int -> Pattern Double -> ControlPattern
slider50bus busid pat = (pF "slider50" pat) # (pI "^slider50" busid)

-- | 
slider5 :: Pattern Double -> ControlPattern
slider5 = pF "slider5"
slider5bus :: Pattern Int -> Pattern Double -> ControlPattern
slider5bus busid pat = (pF "slider5" pat) # (pI "^slider5" busid)

-- | 
slider49 :: Pattern Double -> ControlPattern
slider49 = pF "slider49"
slider49bus :: Pattern Int -> Pattern Double -> ControlPattern
slider49bus busid pat = (pF "slider49" pat) # (pI "^slider49" busid)

-- | 
slider48 :: Pattern Double -> ControlPattern
slider48 = pF "slider48"
slider48bus :: Pattern Int -> Pattern Double -> ControlPattern
slider48bus busid pat = (pF "slider48" pat) # (pI "^slider48" busid)

-- | 
slider47 :: Pattern Double -> ControlPattern
slider47 = pF "slider47"
slider47bus :: Pattern Int -> Pattern Double -> ControlPattern
slider47bus busid pat = (pF "slider47" pat) # (pI "^slider47" busid)

-- | 
slider46 :: Pattern Double -> ControlPattern
slider46 = pF "slider46"
slider46bus :: Pattern Int -> Pattern Double -> ControlPattern
slider46bus busid pat = (pF "slider46" pat) # (pI "^slider46" busid)

-- | 
slider45 :: Pattern Double -> ControlPattern
slider45 = pF "slider45"
slider45bus :: Pattern Int -> Pattern Double -> ControlPattern
slider45bus busid pat = (pF "slider45" pat) # (pI "^slider45" busid)

-- | 
slider44 :: Pattern Double -> ControlPattern
slider44 = pF "slider44"
slider44bus :: Pattern Int -> Pattern Double -> ControlPattern
slider44bus busid pat = (pF "slider44" pat) # (pI "^slider44" busid)

-- | 
slider43 :: Pattern Double -> ControlPattern
slider43 = pF "slider43"
slider43bus :: Pattern Int -> Pattern Double -> ControlPattern
slider43bus busid pat = (pF "slider43" pat) # (pI "^slider43" busid)

-- | 
slider42 :: Pattern Double -> ControlPattern
slider42 = pF "slider42"
slider42bus :: Pattern Int -> Pattern Double -> ControlPattern
slider42bus busid pat = (pF "slider42" pat) # (pI "^slider42" busid)

-- | 
slider41 :: Pattern Double -> ControlPattern
slider41 = pF "slider41"
slider41bus :: Pattern Int -> Pattern Double -> ControlPattern
slider41bus busid pat = (pF "slider41" pat) # (pI "^slider41" busid)

-- | 
slider40 :: Pattern Double -> ControlPattern
slider40 = pF "slider40"
slider40bus :: Pattern Int -> Pattern Double -> ControlPattern
slider40bus busid pat = (pF "slider40" pat) # (pI "^slider40" busid)

-- | 
slider4 :: Pattern Double -> ControlPattern
slider4 = pF "slider4"
slider4bus :: Pattern Int -> Pattern Double -> ControlPattern
slider4bus busid pat = (pF "slider4" pat) # (pI "^slider4" busid)

-- | 
slider39 :: Pattern Double -> ControlPattern
slider39 = pF "slider39"
slider39bus :: Pattern Int -> Pattern Double -> ControlPattern
slider39bus busid pat = (pF "slider39" pat) # (pI "^slider39" busid)

-- | 
slider38 :: Pattern Double -> ControlPattern
slider38 = pF "slider38"
slider38bus :: Pattern Int -> Pattern Double -> ControlPattern
slider38bus busid pat = (pF "slider38" pat) # (pI "^slider38" busid)

-- | 
slider37 :: Pattern Double -> ControlPattern
slider37 = pF "slider37"
slider37bus :: Pattern Int -> Pattern Double -> ControlPattern
slider37bus busid pat = (pF "slider37" pat) # (pI "^slider37" busid)

-- | 
slider36 :: Pattern Double -> ControlPattern
slider36 = pF "slider36"
slider36bus :: Pattern Int -> Pattern Double -> ControlPattern
slider36bus busid pat = (pF "slider36" pat) # (pI "^slider36" busid)

-- | 
slider35 :: Pattern Double -> ControlPattern
slider35 = pF "slider35"
slider35bus :: Pattern Int -> Pattern Double -> ControlPattern
slider35bus busid pat = (pF "slider35" pat) # (pI "^slider35" busid)

-- | 
slider34 :: Pattern Double -> ControlPattern
slider34 = pF "slider34"
slider34bus :: Pattern Int -> Pattern Double -> ControlPattern
slider34bus busid pat = (pF "slider34" pat) # (pI "^slider34" busid)

-- | 
slider33 :: Pattern Double -> ControlPattern
slider33 = pF "slider33"
slider33bus :: Pattern Int -> Pattern Double -> ControlPattern
slider33bus busid pat = (pF "slider33" pat) # (pI "^slider33" busid)

-- | 
slider32 :: Pattern Double -> ControlPattern
slider32 = pF "slider32"
slider32bus :: Pattern Int -> Pattern Double -> ControlPattern
slider32bus busid pat = (pF "slider32" pat) # (pI "^slider32" busid)

-- | 
slider31 :: Pattern Double -> ControlPattern
slider31 = pF "slider31"
slider31bus :: Pattern Int -> Pattern Double -> ControlPattern
slider31bus busid pat = (pF "slider31" pat) # (pI "^slider31" busid)

-- | 
slider30 :: Pattern Double -> ControlPattern
slider30 = pF "slider30"
slider30bus :: Pattern Int -> Pattern Double -> ControlPattern
slider30bus busid pat = (pF "slider30" pat) # (pI "^slider30" busid)

-- | 
slider3 :: Pattern Double -> ControlPattern
slider3 = pF "slider3"
slider3bus :: Pattern Int -> Pattern Double -> ControlPattern
slider3bus busid pat = (pF "slider3" pat) # (pI "^slider3" busid)

-- | 
slider29 :: Pattern Double -> ControlPattern
slider29 = pF "slider29"
slider29bus :: Pattern Int -> Pattern Double -> ControlPattern
slider29bus busid pat = (pF "slider29" pat) # (pI "^slider29" busid)

-- | 
slider28 :: Pattern Double -> ControlPattern
slider28 = pF "slider28"
slider28bus :: Pattern Int -> Pattern Double -> ControlPattern
slider28bus busid pat = (pF "slider28" pat) # (pI "^slider28" busid)

-- | 
slider27 :: Pattern Double -> ControlPattern
slider27 = pF "slider27"
slider27bus :: Pattern Int -> Pattern Double -> ControlPattern
slider27bus busid pat = (pF "slider27" pat) # (pI "^slider27" busid)

-- | 
slider26 :: Pattern Double -> ControlPattern
slider26 = pF "slider26"
slider26bus :: Pattern Int -> Pattern Double -> ControlPattern
slider26bus busid pat = (pF "slider26" pat) # (pI "^slider26" busid)

-- | 
slider25 :: Pattern Double -> ControlPattern
slider25 = pF "slider25"
slider25bus :: Pattern Int -> Pattern Double -> ControlPattern
slider25bus busid pat = (pF "slider25" pat) # (pI "^slider25" busid)

-- | 
slider24 :: Pattern Double -> ControlPattern
slider24 = pF "slider24"
slider24bus :: Pattern Int -> Pattern Double -> ControlPattern
slider24bus busid pat = (pF "slider24" pat) # (pI "^slider24" busid)

-- | 
slider23 :: Pattern Double -> ControlPattern
slider23 = pF "slider23"
slider23bus :: Pattern Int -> Pattern Double -> ControlPattern
slider23bus busid pat = (pF "slider23" pat) # (pI "^slider23" busid)

-- | 
slider22 :: Pattern Double -> ControlPattern
slider22 = pF "slider22"
slider22bus :: Pattern Int -> Pattern Double -> ControlPattern
slider22bus busid pat = (pF "slider22" pat) # (pI "^slider22" busid)

-- | 
slider21 :: Pattern Double -> ControlPattern
slider21 = pF "slider21"
slider21bus :: Pattern Int -> Pattern Double -> ControlPattern
slider21bus busid pat = (pF "slider21" pat) # (pI "^slider21" busid)

-- | 
slider20 :: Pattern Double -> ControlPattern
slider20 = pF "slider20"
slider20bus :: Pattern Int -> Pattern Double -> ControlPattern
slider20bus busid pat = (pF "slider20" pat) # (pI "^slider20" busid)

-- | 
slider2 :: Pattern Double -> ControlPattern
slider2 = pF "slider2"
slider2bus :: Pattern Int -> Pattern Double -> ControlPattern
slider2bus busid pat = (pF "slider2" pat) # (pI "^slider2" busid)

-- | 
slider19 :: Pattern Double -> ControlPattern
slider19 = pF "slider19"
slider19bus :: Pattern Int -> Pattern Double -> ControlPattern
slider19bus busid pat = (pF "slider19" pat) # (pI "^slider19" busid)

-- | 
slider18 :: Pattern Double -> ControlPattern
slider18 = pF "slider18"
slider18bus :: Pattern Int -> Pattern Double -> ControlPattern
slider18bus busid pat = (pF "slider18" pat) # (pI "^slider18" busid)

-- | 
slider17 :: Pattern Double -> ControlPattern
slider17 = pF "slider17"
slider17bus :: Pattern Int -> Pattern Double -> ControlPattern
slider17bus busid pat = (pF "slider17" pat) # (pI "^slider17" busid)

-- | 
slider16 :: Pattern Double -> ControlPattern
slider16 = pF "slider16"
slider16bus :: Pattern Int -> Pattern Double -> ControlPattern
slider16bus busid pat = (pF "slider16" pat) # (pI "^slider16" busid)

-- | 
slider15 :: Pattern Double -> ControlPattern
slider15 = pF "slider15"
slider15bus :: Pattern Int -> Pattern Double -> ControlPattern
slider15bus busid pat = (pF "slider15" pat) # (pI "^slider15" busid)

-- | 
slider14 :: Pattern Double -> ControlPattern
slider14 = pF "slider14"
slider14bus :: Pattern Int -> Pattern Double -> ControlPattern
slider14bus busid pat = (pF "slider14" pat) # (pI "^slider14" busid)

-- | 
slider13 :: Pattern Double -> ControlPattern
slider13 = pF "slider13"
slider13bus :: Pattern Int -> Pattern Double -> ControlPattern
slider13bus busid pat = (pF "slider13" pat) # (pI "^slider13" busid)

-- | 
slider12 :: Pattern Double -> ControlPattern
slider12 = pF "slider12"
slider12bus :: Pattern Int -> Pattern Double -> ControlPattern
slider12bus busid pat = (pF "slider12" pat) # (pI "^slider12" busid)

-- | 
slider11 :: Pattern Double -> ControlPattern
slider11 = pF "slider11"
slider11bus :: Pattern Int -> Pattern Double -> ControlPattern
slider11bus busid pat = (pF "slider11" pat) # (pI "^slider11" busid)

-- | 
slider10 :: Pattern Double -> ControlPattern
slider10 = pF "slider10"
slider10bus :: Pattern Int -> Pattern Double -> ControlPattern
slider10bus busid pat = (pF "slider10" pat) # (pI "^slider10" busid)

-- | 
slider1 :: Pattern Double -> ControlPattern
slider1 = pF "slider1"
slider1bus :: Pattern Int -> Pattern Double -> ControlPattern
slider1bus busid pat = (pF "slider1" pat) # (pI "^slider1" busid)

-- | 
slider0 :: Pattern Double -> ControlPattern
slider0 = pF "slider0"
slider0bus :: Pattern Int -> Pattern Double -> ControlPattern
slider0bus busid pat = (pF "slider0" pat) # (pI "^slider0" busid)

-- | 
slide :: Pattern Double -> ControlPattern
slide = pF "slide"
slidebus :: Pattern Int -> Pattern Double -> ControlPattern
slidebus busid pat = (pF "slide" pat) # (pI "^slide" busid)

-- | a pattern of numbers from 0 to 1. Sets the perceptual size (reverb time) of the `room` to be used in reverb.
size :: Pattern Double -> ControlPattern
size = pF "size"
sizebus :: Pattern Int -> Pattern Double -> ControlPattern
sizebus busid pat = (pF "size" pat) # (pI "^size" busid)

-- | wave shaping distortion, a pattern of numbers from 0 for no distortion up to 1 for loads of distortion.
shape :: Pattern Double -> ControlPattern
shape = pF "shape"
shapebus :: Pattern Int -> Pattern Double -> ControlPattern
shapebus busid pat = (pF "shape" pat) # (pI "^shape" busid)

-- | 
semitone :: Pattern Double -> ControlPattern
semitone = pF "semitone"
semitonebus :: Pattern Int -> Pattern Double -> ControlPattern
semitonebus busid pat = (pF "semitone" pat) # (pI "^semitone" busid)

-- | 
seconds :: Pattern Double -> ControlPattern
seconds = pF "seconds"
secondsbus :: Pattern Int -> Pattern Double -> ControlPattern
secondsbus busid pat = (pF "seconds" pat) # (pI "^seconds" busid)

-- | 
scrash :: Pattern Double -> ControlPattern
scrash = pF "scrash"
scrashbus :: Pattern Int -> Pattern Double -> ControlPattern
scrashbus busid pat = (pF "scrash" pat) # (pI "^scrash" busid)

-- | Spectral scramble
scram :: Pattern Double -> ControlPattern
scram = pF "scram"
scrambus :: Pattern Int -> Pattern Double -> ControlPattern
scrambus busid pat = (pF "scram" pat) # (pI "^scram" busid)

-- | 
sclaves :: Pattern Double -> ControlPattern
sclaves = pF "sclaves"
sclavesbus :: Pattern Int -> Pattern Double -> ControlPattern
sclavesbus busid pat = (pF "sclaves" pat) # (pI "^sclaves" busid)

-- | 
sclap :: Pattern Double -> ControlPattern
sclap = pF "sclap"
sclapbus :: Pattern Int -> Pattern Double -> ControlPattern
sclapbus busid pat = (pF "sclap" pat) # (pI "^sclap" busid)

-- | 
sagogo :: Pattern Double -> ControlPattern
sagogo = pF "sagogo"
sagogobus :: Pattern Int -> Pattern Double -> ControlPattern
sagogobus busid pat = (pF "sagogo" pat) # (pI "^sagogo" busid)

-- | a pattern of numbers from 0 to 1. Sets the level of reverb.
room :: Pattern Double -> ControlPattern
room = pF "room"
roombus :: Pattern Int -> Pattern Double -> ControlPattern
roombus busid pat = (pF "room" pat) # (pI "^room" busid)

-- | ring modulation
ringf :: Pattern Double -> ControlPattern
ringf = pF "ringf"
ringfbus :: Pattern Int -> Pattern Double -> ControlPattern
ringfbus busid pat = (pF "ringf" pat) # (pI "^ringf" busid)

-- | ring modulation
ringdf :: Pattern Double -> ControlPattern
ringdf = pF "ringdf"
ringdfbus :: Pattern Int -> Pattern Double -> ControlPattern
ringdfbus busid pat = (pF "ringdf" pat) # (pI "^ringdf" busid)

-- | ring modulation
ring :: Pattern Double -> ControlPattern
ring = pF "ring"
ringbus :: Pattern Int -> Pattern Double -> ControlPattern
ringbus busid pat = (pF "ring" pat) # (pI "^ring" busid)

-- | a pattern of numbers from 0 to 1. Specifies the resonance of the low-pass filter.
resonance :: Pattern Double -> ControlPattern
resonance = pF "resonance"
resonancebus :: Pattern Int -> Pattern Double -> ControlPattern
resonancebus busid pat = (pF "resonance" pat) # (pI "^resonance" busid)

-- | a pattern of numbers to specify the release time (in seconds) of an envelope applied to each sample. Only takes effect if `attack` is also specified.
release :: Pattern Double -> ControlPattern
release = pF "release"
releasebus :: Pattern Int -> Pattern Double -> ControlPattern
releasebus busid pat = (pF "release" pat) # (pI "^release" busid)

-- | Spectral conform
real :: Pattern Double -> ControlPattern
real = pF "real"
realbus :: Pattern Int -> Pattern Double -> ControlPattern
realbus busid pat = (pF "real" pat) # (pI "^real" busid)

-- | used in SuperDirt softsynths as a control rate or 'speed'
rate :: Pattern Double -> ControlPattern
rate = pF "rate"
ratebus :: Pattern Int -> Pattern Double -> ControlPattern
ratebus busid pat = (pF "rate" pat) # (pI "^rate" busid)

-- | 
progNum :: Pattern Double -> ControlPattern
progNum = pF "progNum"
progNumbus :: Pattern Int -> Pattern Double -> ControlPattern
progNumbus busid pat = (pF "progNum" pat) # (pI "^progNum" busid)

-- | 
portamento :: Pattern Double -> ControlPattern
portamento = pF "portamento"
portamentobus :: Pattern Int -> Pattern Double -> ControlPattern
portamentobus busid pat = (pF "portamento" pat) # (pI "^portamento" busid)

-- | 
polyTouch :: Pattern Double -> ControlPattern
polyTouch = pF "polyTouch"
polyTouchbus :: Pattern Int -> Pattern Double -> ControlPattern
polyTouchbus busid pat = (pF "polyTouch" pat) # (pI "^polyTouch" busid)

-- | 
pitch3 :: Pattern Double -> ControlPattern
pitch3 = pF "pitch3"
pitch3bus :: Pattern Int -> Pattern Double -> ControlPattern
pitch3bus busid pat = (pF "pitch3" pat) # (pI "^pitch3" busid)

-- | 
pitch2 :: Pattern Double -> ControlPattern
pitch2 = pF "pitch2"
pitch2bus :: Pattern Int -> Pattern Double -> ControlPattern
pitch2bus busid pat = (pF "pitch2" pat) # (pI "^pitch2" busid)

-- | 
pitch1 :: Pattern Double -> ControlPattern
pitch1 = pF "pitch1"
pitch1bus :: Pattern Int -> Pattern Double -> ControlPattern
pitch1bus busid pat = (pF "pitch1" pat) # (pI "^pitch1" busid)

-- | Phaser Audio DSP effect | params are 'phaserrate' and 'phaserdepth'
phaserrate :: Pattern Double -> ControlPattern
phaserrate = pF "phaserrate"
phaserratebus :: Pattern Int -> Pattern Double -> ControlPattern
phaserratebus busid pat = (pF "phaserrate" pat) # (pI "^phaserrate" busid)

-- | Phaser Audio DSP effect | params are 'phaserrate' and 'phaserdepth'
phaserdepth :: Pattern Double -> ControlPattern
phaserdepth = pF "phaserdepth"
phaserdepthbus :: Pattern Int -> Pattern Double -> ControlPattern
phaserdepthbus busid pat = (pF "phaserdepth" pat) # (pI "^phaserdepth" busid)

-- | 
partials :: Pattern Double -> ControlPattern
partials = pF "partials"
partialsbus :: Pattern Int -> Pattern Double -> ControlPattern
partialsbus busid pat = (pF "partials" pat) # (pI "^partials" busid)

-- | a pattern of numbers between 0.0 and inf, which controls how much each channel is distributed over neighbours (multichannel only)
panwidth :: Pattern Double -> ControlPattern
panwidth = pF "panwidth"
panwidthbus :: Pattern Int -> Pattern Double -> ControlPattern
panwidthbus busid pat = (pF "panwidth" pat) # (pI "^panwidth" busid)

-- | a pattern of numbers between 0.0 and 1.0, which controls the multichannel spread range (multichannel only)
pansplay :: Pattern Double -> ControlPattern
pansplay = pF "pansplay"
pansplaybus :: Pattern Int -> Pattern Double -> ControlPattern
pansplaybus busid pat = (pF "pansplay" pat) # (pI "^pansplay" busid)

-- | a pattern of numbers between -inf and inf, which controls how much multichannel output is fanned out (negative is backwards ordering)
panspan :: Pattern Double -> ControlPattern
panspan = pF "panspan"
panspanbus :: Pattern Int -> Pattern Double -> ControlPattern
panspanbus busid pat = (pF "panspan" pat) # (pI "^panspan" busid)

-- | a pattern of numbers between -1.0 and 1.0, which controls the relative position of the centre pan in a pair of adjacent speakers (multichannel only)
panorient :: Pattern Double -> ControlPattern
panorient = pF "panorient"
panorientbus :: Pattern Int -> Pattern Double -> ControlPattern
panorientbus busid pat = (pF "panorient" pat) # (pI "^panorient" busid)

-- | a pattern of numbers between 0 and 1, from left to right (assuming stereo), once round a circle (assuming multichannel)
pan :: Pattern Double -> ControlPattern
pan = pF "pan"
panbus :: Pattern Int -> Pattern Double -> ControlPattern
panbus busid pat = (pF "pan" pat) # (pI "^pan" busid)

-- | 
overshape :: Pattern Double -> ControlPattern
overshape = pF "overshape"
overshapebus :: Pattern Int -> Pattern Double -> ControlPattern
overshapebus busid pat = (pF "overshape" pat) # (pI "^overshape" busid)

-- | 
overgain :: Pattern Double -> ControlPattern
overgain = pF "overgain"
overgainbus :: Pattern Int -> Pattern Double -> ControlPattern
overgainbus _ _ = error $ "Control parameter 'overgain' can't be sent to a bus."

-- | a pattern of numbers. An `orbit` is a global parameter context for patterns. Patterns with the same orbit will share hardware output bus offset and global effects, e.g. reverb and delay. The maximum number of orbits is specified in the superdirt startup, numbers higher than maximum will wrap around.
orbit :: Pattern Int -> ControlPattern
orbit = pI "orbit"
orbitbus :: Pattern Int -> Pattern Int -> ControlPattern
orbitbus busid pat = (pI "orbit" pat) # (pI "^orbit" busid)

-- | 
ophatdecay :: Pattern Double -> ControlPattern
ophatdecay = pF "ophatdecay"
ophatdecaybus :: Pattern Int -> Pattern Double -> ControlPattern
ophatdecaybus busid pat = (pF "ophatdecay" pat) # (pI "^ophatdecay" busid)

-- | 
offset :: Pattern Double -> ControlPattern
offset = pF "offset"
offsetbus :: Pattern Int -> Pattern Double -> ControlPattern
offsetbus _ _ = error $ "Control parameter 'offset' can't be sent to a bus."

-- | octaver effect
octersubsub :: Pattern Double -> ControlPattern
octersubsub = pF "octersubsub"
octersubsubbus :: Pattern Int -> Pattern Double -> ControlPattern
octersubsubbus busid pat = (pF "octersubsub" pat) # (pI "^octersubsub" busid)

-- | octaver effect
octersub :: Pattern Double -> ControlPattern
octersub = pF "octersub"
octersubbus :: Pattern Int -> Pattern Double -> ControlPattern
octersubbus busid pat = (pF "octersub" pat) # (pI "^octersub" busid)

-- | octaver effect
octer :: Pattern Double -> ControlPattern
octer = pF "octer"
octerbus :: Pattern Int -> Pattern Double -> ControlPattern
octerbus busid pat = (pF "octer" pat) # (pI "^octer" busid)

-- | 
octaveR :: Pattern Double -> ControlPattern
octaveR = pF "octaveR"
octaveRbus :: Pattern Int -> Pattern Double -> ControlPattern
octaveRbus busid pat = (pF "octaveR" pat) # (pI "^octaveR" busid)

-- | 
octave :: Pattern Int -> ControlPattern
octave = pI "octave"
octavebus :: Pattern Int -> Pattern Int -> ControlPattern
octavebus _ _ = error $ "Control parameter 'octave' can't be sent to a bus."

-- | Nudges events into the future by the specified number of seconds. Negative numbers work up to a point as well (due to internal latency)
nudge :: Pattern Double -> ControlPattern
nudge = pF "nudge"
nudgebus :: Pattern Int -> Pattern Double -> ControlPattern
nudgebus busid pat = (pF "nudge" pat) # (pI "^nudge" busid)

-- | 
nrpnv :: Pattern Int -> ControlPattern
nrpnv = pI "nrpnv"
nrpnvbus :: Pattern Int -> Pattern Int -> ControlPattern
nrpnvbus busid pat = (pI "nrpnv" pat) # (pI "^nrpnv" busid)

-- | 
nrpnn :: Pattern Int -> ControlPattern
nrpnn = pI "nrpnn"
nrpnnbus :: Pattern Int -> Pattern Int -> ControlPattern
nrpnnbus busid pat = (pI "nrpnn" pat) # (pI "^nrpnn" busid)

-- | The note or pitch to play a sound or synth with
note :: Pattern Note -> ControlPattern
note = pN "note"
notebus :: Pattern Int -> Pattern Note -> ControlPattern
notebus _ _ = error $ "Control parameter 'note' can't be sent to a bus."

-- | The note or sample number to choose for a synth or sampleset
n :: Pattern Note -> ControlPattern
n = pN "n"
nbus :: Pattern Int -> Pattern Note -> ControlPattern
nbus _ _ = error $ "Control parameter 'n' can't be sent to a bus."

-- | 
mtranspose :: Pattern Double -> ControlPattern
mtranspose = pF "mtranspose"
mtransposebus :: Pattern Int -> Pattern Double -> ControlPattern
mtransposebus busid pat = (pF "mtranspose" pat) # (pI "^mtranspose" busid)

-- | 
modwheel :: Pattern Double -> ControlPattern
modwheel = pF "modwheel"
modwheelbus :: Pattern Int -> Pattern Double -> ControlPattern
modwheelbus busid pat = (pF "modwheel" pat) # (pI "^modwheel" busid)

-- | 
minutes :: Pattern Double -> ControlPattern
minutes = pF "minutes"
minutesbus :: Pattern Int -> Pattern Double -> ControlPattern
minutesbus busid pat = (pF "minutes" pat) # (pI "^minutes" busid)

-- | 
miditouch :: Pattern Double -> ControlPattern
miditouch = pF "miditouch"
miditouchbus :: Pattern Int -> Pattern Double -> ControlPattern
miditouchbus busid pat = (pF "miditouch" pat) # (pI "^miditouch" busid)

-- | 
midicmd :: Pattern String -> ControlPattern
midicmd = pS "midicmd"
midicmdbus :: Pattern Int -> Pattern String -> ControlPattern
midicmdbus busid pat = (pS "midicmd" pat) # (pI "^midicmd" busid)

-- | 
midichan :: Pattern Double -> ControlPattern
midichan = pF "midichan"
midichanbus :: Pattern Int -> Pattern Double -> ControlPattern
midichanbus busid pat = (pF "midichan" pat) # (pI "^midichan" busid)

-- | 
midibend :: Pattern Double -> ControlPattern
midibend = pF "midibend"
midibendbus :: Pattern Int -> Pattern Double -> ControlPattern
midibendbus busid pat = (pF "midibend" pat) # (pI "^midibend" busid)

-- | 
lsnare :: Pattern Double -> ControlPattern
lsnare = pF "lsnare"
lsnarebus :: Pattern Int -> Pattern Double -> ControlPattern
lsnarebus busid pat = (pF "lsnare" pat) # (pI "^lsnare" busid)

-- | 
lsize :: Pattern Double -> ControlPattern
lsize = pF "lsize"
lsizebus :: Pattern Int -> Pattern Double -> ControlPattern
lsizebus busid pat = (pF "lsize" pat) # (pI "^lsize" busid)

-- | 
lrate :: Pattern Double -> ControlPattern
lrate = pF "lrate"
lratebus :: Pattern Int -> Pattern Double -> ControlPattern
lratebus busid pat = (pF "lrate" pat) # (pI "^lrate" busid)

-- | 
lophat :: Pattern Double -> ControlPattern
lophat = pF "lophat"
lophatbus :: Pattern Int -> Pattern Double -> ControlPattern
lophatbus busid pat = (pF "lophat" pat) # (pI "^lophat" busid)

-- | loops the sample (from `begin` to `end`) the specified number of times.
loop :: Pattern Double -> ControlPattern
loop = pF "loop"
loopbus :: Pattern Int -> Pattern Double -> ControlPattern
loopbus _ _ = error $ "Control parameter 'loop' can't be sent to a bus."

-- | A pattern of numbers. Specifies whether delaytime is calculated relative to cps. When set to 1, delaytime is a direct multiple of a cycle.
lock :: Pattern Double -> ControlPattern
lock = pF "lock"
lockbus :: Pattern Int -> Pattern Double -> ControlPattern
lockbus busid pat = (pF "lock" pat) # (pI "^lock" busid)

-- | 
llotom :: Pattern Double -> ControlPattern
llotom = pF "llotom"
llotombus :: Pattern Int -> Pattern Double -> ControlPattern
llotombus busid pat = (pF "llotom" pat) # (pI "^llotom" busid)

-- | 
lkick :: Pattern Double -> ControlPattern
lkick = pF "lkick"
lkickbus :: Pattern Int -> Pattern Double -> ControlPattern
lkickbus busid pat = (pF "lkick" pat) # (pI "^lkick" busid)

-- | 
lhitom :: Pattern Double -> ControlPattern
lhitom = pF "lhitom"
lhitombus :: Pattern Int -> Pattern Double -> ControlPattern
lhitombus busid pat = (pF "lhitom" pat) # (pI "^lhitom" busid)

-- | 
lfosync :: Pattern Double -> ControlPattern
lfosync = pF "lfosync"
lfosyncbus :: Pattern Int -> Pattern Double -> ControlPattern
lfosyncbus busid pat = (pF "lfosync" pat) # (pI "^lfosync" busid)

-- | 
lfoshape :: Pattern Double -> ControlPattern
lfoshape = pF "lfoshape"
lfoshapebus :: Pattern Int -> Pattern Double -> ControlPattern
lfoshapebus busid pat = (pF "lfoshape" pat) # (pI "^lfoshape" busid)

-- | 
lfopitchint :: Pattern Double -> ControlPattern
lfopitchint = pF "lfopitchint"
lfopitchintbus :: Pattern Int -> Pattern Double -> ControlPattern
lfopitchintbus busid pat = (pF "lfopitchint" pat) # (pI "^lfopitchint" busid)

-- | 
lfoint :: Pattern Double -> ControlPattern
lfoint = pF "lfoint"
lfointbus :: Pattern Int -> Pattern Double -> ControlPattern
lfointbus busid pat = (pF "lfoint" pat) # (pI "^lfoint" busid)

-- | 
lfodelay :: Pattern Double -> ControlPattern
lfodelay = pF "lfodelay"
lfodelaybus :: Pattern Int -> Pattern Double -> ControlPattern
lfodelaybus busid pat = (pF "lfodelay" pat) # (pI "^lfodelay" busid)

-- | 
lfocutoffint :: Pattern Double -> ControlPattern
lfocutoffint = pF "lfocutoffint"
lfocutoffintbus :: Pattern Int -> Pattern Double -> ControlPattern
lfocutoffintbus busid pat = (pF "lfocutoffint" pat) # (pI "^lfocutoffint" busid)

-- | 
lfo :: Pattern Double -> ControlPattern
lfo = pF "lfo"
lfobus :: Pattern Int -> Pattern Double -> ControlPattern
lfobus busid pat = (pF "lfo" pat) # (pI "^lfo" busid)

-- | 
leslie :: Pattern Double -> ControlPattern
leslie = pF "leslie"
lesliebus :: Pattern Int -> Pattern Double -> ControlPattern
lesliebus busid pat = (pF "leslie" pat) # (pI "^leslie" busid)

-- | controls the amount of overlap between two adjacent sounds
legato :: Pattern Double -> ControlPattern
legato = pF "legato"
legatobus :: Pattern Int -> Pattern Double -> ControlPattern
legatobus _ _ = error $ "Control parameter 'legato' can't be sent to a bus."

-- | 
lcrash :: Pattern Double -> ControlPattern
lcrash = pF "lcrash"
lcrashbus :: Pattern Int -> Pattern Double -> ControlPattern
lcrashbus busid pat = (pF "lcrash" pat) # (pI "^lcrash" busid)

-- | 
lclhat :: Pattern Double -> ControlPattern
lclhat = pF "lclhat"
lclhatbus :: Pattern Int -> Pattern Double -> ControlPattern
lclhatbus busid pat = (pF "lclhat" pat) # (pI "^lclhat" busid)

-- | 
lclaves :: Pattern Double -> ControlPattern
lclaves = pF "lclaves"
lclavesbus :: Pattern Int -> Pattern Double -> ControlPattern
lclavesbus busid pat = (pF "lclaves" pat) # (pI "^lclaves" busid)

-- | 
lclap :: Pattern Double -> ControlPattern
lclap = pF "lclap"
lclapbus :: Pattern Int -> Pattern Double -> ControlPattern
lclapbus busid pat = (pF "lclap" pat) # (pI "^lclap" busid)

-- | Low pass sort of spectral filter
lbrick :: Pattern Double -> ControlPattern
lbrick = pF "lbrick"
lbrickbus :: Pattern Int -> Pattern Double -> ControlPattern
lbrickbus busid pat = (pF "lbrick" pat) # (pI "^lbrick" busid)

-- | 
lagogo :: Pattern Double -> ControlPattern
lagogo = pF "lagogo"
lagogobus :: Pattern Int -> Pattern Double -> ControlPattern
lagogobus busid pat = (pF "lagogo" pat) # (pI "^lagogo" busid)

-- | shape/bass enhancer
krush :: Pattern Double -> ControlPattern
krush = pF "krush"
krushbus :: Pattern Int -> Pattern Double -> ControlPattern
krushbus busid pat = (pF "krush" pat) # (pI "^krush" busid)

-- | 
kcutoff :: Pattern Double -> ControlPattern
kcutoff = pF "kcutoff"
kcutoffbus :: Pattern Int -> Pattern Double -> ControlPattern
kcutoffbus busid pat = (pF "kcutoff" pat) # (pI "^kcutoff" busid)

-- | 
imag :: Pattern Double -> ControlPattern
imag = pF "imag"
imagbus :: Pattern Int -> Pattern Double -> ControlPattern
imagbus busid pat = (pF "imag" pat) # (pI "^imag" busid)

-- | a pattern of numbers from 0 to 1. Applies the resonance of the high-pass filter. Has alias @hpq@
hresonance :: Pattern Double -> ControlPattern
hresonance = pF "hresonance"
hresonancebus :: Pattern Int -> Pattern Double -> ControlPattern
hresonancebus busid pat = (pF "hresonance" pat) # (pI "^hresonance" busid)

-- | 
hours :: Pattern Double -> ControlPattern
hours = pF "hours"
hoursbus :: Pattern Int -> Pattern Double -> ControlPattern
hoursbus busid pat = (pF "hours" pat) # (pI "^hours" busid)

-- | a pattern of numbers to specify the hold time (in seconds) of an envelope applied to each sample. Only takes effect if `attack` and `release` are also specified.
hold :: Pattern Double -> ControlPattern
hold = pF "hold"
holdbus :: Pattern Int -> Pattern Double -> ControlPattern
holdbus busid pat = (pF "hold" pat) # (pI "^hold" busid)

-- | a pattern of numbers from 0 to 1. Applies the cutoff frequency of the high-pass filter. Also has alias @hpf@
hcutoff :: Pattern Double -> ControlPattern
hcutoff = pF "hcutoff"
hcutoffbus :: Pattern Int -> Pattern Double -> ControlPattern
hcutoffbus busid pat = (pF "hcutoff" pat) # (pI "^hcutoff" busid)

-- | High pass sort of spectral filter
hbrick :: Pattern Double -> ControlPattern
hbrick = pF "hbrick"
hbrickbus :: Pattern Int -> Pattern Double -> ControlPattern
hbrickbus busid pat = (pF "hbrick" pat) # (pI "^hbrick" busid)

-- | 
hatgrain :: Pattern Double -> ControlPattern
hatgrain = pF "hatgrain"
hatgrainbus :: Pattern Int -> Pattern Double -> ControlPattern
hatgrainbus busid pat = (pF "hatgrain" pat) # (pI "^hatgrain" busid)

-- | 
harmonic :: Pattern Double -> ControlPattern
harmonic = pF "harmonic"
harmonicbus :: Pattern Int -> Pattern Double -> ControlPattern
harmonicbus busid pat = (pF "harmonic" pat) # (pI "^harmonic" busid)

-- | 
gate :: Pattern Double -> ControlPattern
gate = pF "gate"
gatebus :: Pattern Int -> Pattern Double -> ControlPattern
gatebus busid pat = (pF "gate" pat) # (pI "^gate" busid)

-- | a pattern of numbers that specify volume. Values less than 1 make the sound quieter. Values greater than 1 make the sound louder. For the linear equivalent, see @amp@.
gain :: Pattern Double -> ControlPattern
gain = pF "gain"
gainbus :: Pattern Int -> Pattern Double -> ControlPattern
gainbus _ _ = error $ "Control parameter 'gain' can't be sent to a bus."

-- | frequency shifter
fshiftphase :: Pattern Double -> ControlPattern
fshiftphase = pF "fshiftphase"
fshiftphasebus :: Pattern Int -> Pattern Double -> ControlPattern
fshiftphasebus busid pat = (pF "fshiftphase" pat) # (pI "^fshiftphase" busid)

-- | frequency shifter
fshiftnote :: Pattern Double -> ControlPattern
fshiftnote = pF "fshiftnote"
fshiftnotebus :: Pattern Int -> Pattern Double -> ControlPattern
fshiftnotebus busid pat = (pF "fshiftnote" pat) # (pI "^fshiftnote" busid)

-- | frequency shifter
fshift :: Pattern Double -> ControlPattern
fshift = pF "fshift"
fshiftbus :: Pattern Int -> Pattern Double -> ControlPattern
fshiftbus busid pat = (pF "fshift" pat) # (pI "^fshift" busid)

-- | for internal sound routing
from :: Pattern Double -> ControlPattern
from = pF "from"
frombus :: Pattern Int -> Pattern Double -> ControlPattern
frombus busid pat = (pF "from" pat) # (pI "^from" busid)

-- | 
freq :: Pattern Double -> ControlPattern
freq = pF "freq"
freqbus :: Pattern Int -> Pattern Double -> ControlPattern
freqbus busid pat = (pF "freq" pat) # (pI "^freq" busid)

-- | Spectral freeze
freeze :: Pattern Double -> ControlPattern
freeze = pF "freeze"
freezebus :: Pattern Int -> Pattern Double -> ControlPattern
freezebus busid pat = (pF "freeze" pat) # (pI "^freeze" busid)

-- | 
frames :: Pattern Double -> ControlPattern
frames = pF "frames"
framesbus :: Pattern Int -> Pattern Double -> ControlPattern
framesbus busid pat = (pF "frames" pat) # (pI "^frames" busid)

-- | 
frameRate :: Pattern Double -> ControlPattern
frameRate = pF "frameRate"
frameRatebus :: Pattern Int -> Pattern Double -> ControlPattern
frameRatebus busid pat = (pF "frameRate" pat) # (pI "^frameRate" busid)

-- | 
expression :: Pattern Double -> ControlPattern
expression = pF "expression"
expressionbus :: Pattern Int -> Pattern Double -> ControlPattern
expressionbus busid pat = (pF "expression" pat) # (pI "^expression" busid)

-- | Spectral enhance
enhance :: Pattern Double -> ControlPattern
enhance = pF "enhance"
enhancebus :: Pattern Int -> Pattern Double -> ControlPattern
enhancebus busid pat = (pF "enhance" pat) # (pI "^enhance" busid)

-- | the same as `begin`, but cuts the end off samples, shortening them; e.g. `0.75` to cut off the last quarter of each sample.
end :: Pattern Double -> ControlPattern
end = pF "end"
endbus :: Pattern Int -> Pattern Double -> ControlPattern
endbus _ _ = error $ "Control parameter 'end' can't be sent to a bus."

-- | 
dur :: Pattern Double -> ControlPattern
dur = pF "dur"
durbus :: Pattern Int -> Pattern Double -> ControlPattern
durbus busid pat = (pF "dur" pat) # (pI "^dur" busid)

-- | when set to `1` will disable all reverb for this pattern. See `room` and `size` for more information about reverb.
dry :: Pattern Double -> ControlPattern
dry = pF "dry"
drybus :: Pattern Int -> Pattern Double -> ControlPattern
drybus busid pat = (pF "dry" pat) # (pI "^dry" busid)

-- | DJ filter, below 0.5 is low pass filter, above is high pass filter.
djf :: Pattern Double -> ControlPattern
djf = pF "djf"
djfbus :: Pattern Int -> Pattern Double -> ControlPattern
djfbus busid pat = (pF "djf" pat) # (pI "^djf" busid)

-- | noisy fuzzy distortion
distort :: Pattern Double -> ControlPattern
distort = pF "distort"
distortbus :: Pattern Int -> Pattern Double -> ControlPattern
distortbus busid pat = (pF "distort" pat) # (pI "^distort" busid)

-- | 
detune :: Pattern Double -> ControlPattern
detune = pF "detune"
detunebus :: Pattern Int -> Pattern Double -> ControlPattern
detunebus busid pat = (pF "detune" pat) # (pI "^detune" busid)

-- | a pattern of numbers from 0 to 1. Sets the length of the delay.
delaytime :: Pattern Double -> ControlPattern
delaytime = pF "delaytime"
delaytimebus :: Pattern Int -> Pattern Double -> ControlPattern
delaytimebus busid pat = (pF "delaytime" pat) # (pI "^delaytime" busid)

-- | a pattern of numbers from 0 to 1. Sets the amount of delay feedback.
delayfeedback :: Pattern Double -> ControlPattern
delayfeedback = pF "delayfeedback"
delayfeedbackbus :: Pattern Int -> Pattern Double -> ControlPattern
delayfeedbackbus busid pat = (pF "delayfeedback" pat) # (pI "^delayfeedback" busid)

-- | a pattern of numbers from 0 to 1. Sets the level of the delay signal.
delay :: Pattern Double -> ControlPattern
delay = pF "delay"
delaybus :: Pattern Int -> Pattern Double -> ControlPattern
delaybus busid pat = (pF "delay" pat) # (pI "^delay" busid)

-- | 
degree :: Pattern Double -> ControlPattern
degree = pF "degree"
degreebus :: Pattern Int -> Pattern Double -> ControlPattern
degreebus busid pat = (pF "degree" pat) # (pI "^degree" busid)

-- | 
decay :: Pattern Double -> ControlPattern
decay = pF "decay"
decaybus :: Pattern Int -> Pattern Double -> ControlPattern
decaybus busid pat = (pF "decay" pat) # (pI "^decay" busid)

-- | 
cutoffegint :: Pattern Double -> ControlPattern
cutoffegint = pF "cutoffegint"
cutoffegintbus :: Pattern Int -> Pattern Double -> ControlPattern
cutoffegintbus busid pat = (pF "cutoffegint" pat) # (pI "^cutoffegint" busid)

-- | a pattern of numbers from 0 to 1. Applies the cutoff frequency of the low-pass filter.
cutoff :: Pattern Double -> ControlPattern
cutoff = pF "cutoff"
cutoffbus :: Pattern Int -> Pattern Double -> ControlPattern
cutoffbus busid pat = (pF "cutoff" pat) # (pI "^cutoff" busid)

-- | In the style of classic drum-machines, `cut` will stop a playing sample as soon as another samples with in same cutgroup is to be played. An example would be an open hi-hat followed by a closed one, essentially muting the open.
cut :: Pattern Int -> ControlPattern
cut = pI "cut"
cutbus :: Pattern Int -> Pattern Int -> ControlPattern
cutbus busid pat = (pI "cut" pat) # (pI "^cut" busid)

-- | 
ctranspose :: Pattern Double -> ControlPattern
ctranspose = pF "ctranspose"
ctransposebus :: Pattern Int -> Pattern Double -> ControlPattern
ctransposebus busid pat = (pF "ctranspose" pat) # (pI "^ctranspose" busid)

-- | 
ctlNum :: Pattern Double -> ControlPattern
ctlNum = pF "ctlNum"
ctlNumbus :: Pattern Int -> Pattern Double -> ControlPattern
ctlNumbus busid pat = (pF "ctlNum" pat) # (pI "^ctlNum" busid)

-- | bit crushing, a pattern of numbers from 1 (for drastic reduction in bit-depth) to 16 (for barely no reduction).
crush :: Pattern Double -> ControlPattern
crush = pF "crush"
crushbus :: Pattern Int -> Pattern Double -> ControlPattern
crushbus busid pat = (pF "crush" pat) # (pI "^crush" busid)

-- | 
cps :: Pattern Double -> ControlPattern
cps = pF "cps"
cpsbus :: Pattern Int -> Pattern Double -> ControlPattern
cpsbus busid pat = (pF "cps" pat) # (pI "^cps" busid)

-- | 
control :: Pattern Double -> ControlPattern
control = pF "control"
controlbus :: Pattern Int -> Pattern Double -> ControlPattern
controlbus busid pat = (pF "control" pat) # (pI "^control" busid)

-- | Spectral comb
comb :: Pattern Double -> ControlPattern
comb = pF "comb"
combbus :: Pattern Int -> Pattern Double -> ControlPattern
combbus busid pat = (pF "comb" pat) # (pI "^comb" busid)

-- | fake-resampling, a pattern of numbers for lowering the sample rate, i.e. 1 for original 2 for half, 3 for a third and so on.
coarse :: Pattern Double -> ControlPattern
coarse = pF "coarse"
coarsebus :: Pattern Int -> Pattern Double -> ControlPattern
coarsebus busid pat = (pF "coarse" pat) # (pI "^coarse" busid)

-- | 
clhatdecay :: Pattern Double -> ControlPattern
clhatdecay = pF "clhatdecay"
clhatdecaybus :: Pattern Int -> Pattern Double -> ControlPattern
clhatdecaybus busid pat = (pF "clhatdecay" pat) # (pI "^clhatdecay" busid)

-- | choose the channel the pattern is sent to in superdirt
channel :: Pattern Int -> ControlPattern
channel = pI "channel"
channelbus :: Pattern Int -> Pattern Int -> ControlPattern
channelbus _ _ = error $ "Control parameter 'channel' can't be sent to a bus."

-- | 
ccv :: Pattern Double -> ControlPattern
ccv = pF "ccv"
ccvbus :: Pattern Int -> Pattern Double -> ControlPattern
ccvbus busid pat = (pF "ccv" pat) # (pI "^ccv" busid)

-- | 
ccn :: Pattern Double -> ControlPattern
ccn = pF "ccn"
ccnbus :: Pattern Int -> Pattern Double -> ControlPattern
ccnbus busid pat = (pF "ccn" pat) # (pI "^ccn" busid)

-- | 
button9 :: Pattern Double -> ControlPattern
button9 = pF "button9"
button9bus :: Pattern Int -> Pattern Double -> ControlPattern
button9bus busid pat = (pF "button9" pat) # (pI "^button9" busid)

-- | 
button8 :: Pattern Double -> ControlPattern
button8 = pF "button8"
button8bus :: Pattern Int -> Pattern Double -> ControlPattern
button8bus busid pat = (pF "button8" pat) # (pI "^button8" busid)

-- | 
button7 :: Pattern Double -> ControlPattern
button7 = pF "button7"
button7bus :: Pattern Int -> Pattern Double -> ControlPattern
button7bus busid pat = (pF "button7" pat) # (pI "^button7" busid)

-- | 
button63 :: Pattern Double -> ControlPattern
button63 = pF "button63"
button63bus :: Pattern Int -> Pattern Double -> ControlPattern
button63bus busid pat = (pF "button63" pat) # (pI "^button63" busid)

-- | 
button62 :: Pattern Double -> ControlPattern
button62 = pF "button62"
button62bus :: Pattern Int -> Pattern Double -> ControlPattern
button62bus busid pat = (pF "button62" pat) # (pI "^button62" busid)

-- | 
button61 :: Pattern Double -> ControlPattern
button61 = pF "button61"
button61bus :: Pattern Int -> Pattern Double -> ControlPattern
button61bus busid pat = (pF "button61" pat) # (pI "^button61" busid)

-- | 
button60 :: Pattern Double -> ControlPattern
button60 = pF "button60"
button60bus :: Pattern Int -> Pattern Double -> ControlPattern
button60bus busid pat = (pF "button60" pat) # (pI "^button60" busid)

-- | 
button6 :: Pattern Double -> ControlPattern
button6 = pF "button6"
button6bus :: Pattern Int -> Pattern Double -> ControlPattern
button6bus busid pat = (pF "button6" pat) # (pI "^button6" busid)

-- | 
button59 :: Pattern Double -> ControlPattern
button59 = pF "button59"
button59bus :: Pattern Int -> Pattern Double -> ControlPattern
button59bus busid pat = (pF "button59" pat) # (pI "^button59" busid)

-- | 
button58 :: Pattern Double -> ControlPattern
button58 = pF "button58"
button58bus :: Pattern Int -> Pattern Double -> ControlPattern
button58bus busid pat = (pF "button58" pat) # (pI "^button58" busid)

-- | 
button57 :: Pattern Double -> ControlPattern
button57 = pF "button57"
button57bus :: Pattern Int -> Pattern Double -> ControlPattern
button57bus busid pat = (pF "button57" pat) # (pI "^button57" busid)

-- | 
button56 :: Pattern Double -> ControlPattern
button56 = pF "button56"
button56bus :: Pattern Int -> Pattern Double -> ControlPattern
button56bus busid pat = (pF "button56" pat) # (pI "^button56" busid)

-- | 
button55 :: Pattern Double -> ControlPattern
button55 = pF "button55"
button55bus :: Pattern Int -> Pattern Double -> ControlPattern
button55bus busid pat = (pF "button55" pat) # (pI "^button55" busid)

-- | 
button54 :: Pattern Double -> ControlPattern
button54 = pF "button54"
button54bus :: Pattern Int -> Pattern Double -> ControlPattern
button54bus busid pat = (pF "button54" pat) # (pI "^button54" busid)

-- | 
button53 :: Pattern Double -> ControlPattern
button53 = pF "button53"
button53bus :: Pattern Int -> Pattern Double -> ControlPattern
button53bus busid pat = (pF "button53" pat) # (pI "^button53" busid)

-- | 
button52 :: Pattern Double -> ControlPattern
button52 = pF "button52"
button52bus :: Pattern Int -> Pattern Double -> ControlPattern
button52bus busid pat = (pF "button52" pat) # (pI "^button52" busid)

-- | 
button51 :: Pattern Double -> ControlPattern
button51 = pF "button51"
button51bus :: Pattern Int -> Pattern Double -> ControlPattern
button51bus busid pat = (pF "button51" pat) # (pI "^button51" busid)

-- | 
button50 :: Pattern Double -> ControlPattern
button50 = pF "button50"
button50bus :: Pattern Int -> Pattern Double -> ControlPattern
button50bus busid pat = (pF "button50" pat) # (pI "^button50" busid)

-- | 
button5 :: Pattern Double -> ControlPattern
button5 = pF "button5"
button5bus :: Pattern Int -> Pattern Double -> ControlPattern
button5bus busid pat = (pF "button5" pat) # (pI "^button5" busid)

-- | 
button49 :: Pattern Double -> ControlPattern
button49 = pF "button49"
button49bus :: Pattern Int -> Pattern Double -> ControlPattern
button49bus busid pat = (pF "button49" pat) # (pI "^button49" busid)

-- | 
button48 :: Pattern Double -> ControlPattern
button48 = pF "button48"
button48bus :: Pattern Int -> Pattern Double -> ControlPattern
button48bus busid pat = (pF "button48" pat) # (pI "^button48" busid)

-- | 
button47 :: Pattern Double -> ControlPattern
button47 = pF "button47"
button47bus :: Pattern Int -> Pattern Double -> ControlPattern
button47bus busid pat = (pF "button47" pat) # (pI "^button47" busid)

-- | 
button46 :: Pattern Double -> ControlPattern
button46 = pF "button46"
button46bus :: Pattern Int -> Pattern Double -> ControlPattern
button46bus busid pat = (pF "button46" pat) # (pI "^button46" busid)

-- | 
button45 :: Pattern Double -> ControlPattern
button45 = pF "button45"
button45bus :: Pattern Int -> Pattern Double -> ControlPattern
button45bus busid pat = (pF "button45" pat) # (pI "^button45" busid)

-- | 
button44 :: Pattern Double -> ControlPattern
button44 = pF "button44"
button44bus :: Pattern Int -> Pattern Double -> ControlPattern
button44bus busid pat = (pF "button44" pat) # (pI "^button44" busid)

-- | 
button43 :: Pattern Double -> ControlPattern
button43 = pF "button43"
button43bus :: Pattern Int -> Pattern Double -> ControlPattern
button43bus busid pat = (pF "button43" pat) # (pI "^button43" busid)

-- | 
button42 :: Pattern Double -> ControlPattern
button42 = pF "button42"
button42bus :: Pattern Int -> Pattern Double -> ControlPattern
button42bus busid pat = (pF "button42" pat) # (pI "^button42" busid)

-- | 
button41 :: Pattern Double -> ControlPattern
button41 = pF "button41"
button41bus :: Pattern Int -> Pattern Double -> ControlPattern
button41bus busid pat = (pF "button41" pat) # (pI "^button41" busid)

-- | 
button40 :: Pattern Double -> ControlPattern
button40 = pF "button40"
button40bus :: Pattern Int -> Pattern Double -> ControlPattern
button40bus busid pat = (pF "button40" pat) # (pI "^button40" busid)

-- | 
button4 :: Pattern Double -> ControlPattern
button4 = pF "button4"
button4bus :: Pattern Int -> Pattern Double -> ControlPattern
button4bus busid pat = (pF "button4" pat) # (pI "^button4" busid)

-- | 
button39 :: Pattern Double -> ControlPattern
button39 = pF "button39"
button39bus :: Pattern Int -> Pattern Double -> ControlPattern
button39bus busid pat = (pF "button39" pat) # (pI "^button39" busid)

-- | 
button38 :: Pattern Double -> ControlPattern
button38 = pF "button38"
button38bus :: Pattern Int -> Pattern Double -> ControlPattern
button38bus busid pat = (pF "button38" pat) # (pI "^button38" busid)

-- | 
button37 :: Pattern Double -> ControlPattern
button37 = pF "button37"
button37bus :: Pattern Int -> Pattern Double -> ControlPattern
button37bus busid pat = (pF "button37" pat) # (pI "^button37" busid)

-- | 
button36 :: Pattern Double -> ControlPattern
button36 = pF "button36"
button36bus :: Pattern Int -> Pattern Double -> ControlPattern
button36bus busid pat = (pF "button36" pat) # (pI "^button36" busid)

-- | 
button35 :: Pattern Double -> ControlPattern
button35 = pF "button35"
button35bus :: Pattern Int -> Pattern Double -> ControlPattern
button35bus busid pat = (pF "button35" pat) # (pI "^button35" busid)

-- | 
button34 :: Pattern Double -> ControlPattern
button34 = pF "button34"
button34bus :: Pattern Int -> Pattern Double -> ControlPattern
button34bus busid pat = (pF "button34" pat) # (pI "^button34" busid)

-- | 
button33 :: Pattern Double -> ControlPattern
button33 = pF "button33"
button33bus :: Pattern Int -> Pattern Double -> ControlPattern
button33bus busid pat = (pF "button33" pat) # (pI "^button33" busid)

-- | 
button32 :: Pattern Double -> ControlPattern
button32 = pF "button32"
button32bus :: Pattern Int -> Pattern Double -> ControlPattern
button32bus busid pat = (pF "button32" pat) # (pI "^button32" busid)

-- | 
button31 :: Pattern Double -> ControlPattern
button31 = pF "button31"
button31bus :: Pattern Int -> Pattern Double -> ControlPattern
button31bus busid pat = (pF "button31" pat) # (pI "^button31" busid)

-- | 
button30 :: Pattern Double -> ControlPattern
button30 = pF "button30"
button30bus :: Pattern Int -> Pattern Double -> ControlPattern
button30bus busid pat = (pF "button30" pat) # (pI "^button30" busid)

-- | 
button3 :: Pattern Double -> ControlPattern
button3 = pF "button3"
button3bus :: Pattern Int -> Pattern Double -> ControlPattern
button3bus busid pat = (pF "button3" pat) # (pI "^button3" busid)

-- | 
button29 :: Pattern Double -> ControlPattern
button29 = pF "button29"
button29bus :: Pattern Int -> Pattern Double -> ControlPattern
button29bus busid pat = (pF "button29" pat) # (pI "^button29" busid)

-- | 
button28 :: Pattern Double -> ControlPattern
button28 = pF "button28"
button28bus :: Pattern Int -> Pattern Double -> ControlPattern
button28bus busid pat = (pF "button28" pat) # (pI "^button28" busid)

-- | 
button27 :: Pattern Double -> ControlPattern
button27 = pF "button27"
button27bus :: Pattern Int -> Pattern Double -> ControlPattern
button27bus busid pat = (pF "button27" pat) # (pI "^button27" busid)

-- | 
button26 :: Pattern Double -> ControlPattern
button26 = pF "button26"
button26bus :: Pattern Int -> Pattern Double -> ControlPattern
button26bus busid pat = (pF "button26" pat) # (pI "^button26" busid)

-- | 
button25 :: Pattern Double -> ControlPattern
button25 = pF "button25"
button25bus :: Pattern Int -> Pattern Double -> ControlPattern
button25bus busid pat = (pF "button25" pat) # (pI "^button25" busid)

-- | 
button24 :: Pattern Double -> ControlPattern
button24 = pF "button24"
button24bus :: Pattern Int -> Pattern Double -> ControlPattern
button24bus busid pat = (pF "button24" pat) # (pI "^button24" busid)

-- | 
button23 :: Pattern Double -> ControlPattern
button23 = pF "button23"
button23bus :: Pattern Int -> Pattern Double -> ControlPattern
button23bus busid pat = (pF "button23" pat) # (pI "^button23" busid)

-- | 
button22 :: Pattern Double -> ControlPattern
button22 = pF "button22"
button22bus :: Pattern Int -> Pattern Double -> ControlPattern
button22bus busid pat = (pF "button22" pat) # (pI "^button22" busid)

-- | 
button21 :: Pattern Double -> ControlPattern
button21 = pF "button21"
button21bus :: Pattern Int -> Pattern Double -> ControlPattern
button21bus busid pat = (pF "button21" pat) # (pI "^button21" busid)

-- | 
button20 :: Pattern Double -> ControlPattern
button20 = pF "button20"
button20bus :: Pattern Int -> Pattern Double -> ControlPattern
button20bus busid pat = (pF "button20" pat) # (pI "^button20" busid)

-- | 
button2 :: Pattern Double -> ControlPattern
button2 = pF "button2"
button2bus :: Pattern Int -> Pattern Double -> ControlPattern
button2bus busid pat = (pF "button2" pat) # (pI "^button2" busid)

-- | 
button19 :: Pattern Double -> ControlPattern
button19 = pF "button19"
button19bus :: Pattern Int -> Pattern Double -> ControlPattern
button19bus busid pat = (pF "button19" pat) # (pI "^button19" busid)

-- | 
button18 :: Pattern Double -> ControlPattern
button18 = pF "button18"
button18bus :: Pattern Int -> Pattern Double -> ControlPattern
button18bus busid pat = (pF "button18" pat) # (pI "^button18" busid)

-- | 
button17 :: Pattern Double -> ControlPattern
button17 = pF "button17"
button17bus :: Pattern Int -> Pattern Double -> ControlPattern
button17bus busid pat = (pF "button17" pat) # (pI "^button17" busid)

-- | 
button16 :: Pattern Double -> ControlPattern
button16 = pF "button16"
button16bus :: Pattern Int -> Pattern Double -> ControlPattern
button16bus busid pat = (pF "button16" pat) # (pI "^button16" busid)

-- | 
button15 :: Pattern Double -> ControlPattern
button15 = pF "button15"
button15bus :: Pattern Int -> Pattern Double -> ControlPattern
button15bus busid pat = (pF "button15" pat) # (pI "^button15" busid)

-- | 
button14 :: Pattern Double -> ControlPattern
button14 = pF "button14"
button14bus :: Pattern Int -> Pattern Double -> ControlPattern
button14bus busid pat = (pF "button14" pat) # (pI "^button14" busid)

-- | 
button13 :: Pattern Double -> ControlPattern
button13 = pF "button13"
button13bus :: Pattern Int -> Pattern Double -> ControlPattern
button13bus busid pat = (pF "button13" pat) # (pI "^button13" busid)

-- | 
button12 :: Pattern Double -> ControlPattern
button12 = pF "button12"
button12bus :: Pattern Int -> Pattern Double -> ControlPattern
button12bus busid pat = (pF "button12" pat) # (pI "^button12" busid)

-- | 
button11 :: Pattern Double -> ControlPattern
button11 = pF "button11"
button11bus :: Pattern Int -> Pattern Double -> ControlPattern
button11bus busid pat = (pF "button11" pat) # (pI "^button11" busid)

-- | 
button10 :: Pattern Double -> ControlPattern
button10 = pF "button10"
button10bus :: Pattern Int -> Pattern Double -> ControlPattern
button10bus busid pat = (pF "button10" pat) # (pI "^button10" busid)

-- | 
button1 :: Pattern Double -> ControlPattern
button1 = pF "button1"
button1bus :: Pattern Int -> Pattern Double -> ControlPattern
button1bus busid pat = (pF "button1" pat) # (pI "^button1" busid)

-- | 
button0 :: Pattern Double -> ControlPattern
button0 = pF "button0"
button0bus :: Pattern Int -> Pattern Double -> ControlPattern
button0bus busid pat = (pF "button0" pat) # (pI "^button0" busid)

-- | Spectral binshift
binshift :: Pattern Double -> ControlPattern
binshift = pF "binshift"
binshiftbus :: Pattern Int -> Pattern Double -> ControlPattern
binshiftbus busid pat = (pF "binshift" pat) # (pI "^binshift" busid)

-- | a pattern of numbers from 0 to 1. Skips the beginning of each sample, e.g. `0.25` to cut off the first quarter from each sample.
begin :: Pattern Double -> ControlPattern
begin = pF "begin"
beginbus :: Pattern Int -> Pattern Double -> ControlPattern
beginbus busid pat = (pF "begin" pat) # (pI "^begin" busid)

-- | a pattern of anumbers from 0 to 1. Sets the q-factor of the band-pass filter.
bandq :: Pattern Double -> ControlPattern
bandq = pF "bandq"
bandqbus :: Pattern Int -> Pattern Double -> ControlPattern
bandqbus busid pat = (pF "bandq" pat) # (pI "^bandq" busid)

-- | a pattern of numbers from 0 to 1. Sets the center frequency of the band-pass filter.
bandf :: Pattern Double -> ControlPattern
bandf = pF "bandf"
bandfbus :: Pattern Int -> Pattern Double -> ControlPattern
bandfbus busid pat = (pF "bandf" pat) # (pI "^bandf" busid)

-- | a pattern of numbers to specify the attack time (in seconds) of an envelope applied to each sample. Only takes effect if `release` is also specified.
attack :: Pattern Double -> ControlPattern
attack = pF "attack"
attackbus :: Pattern Int -> Pattern Double -> ControlPattern
attackbus busid pat = (pF "attack" pat) # (pI "^attack" busid)

-- | 
array :: Pattern [Word8] -> ControlPattern
array = pX "array"
arraybus :: Pattern Int -> Pattern [Word8] -> ControlPattern
arraybus busid pat = (pX "array" pat) # (pI "^array" busid)

-- | like @gain@, but linear.
amp :: Pattern Double -> ControlPattern
amp = pF "amp"
ampbus :: Pattern Int -> Pattern Double -> ControlPattern
ampbus busid pat = (pF "amp" pat) # (pI "^amp" busid)

-- | a pattern of numbers that speed up (or slow down) samples while they play.
accelerate :: Pattern Double -> ControlPattern
accelerate = pF "accelerate"
acceleratebus :: Pattern Int -> Pattern Double -> ControlPattern
acceleratebus busid pat = (pF "accelerate" pat) # (pI "^accelerate" busid)



-- aliases

voi :: Pattern Double -> ControlPattern
voi = voice

vco :: Pattern Double -> ControlPattern
vco = vcoegint

vcf :: Pattern Double -> ControlPattern
vcf = vcfegint

up :: Pattern Note -> ControlPattern
up = note

tremr :: Pattern Double -> ControlPattern
tremr = tremolorate

tremdp :: Pattern Double -> ControlPattern
tremdp = tremolodepth

tdecay :: Pattern Double -> ControlPattern
tdecay = tomdecay

sz :: Pattern Double -> ControlPattern
sz = size

sus :: Pattern Double -> ControlPattern
sus = sustain

stt :: Pattern Double -> ControlPattern
stt = stuttertime

std :: Pattern Double -> ControlPattern
std = stutterdepth

sld :: Pattern Double -> ControlPattern
sld = slide

scr :: Pattern Double -> ControlPattern
scr = scrash

scp :: Pattern Double -> ControlPattern
scp = sclap

scl :: Pattern Double -> ControlPattern
scl = sclaves

sag :: Pattern Double -> ControlPattern
sag = sagogo

s :: Pattern String -> ControlPattern
s = sound

rel :: Pattern Double -> ControlPattern
rel = release

por :: Pattern Double -> ControlPattern
por = portamento

pit3 :: Pattern Double -> ControlPattern
pit3 = pitch3

pit2 :: Pattern Double -> ControlPattern
pit2 = pitch2

pit1 :: Pattern Double -> ControlPattern
pit1 = pitch1

phasr :: Pattern Double -> ControlPattern
phasr = phaserrate

phasdp :: Pattern Double -> ControlPattern
phasdp = phaserdepth

ohdecay :: Pattern Double -> ControlPattern
ohdecay = ophatdecay

lsn :: Pattern Double -> ControlPattern
lsn = lsnare

lpq :: Pattern Double -> ControlPattern
lpq = resonance

lpf :: Pattern Double -> ControlPattern
lpf = cutoff

loh :: Pattern Double -> ControlPattern
loh = lophat

llt :: Pattern Double -> ControlPattern
llt = llotom

lht :: Pattern Double -> ControlPattern
lht = lhitom

lfop :: Pattern Double -> ControlPattern
lfop = lfopitchint

lfoi :: Pattern Double -> ControlPattern
lfoi = lfoint

lfoc :: Pattern Double -> ControlPattern
lfoc = lfocutoffint

lcr :: Pattern Double -> ControlPattern
lcr = lcrash

lcp :: Pattern Double -> ControlPattern
lcp = lclap

lcl :: Pattern Double -> ControlPattern
lcl = lclaves

lch :: Pattern Double -> ControlPattern
lch = lclhat

lbd :: Pattern Double -> ControlPattern
lbd = lkick

lag :: Pattern Double -> ControlPattern
lag = lagogo

hpq :: Pattern Double -> ControlPattern
hpq = hresonance

hpf :: Pattern Double -> ControlPattern
hpf = hcutoff

hg :: Pattern Double -> ControlPattern
hg = hatgrain

gat :: Pattern Double -> ControlPattern
gat = gate

dt :: Pattern Double -> ControlPattern
dt = delaytime

dfb :: Pattern Double -> ControlPattern
dfb = delayfeedback

det :: Pattern Double -> ControlPattern
det = detune

delayt :: Pattern Double -> ControlPattern
delayt = delaytime

delayfb :: Pattern Double -> ControlPattern
delayfb = delayfeedback

ctfg :: Pattern Double -> ControlPattern
ctfg = cutoffegint

ctf :: Pattern Double -> ControlPattern
ctf = cutoff

chdecay :: Pattern Double -> ControlPattern
chdecay = clhatdecay

bpq :: Pattern Double -> ControlPattern
bpq = bandq

bpf :: Pattern Double -> ControlPattern
bpf = bandf

att :: Pattern Double -> ControlPattern
att = attack

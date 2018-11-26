module Sound.Tidal.Params where

import qualified Data.Map.Strict as Map

import Sound.Tidal.Pattern
import Sound.Tidal.Utils

-- | group multiple params into one
grp :: [String -> ControlMap] -> Pattern String -> ControlPattern
grp [] _ = empty
grp fs p = splitby <$> p
  where splitby name = Map.unions $ map (\(v, f) -> f v) $ zip (split name) fs
        split :: String -> [String]
        split = wordsBy (==':')

mF :: String -> String -> ControlMap
mF name v = Map.singleton name (VF $ read v)

mI :: String -> String -> ControlMap
mI name v = Map.singleton name (VI $ read v)

mS :: String -> String -> ControlMap
mS name v = Map.singleton name (VS v)

-- | Grouped params

sound :: Pattern String -> ControlPattern
sound = grp [mS "s", mF "n"]

s :: Pattern String -> ControlPattern
s = sound

cc :: Pattern String -> ControlPattern
cc = grp [mF "ccn", mF "ccv"]

-- | Singular params

pF :: String -> Pattern Double -> ControlPattern
pF name = fmap (Map.singleton name . VF)

pI :: String -> Pattern Int -> ControlPattern
pI name = fmap (Map.singleton name . VI)

pS :: String -> Pattern String -> ControlPattern
pS name = fmap (Map.singleton name . VS)

-- | a pattern of numbers that speed up (or slow down) samples while they play.
accelerate :: Pattern Double -> ControlPattern
accelerate       = pF "accelerate"

-- | a pattern of numbers to specify the attack time (in seconds) of an envelope applied to each sample. Only takes effect if `release` is also specified.
attack :: Pattern Double -> ControlPattern
attack = pF "attack"

-- | a pattern of numbers from 0 to 1. Sets the center frequency of the band-pass filter.
bandf :: Pattern Double -> ControlPattern
bandf = pF "bandf"

-- | a pattern of numbers from 0 to 1. Sets the q-factor of the band-pass filter.y
bandq :: Pattern Double -> ControlPattern
bandq = pF "bandq"

{- | a pattern of numbers from 0 to 1. Skips the beginning of each sample, e.g. `0.25` to cut off the first quarter from each sample.

Using `begin "-1"` combined with `cut "-1"` means that when the sample cuts itself it will begin playback from where the previous one left off, so it will sound like one seamless sample. This allows you to apply a synth param across a long sample in a way similar to `chop`:

@
cps 0.5

d1 $ sound "breaks125*8" # unit "c" # begin "-1" # cut "-1" # coarse "1 2 4 8 16 32 64 128"
@

This will play the `breaks125` sample and apply the changing `coarse` parameter over the sample. Compare to:

@
d1 $ (chop 8 $ sounds "breaks125") # unit "c" # coarse "1 2 4 8 16 32 64 128"
@

which performs a similar effect, but due to differences in implementation sounds different.
-}
begin, legato, clhatdecay, crush :: Pattern Double -> ControlPattern
channel, coarse :: Pattern Int -> ControlPattern
begin = pF "begin"
-- | choose the physical channel the pattern is sent to, this is super dirt specific
channel = pI "channel"

--legato controls the amount of overlap between two adjacent synth sounds
legato = pF "legato"

clhatdecay = pF "clhatdecay"
-- | fake-resampling, a pattern of numbers for lowering the sample rate, i.e. 1 for original 2 for half, 3 for a third and so on.
coarse = pI "coarse"
-- | bit crushing, a pattern of numbers from 1 (for drastic reduction in bit-depth) to 16 (for barely no reduction).
crush = pF "crush"
{- |
In the style of classic drum-machines, `cut` will stop a playing sample as soon as another samples with in same cutgroup is to be played.

An example would be an open hi-hat followed by a closed one, essentially muting the open.

@
d1 $ stack [
  sound "bd",
  sound "~ [~ [ho:2 hc/2]]" # cut "1"
  ]
@

This will mute the open hi-hat every second cycle when the closed one is played.

Using `cut` with negative values will only cut the same sample. This is useful to cut very long samples

@
d1 $ sound "[bev, [ho:3](3,8)]" # cut "-1"
@

Using `cut "0"` is effectively _no_ cutgroup.
-}
cut :: Pattern Int -> ControlPattern
cut = pI "cut"

-- | a pattern of numbers from 0 to 1. Applies the cutoff frequency of the low-pass filter.
cutoff :: Pattern Double -> ControlPattern
cutoff = pF "cutoff"

cutoffegint :: Pattern Double -> ControlPattern
cutoffegint = pF "cutoffegint"

decay :: Pattern Double -> ControlPattern
decay = pF "decay"
-- | a pattern of numbers from 0 to 1. Sets the level of the delay signal.
delay :: Pattern Double -> ControlPattern
delay = pF "delay"
-- | a pattern of numbers from 0 to 1. Sets the amount of delay feedback.
delayfeedback :: Pattern Double -> ControlPattern
delayfeedback = pF "delayfeedback"
-- | a pattern of numbers from 0 to 1. Sets the length of the delay.
delaytime :: Pattern Double -> ControlPattern
delaytime = pF "delaytime"
detune :: Pattern Double -> ControlPattern
detune = pF "detune"
-- | when set to `1` will disable all reverb for this pattern. See `room` and `size` for more information about reverb.
dry :: Pattern Double -> ControlPattern
dry = pF "dry"
{- the same as `begin`, but cuts the end off samples, shortening them;
 e.g. `0.75` to cut off the last quarter of each sample.
-}
end :: Pattern Double -> ControlPattern
end = pF "end"
-- | a pattern of numbers that specify volume. Values less than 1 make the sound quieter. Values greater than 1 make the sound louder.
gain :: Pattern Double -> ControlPattern
gain = pF "gain"
gate :: Pattern Double -> ControlPattern
gate = pF "gate"
hatgrain :: Pattern Double -> ControlPattern
hatgrain = pF "hatgrain"
-- | a pattern of numbers from 0 to 1. Applies the cutoff frequency of the high-pass filter.
hcutoff :: Pattern Double -> ControlPattern
hcutoff = pF "hcutoff"
-- | a pattern of numbers to specify the hold time (in seconds) of an envelope applied to each sample. Only takes effect if `attack` and `release` are also specified.
hold :: Pattern Double -> ControlPattern
hold = pF "hold"
-- | a pattern of numbers from 0 to 1. Applies the resonance of the high-pass filter.
hresonance :: Pattern Double -> ControlPattern
hresonance = pF "hresonance"
kriole :: Pattern Int -> ControlPattern
kriole = pI "kriole"
lagogo :: Pattern Double -> ControlPattern
lagogo = pF "lagogo"
lclap :: Pattern Double -> ControlPattern
lclap = pF "lclap"
lclaves :: Pattern Double -> ControlPattern
lclaves = pF "lclaves"
lclhat :: Pattern Double -> ControlPattern
lclhat = pF "lclhat"
lcrash :: Pattern Double -> ControlPattern
lcrash = pF "lcrash"
leslie :: Pattern Double -> ControlPattern
leslie = pF "leslie"
lrate :: Pattern Double -> ControlPattern
lrate = pF "lrate"
lsize :: Pattern Double -> ControlPattern
lsize = pF "lsize"
lfo :: Pattern Double -> ControlPattern
lfo = pF "lfo"
lfocutoffint :: Pattern Double -> ControlPattern
lfocutoffint = pF "lfocutoffint"
lfodelay :: Pattern Double -> ControlPattern
lfodelay = pF "lfodelay"
lfoint :: Pattern Double -> ControlPattern
lfoint = pF "lfoint"
lfopitchint :: Pattern Double -> ControlPattern
lfopitchint = pF "lfopitchint"
lfoshape :: Pattern Double -> ControlPattern
lfoshape = pF "lfoshape"
lfosync :: Pattern Double -> ControlPattern
lfosync = pF "lfosync"
lhitom :: Pattern Double -> ControlPattern
lhitom = pF "lhitom"
lkick :: Pattern Double -> ControlPattern
lkick = pF "lkick"
llotom :: Pattern Double -> ControlPattern
llotom = pF "llotom"
{- | A pattern of numbers. Specifies whether delaytime is calculated relative to cps. When set to 1, delaytime is a direct multiple of a cycle.
-}
lock :: Pattern Double -> ControlPattern
lock = pF "lock"
-- | loops the sample (from `begin` to `end`) the specified number of times.
loop :: Pattern Double -> ControlPattern
loop = pF "loop"
lophat :: Pattern Double -> ControlPattern
lophat = pF "lophat"
lsnare :: Pattern Double -> ControlPattern
lsnare = pF "lsnare"
-- | specifies the sample or note number to be used
n :: Pattern Double -> ControlPattern
n = pF "n"
note :: Pattern Double -> ControlPattern
note = pF "note"
{- |
Pushes things forward (or backwards within built-in latency) in time. Allows for nice things like _swing_ feeling:

@
d1 $ stack [
 sound "bd bd/4",
 sound "hh(5,8)"
 ] # nudge "[0 0.04]*4"
@

--pitch model -}

degree, mtranspose, ctranspose, harmonic, stepsPerOctave, octaveRatio :: Pattern Double -> ControlPattern
degree = pF "degree"
mtranspose = pF "mtranspose"
ctranspose = pF "ctranspose"
harmonic = pF "ctranspose"
stepsPerOctave = pF "stepsPerOctave"
octaveRatio = pF "octaveRatio"


--Low values will give a more _human_ feeling, high values might result in quite the contrary.

nudge :: Pattern Double -> ControlPattern
nudge = pF "nudge"
octave :: Pattern Int -> ControlPattern
octave = pI "octave"
offset :: Pattern Double -> ControlPattern
offset = pF "offset"
ophatdecay :: Pattern Double -> ControlPattern
ophatdecay = pF "ophatdecay"
{- | a pattern of numbers. An `orbit` is a global parameter context for patterns. Patterns with the same orbit will share hardware output bus offset and global effects, e.g. reverb and delay. The maximum number of orbits is specified in the superdirt startup, numbers higher than maximum will wrap around.
-}
orbit :: Pattern Int -> ControlPattern
orbit = pI "orbit"
-- | a pattern of numbers between 0 and 1, from left to right (assuming stereo), once round a circle (assuming multichannel)
pan :: Pattern Double -> ControlPattern
pan = pF "pan"
-- | a pattern of numbers between -inf and inf, which controls how much multichannel output is fanned out (negative is backwards ordering)
panspan :: Pattern Double -> ControlPattern
panspan = pF "span"
-- | a pattern of numbers between 0.0 and 1.0, which controls the multichannel spread range (multichannel only)
pansplay :: Pattern Double -> ControlPattern
pansplay = pF "splay"
-- | a pattern of numbers between 0.0 and inf, which controls how much each channel is distributed over neighbours (multichannel only)
panwidth :: Pattern Double -> ControlPattern
panwidth = pF "panwidth"
-- | a pattern of numbers between -1.0 and 1.0, which controls the relative position of the centre pan in a pair of adjacent speakers (multichannel only)
panorient :: Pattern Double -> ControlPattern
panorient = pF "orientation"

pitch1 :: Pattern Double -> ControlPattern
pitch1 = pF "pitch1"
pitch2 :: Pattern Double -> ControlPattern
pitch2 = pF "pitch2"
pitch3 :: Pattern Double -> ControlPattern
pitch3 = pF "pitch3"
portamento :: Pattern Double -> ControlPattern
portamento = pF "portamento"
-- | a pattern of numbers to specify the release time (in seconds) of an envelope applied to each sample. Only takes effect if `attack` is also specified.
release :: Pattern Double -> ControlPattern
release = pF "release"
-- | a pattern of numbers from 0 to 1. Specifies the resonance of the low-pass filter.
resonance :: Pattern Double -> ControlPattern
resonance = pF "resonance"
-- | a pattern of numbers from 0 to 1. Sets the level of reverb.
room :: Pattern Double -> ControlPattern
room = pF "room"
sagogo :: Pattern Double -> ControlPattern
sagogo = pF "sagogo"
sclap :: Pattern Double -> ControlPattern
sclap = pF "sclap"
sclaves :: Pattern Double -> ControlPattern
sclaves = pF "sclaves"
scrash :: Pattern Double -> ControlPattern
scrash = pF "scrash"
semitone :: Pattern Double -> ControlPattern
semitone = pF "semitone"
-- | wave shaping distortion, a pattern of numbers from 0 for no distortion up to 1 for loads of distortion.
shape :: Pattern Double -> ControlPattern
shape = pF "shape"
-- | a pattern of numbers from 0 to 1. Sets the perceptual size (reverb time) of the `room` to be used in reverb.
size :: Pattern Double -> ControlPattern
size = pF "size"
slide :: Pattern Double -> ControlPattern
slide = pF "slide"
-- | a pattern of numbers which changes the speed of sample playback, i.e. a cheap way of changing pitch. Negative values will play the sample backwards!
speed :: Pattern Double -> ControlPattern
speed = pF "speed"
-- | a pattern of strings. Selects the sample to be played.
s' :: Pattern String -> ControlPattern
s' = pS "s"
stutterdepth :: Pattern Double -> ControlPattern
stutterdepth = pF "stutterdepth"
stuttertime :: Pattern Double -> ControlPattern
stuttertime = pF "stuttertime"
sustain :: Pattern Double -> ControlPattern
sustain = pF "sustain"
tomdecay :: Pattern Double -> ControlPattern
tomdecay = pF "tomdecay"
{- | used in conjunction with `speed`, accepts values of "r" (rate, default behavior), "c" (cycles), or "s" (seconds).
Using `unit "c"` means `speed` will be interpreted in units of cycles, e.g. `speed "1"` means samples will be stretched to fill a cycle.
Using `unit "s"` means the playback speed will be adjusted so that the duration is the number of seconds specified by `speed`.
-}
unit :: Pattern String -> ControlPattern
unit = pS "unit"
velocity :: Pattern Double -> ControlPattern
velocity = pF "velocity"
vcfegint :: Pattern Double -> ControlPattern
vcfegint = pF "vcfegint"
vcoegint :: Pattern Double -> ControlPattern
vcoegint = pF "vcoegint"
voice :: Pattern Double -> ControlPattern
voice = pF "voice"
-- | formant filter to make things sound like vowels, a pattern of either `a`, `e`, `i`, `o` or `u`. Use a rest (`~`) for no effect.
vowel :: Pattern String -> ControlPattern
vowel = pS "vowel"

-- MIDI-specific params

dur :: Pattern Double -> ControlPattern
dur = pF "dur"
modwheel :: Pattern Double -> ControlPattern
modwheel = pF "modwheel"
expression :: Pattern Double -> ControlPattern
expression = pF "expression"
sustainpedal :: Pattern Double -> ControlPattern
sustainpedal = pF "sustainpedal"

-- Tremolo Audio DSP effect | params are "tremolorate" and "tremolodepth"
tremolorate, tremolodepth :: Pattern Double -> ControlPattern
tremolorate = pF "tremolorate"
tremolodepth = pF "tremolodepth"

-- Phaser Audio DSP effect | params are "phaserrate" and "phaserdepth"
phaserrate, phaserdepth :: Pattern Double -> ControlPattern
phaserrate = pF "phaserrate"
phaserdepth = pF "phaserdepth"

-- aliases
att, bpf, bpq, chdecay, ctf, ctfg, delayfb, delayt, det, gat, hg, hpf, hpq, lag, lbd, lch, lcl, lcp, lcr, lfoc, lfoi
   , lfop, lht, llt, loh, lpf, lpq, lsn, ohdecay, phasdp, phasr, pit1, pit2, pit3, por, rel, sz, sag, scl, scp
   , scr, sld, std, stt, sus, tdecay, tremdp, tremr, vcf, vco, voi
 :: Pattern Double -> ControlPattern
att = attack
bpf = bandf
bpq = bandq
chdecay = clhatdecay
ctf = cutoff
ctfg = cutoffegint
delayfb = delayfeedback
delayt = delaytime
det = detune
gat = gate
hg = hatgrain
hpf = hcutoff
hpq = hresonance
lag = lagogo
lbd = lkick
lch = lclhat
lcl = lclaves
lcp = lclap
lcr = lcrash
lfoc = lfocutoffint
lfoi = lfoint
lfop = lfopitchint
lht = lhitom
llt = llotom
loh = lophat
lpf = cutoff
lpq = resonance
lsn = lsnare
ohdecay = ophatdecay
phasdp = phaserdepth
phasr = phaserrate
pit1 = pitch1
pit2 = pitch2
pit3 = pitch3
por = portamento
rel = release
sag = sagogo
scl = sclaves
scp = sclap
scr = scrash
sz = size
sld = slide
std = stutterdepth
stt = stuttertime
sus = sustain
tdecay = tomdecay
tremdp = tremolodepth
tremr = tremolorate
vcf = vcfegint
vco = vcoegint
voi = voice

midinote :: Pattern Double -> ControlPattern
midinote = note . ((subtract 60) <$>)

drum :: Pattern String -> ControlPattern
drum = n . ((subtract 60) . drumN <$>)

drumN :: Num a => String -> a
drumN "bd" = 36
drumN "sn" = 38
drumN "lt" = 43
drumN "ht" = 50
drumN "ch" = 42
drumN "oh" = 46
drumN "cp" = 39
drumN "cl" = 75
drumN "ag" = 67
drumN "cr" = 49
drumN _ = 0


-- SuperDirt MIDI Params

array :: Pattern Double -> ControlPattern
array = pF "array"
midichan :: Pattern Double -> ControlPattern
midichan = pF "midichan"
control :: Pattern Double -> ControlPattern
control = pF "control"

ccn :: Pattern Double -> ControlPattern
ccn = pF "ccn"
ccv :: Pattern Double -> ControlPattern
ccv = pF "ccv"

ctlNum :: Pattern Double -> ControlPattern
ctlNum = pF "ctlNum"

frameRate :: Pattern Double -> ControlPattern
frameRate = pF "frameRate"
frames :: Pattern Double -> ControlPattern
frames = pF "frames"
hours :: Pattern Double -> ControlPattern
hours = pF "hours"

midicmd :: Pattern String -> ControlPattern
midicmd = pS "midicmd"
command :: Pattern String -> ControlPattern
command = midicmd

minutes :: Pattern Double -> ControlPattern
minutes = pF "minutes"
progNum :: Pattern Double -> ControlPattern
progNum = pF "progNum"
seconds :: Pattern Double -> ControlPattern
seconds = pF "seconds"
songPtr :: Pattern Double -> ControlPattern
songPtr = pF "songPtr"
uid :: Pattern Double -> ControlPattern
uid = pF "uid"
val :: Pattern Double -> ControlPattern
val = pF "val"

{- | `up` is now an alias of `note`. -}
up :: Pattern Double -> ControlPattern
up = note

cps :: Pattern Double -> ControlPattern
cps = pF "cps"

-- | Useful for MIDI CCs
cc0:: Pattern Double -> ControlPattern
cc0 = pF "cc0"
cc1:: Pattern Double -> ControlPattern
cc1 = pF "cc1"
cc2:: Pattern Double -> ControlPattern
cc2 = pF "cc2"
cc3:: Pattern Double -> ControlPattern
cc3 = pF "cc3"
cc4:: Pattern Double -> ControlPattern
cc4 = pF "cc4"
cc5:: Pattern Double -> ControlPattern
cc5 = pF "cc5"
cc6:: Pattern Double -> ControlPattern
cc6 = pF "cc6"
cc7:: Pattern Double -> ControlPattern
cc7 = pF "cc7"
cc8:: Pattern Double -> ControlPattern
cc8 = pF "cc8"
cc9:: Pattern Double -> ControlPattern
cc9 = pF "cc9"
cc10:: Pattern Double -> ControlPattern
cc10 = pF "cc10"
cc11:: Pattern Double -> ControlPattern
cc11 = pF "cc11"
cc12:: Pattern Double -> ControlPattern
cc12 = pF "cc12"
cc13:: Pattern Double -> ControlPattern
cc13 = pF "cc13"
cc14:: Pattern Double -> ControlPattern
cc14 = pF "cc14"
cc15:: Pattern Double -> ControlPattern
cc15 = pF "cc15"
cc16:: Pattern Double -> ControlPattern
cc16 = pF "cc16"
cc17:: Pattern Double -> ControlPattern
cc17 = pF "cc17"
cc18:: Pattern Double -> ControlPattern
cc18 = pF "cc18"
cc19:: Pattern Double -> ControlPattern
cc19 = pF "cc19"
cc20:: Pattern Double -> ControlPattern
cc20 = pF "cc20"
cc21:: Pattern Double -> ControlPattern
cc21 = pF "cc21"
cc22:: Pattern Double -> ControlPattern
cc22 = pF "cc22"
cc23:: Pattern Double -> ControlPattern
cc23 = pF "cc23"
cc24:: Pattern Double -> ControlPattern
cc24 = pF "cc24"
cc25:: Pattern Double -> ControlPattern
cc25 = pF "cc25"
cc26:: Pattern Double -> ControlPattern
cc26 = pF "cc26"
cc27:: Pattern Double -> ControlPattern
cc27 = pF "cc27"
cc28:: Pattern Double -> ControlPattern
cc28 = pF "cc28"
cc29:: Pattern Double -> ControlPattern
cc29 = pF "cc29"
cc30:: Pattern Double -> ControlPattern
cc30 = pF "cc30"
cc31:: Pattern Double -> ControlPattern
cc31 = pF "cc31"
cc32:: Pattern Double -> ControlPattern
cc32 = pF "cc32"
cc33:: Pattern Double -> ControlPattern
cc33 = pF "cc33"
cc34:: Pattern Double -> ControlPattern
cc34 = pF "cc34"
cc35:: Pattern Double -> ControlPattern
cc35 = pF "cc35"
cc36:: Pattern Double -> ControlPattern
cc36 = pF "cc36"
cc37:: Pattern Double -> ControlPattern
cc37 = pF "cc37"
cc38:: Pattern Double -> ControlPattern
cc38 = pF "cc38"
cc39:: Pattern Double -> ControlPattern
cc39 = pF "cc39"
cc40:: Pattern Double -> ControlPattern
cc40 = pF "cc40"
cc41:: Pattern Double -> ControlPattern
cc41 = pF "cc41"
cc42:: Pattern Double -> ControlPattern
cc42 = pF "cc42"
cc43:: Pattern Double -> ControlPattern
cc43 = pF "cc43"
cc44:: Pattern Double -> ControlPattern
cc44 = pF "cc44"
cc45:: Pattern Double -> ControlPattern
cc45 = pF "cc45"
cc46:: Pattern Double -> ControlPattern
cc46 = pF "cc46"
cc47:: Pattern Double -> ControlPattern
cc47 = pF "cc47"
cc48:: Pattern Double -> ControlPattern
cc48 = pF "cc48"
cc49:: Pattern Double -> ControlPattern
cc49 = pF "cc49"
cc50:: Pattern Double -> ControlPattern
cc50 = pF "cc50"
cc51:: Pattern Double -> ControlPattern
cc51 = pF "cc51"
cc52:: Pattern Double -> ControlPattern
cc52 = pF "cc52"
cc53:: Pattern Double -> ControlPattern
cc53 = pF "cc53"
cc54:: Pattern Double -> ControlPattern
cc54 = pF "cc54"
cc55:: Pattern Double -> ControlPattern
cc55 = pF "cc55"
cc56:: Pattern Double -> ControlPattern
cc56 = pF "cc56"
cc57:: Pattern Double -> ControlPattern
cc57 = pF "cc57"
cc58:: Pattern Double -> ControlPattern
cc58 = pF "cc58"
cc59:: Pattern Double -> ControlPattern
cc59 = pF "cc59"
cc60:: Pattern Double -> ControlPattern
cc60 = pF "cc60"
cc61:: Pattern Double -> ControlPattern
cc61 = pF "cc61"
cc62:: Pattern Double -> ControlPattern
cc62 = pF "cc62"
cc63:: Pattern Double -> ControlPattern
cc63 = pF "cc63"
cc64:: Pattern Double -> ControlPattern
cc64 = pF "cc64"
cc65:: Pattern Double -> ControlPattern
cc65 = pF "cc65"
cc66:: Pattern Double -> ControlPattern
cc66 = pF "cc66"
cc67:: Pattern Double -> ControlPattern
cc67 = pF "cc67"
cc68:: Pattern Double -> ControlPattern
cc68 = pF "cc68"
cc69:: Pattern Double -> ControlPattern
cc69 = pF "cc69"
cc70:: Pattern Double -> ControlPattern
cc70 = pF "cc70"
cc71:: Pattern Double -> ControlPattern
cc71 = pF "cc71"
cc72:: Pattern Double -> ControlPattern
cc72 = pF "cc72"
cc73:: Pattern Double -> ControlPattern
cc73 = pF "cc73"
cc74:: Pattern Double -> ControlPattern
cc74 = pF "cc74"
cc75:: Pattern Double -> ControlPattern
cc75 = pF "cc75"
cc76:: Pattern Double -> ControlPattern
cc76 = pF "cc76"
cc77:: Pattern Double -> ControlPattern
cc77 = pF "cc77"
cc78:: Pattern Double -> ControlPattern
cc78 = pF "cc78"
cc79:: Pattern Double -> ControlPattern
cc79 = pF "cc79"
cc80:: Pattern Double -> ControlPattern
cc80 = pF "cc80"
cc81:: Pattern Double -> ControlPattern
cc81 = pF "cc81"
cc82:: Pattern Double -> ControlPattern
cc82 = pF "cc82"
cc83:: Pattern Double -> ControlPattern
cc83 = pF "cc83"
cc84:: Pattern Double -> ControlPattern
cc84 = pF "cc84"
cc85:: Pattern Double -> ControlPattern
cc85 = pF "cc85"
cc86:: Pattern Double -> ControlPattern
cc86 = pF "cc86"
cc87:: Pattern Double -> ControlPattern
cc87 = pF "cc87"
cc88:: Pattern Double -> ControlPattern
cc88 = pF "cc88"
cc89:: Pattern Double -> ControlPattern
cc89 = pF "cc89"
cc90:: Pattern Double -> ControlPattern
cc90 = pF "cc90"
cc91:: Pattern Double -> ControlPattern
cc91 = pF "cc91"
cc92:: Pattern Double -> ControlPattern
cc92 = pF "cc92"
cc93:: Pattern Double -> ControlPattern
cc93 = pF "cc93"
cc94:: Pattern Double -> ControlPattern
cc94 = pF "cc94"
cc95:: Pattern Double -> ControlPattern
cc95 = pF "cc95"
cc96:: Pattern Double -> ControlPattern
cc96 = pF "cc96"
cc97:: Pattern Double -> ControlPattern
cc97 = pF "cc97"
cc98:: Pattern Double -> ControlPattern
cc98 = pF "cc98"
cc99:: Pattern Double -> ControlPattern
cc99 = pF "cc99"
cc100:: Pattern Double -> ControlPattern
cc100 = pF "cc100"
cc101:: Pattern Double -> ControlPattern
cc101 = pF "cc101"
cc102:: Pattern Double -> ControlPattern
cc102 = pF "cc102"
cc103:: Pattern Double -> ControlPattern
cc103 = pF "cc103"
cc104:: Pattern Double -> ControlPattern
cc104 = pF "cc104"
cc105:: Pattern Double -> ControlPattern
cc105 = pF "cc105"
cc106:: Pattern Double -> ControlPattern
cc106 = pF "cc106"
cc107:: Pattern Double -> ControlPattern
cc107 = pF "cc107"
cc108:: Pattern Double -> ControlPattern
cc108 = pF "cc108"
cc109:: Pattern Double -> ControlPattern
cc109 = pF "cc109"
cc110:: Pattern Double -> ControlPattern
cc110 = pF "cc110"
cc111:: Pattern Double -> ControlPattern
cc111 = pF "cc111"
cc112:: Pattern Double -> ControlPattern
cc112 = pF "cc112"
cc113:: Pattern Double -> ControlPattern
cc113 = pF "cc113"
cc114:: Pattern Double -> ControlPattern
cc114 = pF "cc114"
cc115:: Pattern Double -> ControlPattern
cc115 = pF "cc115"
cc116:: Pattern Double -> ControlPattern
cc116 = pF "cc116"
cc117:: Pattern Double -> ControlPattern
cc117 = pF "cc117"
cc118:: Pattern Double -> ControlPattern
cc118 = pF "cc118"
cc119:: Pattern Double -> ControlPattern
cc119 = pF "cc119"
cc120:: Pattern Double -> ControlPattern
cc120 = pF "cc120"
cc121:: Pattern Double -> ControlPattern
cc121 = pF "cc121"
cc122:: Pattern Double -> ControlPattern
cc122 = pF "cc122"
cc123:: Pattern Double -> ControlPattern
cc123 = pF "cc123"
cc124:: Pattern Double -> ControlPattern
cc124 = pF "cc124"
cc125:: Pattern Double -> ControlPattern
cc125 = pF "cc125"
cc126:: Pattern Double -> ControlPattern
cc126 = pF "cc126"
cc127:: Pattern Double -> ControlPattern
cc127 = pF "cc127"

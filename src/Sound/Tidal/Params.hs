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
squiz :: Pattern Double -> ControlPattern
squiz = pF "squiz"
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
waveloss :: Pattern Double -> ControlPattern
waveloss = pF "waveloss"

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

-- generic names for mapping to e.g. midi controls
button0 :: Pattern Double -> ControlPattern
button0 = pF "button0"
button1 :: Pattern Double -> ControlPattern
button1 = pF "button1"
button2 :: Pattern Double -> ControlPattern
button2 = pF "button2"
button3 :: Pattern Double -> ControlPattern
button3 = pF "button3"
button4 :: Pattern Double -> ControlPattern
button4 = pF "button4"
button5 :: Pattern Double -> ControlPattern
button5 = pF "button5"
button6 :: Pattern Double -> ControlPattern
button6 = pF "button6"
button7 :: Pattern Double -> ControlPattern
button7 = pF "button7"
button8 :: Pattern Double -> ControlPattern
button8 = pF "button8"
button9 :: Pattern Double -> ControlPattern
button9 = pF "button9"
button10 :: Pattern Double -> ControlPattern
button10 = pF "button10"
button11 :: Pattern Double -> ControlPattern
button11 = pF "button11"
button12 :: Pattern Double -> ControlPattern
button12 = pF "button12"
button13 :: Pattern Double -> ControlPattern
button13 = pF "button13"
button14 :: Pattern Double -> ControlPattern
button14 = pF "button14"
button15 :: Pattern Double -> ControlPattern
button15 = pF "button15"
button16 :: Pattern Double -> ControlPattern
button16 = pF "button16"
button17 :: Pattern Double -> ControlPattern
button17 = pF "button17"
button18 :: Pattern Double -> ControlPattern
button18 = pF "button18"
button19 :: Pattern Double -> ControlPattern
button19 = pF "button19"
button20 :: Pattern Double -> ControlPattern
button20 = pF "button20"
button21 :: Pattern Double -> ControlPattern
button21 = pF "button21"
button22 :: Pattern Double -> ControlPattern
button22 = pF "button22"
button23 :: Pattern Double -> ControlPattern
button23 = pF "button23"
button24 :: Pattern Double -> ControlPattern
button24 = pF "button24"
button25 :: Pattern Double -> ControlPattern
button25 = pF "button25"
button26 :: Pattern Double -> ControlPattern
button26 = pF "button26"
button27 :: Pattern Double -> ControlPattern
button27 = pF "button27"
button28 :: Pattern Double -> ControlPattern
button28 = pF "button28"
button29 :: Pattern Double -> ControlPattern
button29 = pF "button29"
button30 :: Pattern Double -> ControlPattern
button30 = pF "button30"
button31 :: Pattern Double -> ControlPattern
button31 = pF "button31"

slider0 :: Pattern Double -> ControlPattern
slider0 = pF "slider0"
slider1 :: Pattern Double -> ControlPattern
slider1 = pF "slider1"
slider2 :: Pattern Double -> ControlPattern
slider2 = pF "slider2"
slider3 :: Pattern Double -> ControlPattern
slider3 = pF "slider3"
slider4 :: Pattern Double -> ControlPattern
slider4 = pF "slider4"
slider5 :: Pattern Double -> ControlPattern
slider5 = pF "slider5"
slider6 :: Pattern Double -> ControlPattern
slider6 = pF "slider6"
slider7 :: Pattern Double -> ControlPattern
slider7 = pF "slider7"
slider8 :: Pattern Double -> ControlPattern
slider8 = pF "slider8"
slider9 :: Pattern Double -> ControlPattern
slider9 = pF "slider9"
slider10 :: Pattern Double -> ControlPattern
slider10 = pF "slider10"
slider11 :: Pattern Double -> ControlPattern
slider11 = pF "slider11"
slider12 :: Pattern Double -> ControlPattern
slider12 = pF "slider12"
slider13 :: Pattern Double -> ControlPattern
slider13 = pF "slider13"
slider14 :: Pattern Double -> ControlPattern
slider14 = pF "slider14"
slider15 :: Pattern Double -> ControlPattern
slider15 = pF "slider15"
slider16 :: Pattern Double -> ControlPattern
slider16 = pF "slider16"
slider17 :: Pattern Double -> ControlPattern
slider17 = pF "slider17"
slider18 :: Pattern Double -> ControlPattern
slider18 = pF "slider18"
slider19 :: Pattern Double -> ControlPattern
slider19 = pF "slider19"
slider20 :: Pattern Double -> ControlPattern
slider20 = pF "slider20"
slider21 :: Pattern Double -> ControlPattern
slider21 = pF "slider21"
slider22 :: Pattern Double -> ControlPattern
slider22 = pF "slider22"
slider23 :: Pattern Double -> ControlPattern
slider23 = pF "slider23"
slider24 :: Pattern Double -> ControlPattern
slider24 = pF "slider24"
slider25 :: Pattern Double -> ControlPattern
slider25 = pF "slider25"
slider26 :: Pattern Double -> ControlPattern
slider26 = pF "slider26"
slider27 :: Pattern Double -> ControlPattern
slider27 = pF "slider27"
slider28 :: Pattern Double -> ControlPattern
slider28 = pF "slider28"
slider29 :: Pattern Double -> ControlPattern
slider29 = pF "slider29"
slider30 :: Pattern Double -> ControlPattern
slider30 = pF "slider30"
slider31 :: Pattern Double -> ControlPattern
slider31 = pF "slider31"

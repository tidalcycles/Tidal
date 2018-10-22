module Sound.Tidal.Params where

import qualified Data.Map.Strict as Map

import Sound.Tidal.Pattern
import Sound.Tidal.Core
import Sound.Tidal.Utils

-- | group multiple params into one
grp :: [String -> ControlMap] -> Pattern String -> ControlPattern
grp [] _ = empty
grp fs p = f <$> p
  where f s = Map.unions $ map (\(v, f) -> f v) $ zip (split s) fs
        split :: String -> [String]
        split s = wordsBy (==':') s

mF :: String -> String -> ControlMap
mF name v = Map.singleton name (VF $ read v)

mI :: String -> String -> ControlMap
mI name v = Map.singleton name (VI $ read v)

mS :: String -> String -> ControlMap
mS name v = Map.singleton name (VS v)

sound = grp [mS "s", mF "n"]
s = sound
cc = grp [mF "ccn", mF "ccv"]

-- | Singular params
pF :: String -> Pattern Double -> ControlPattern
pF name = fmap (Map.singleton name . VF)

pI :: String -> Pattern Int -> ControlPattern
pI name = fmap (Map.singleton name . VI)

pS :: String -> Pattern String -> ControlPattern
pS name = fmap (Map.singleton name . VS)

-- | a pattern of numbers that speed up (or slow down) samples while they play.
accelerate       = pF "accelerate"
-- | a pattern of numbers to specify the attack time (in seconds) of an envelope applied to each sample. Only takes effect if `release` is also specified.
attack = pF "attack"
-- | a pattern of numbers from 0 to 1. Sets the center frequency of the band-pass filter.
bandf = pF "bandf"
-- | a pattern of numbers from 0 to 1. Sets the q-factor of the band-pass filter.y
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
cut = pI "cut"
-- | a pattern of numbers from 0 to 1. Applies the cutoff frequency of the low-pass filter.
cutoff = pF "cutoff"
cutoffegint = pF "cutoffegint"
decay = pF "decay"
-- | a pattern of numbers from 0 to 1. Sets the level of the delay signal.
delay = pF "delay"
-- | a pattern of numbers from 0 to 1. Sets the amount of delay feedback.
delayfeedback = pF "delayfeedback"
-- | a pattern of numbers from 0 to 1. Sets the length of the delay.
delaytime = pF "delaytime"
detune = pF "detune"
-- | when set to `1` will disable all reverb for this pattern. See `room` and `size` for more information about reverb.
dry = pF "dry"
{- the same as `begin`, but cuts the end off samples, shortening them;
 e.g. `0.75` to cut off the last quarter of each sample.
-}
end = pF "end"
-- | a pattern of numbers that specify volume. Values less than 1 make the sound quieter. Values greater than 1 make the sound louder.
gain = pF "gain"
gate = pF "gate"
hatgrain = pF "hatgrain"
-- | a pattern of numbers from 0 to 1. Applies the cutoff frequency of the high-pass filter.
hcutoff = pF "hcutoff"
-- | a pattern of numbers to specify the hold time (in seconds) of an envelope applied to each sample. Only takes effect if `attack` and `release` are also specified.
hold = pF "hold"
-- | a pattern of numbers from 0 to 1. Applies the resonance of the high-pass filter.
hresonance = pF "hresonance"
kriole = pI "kriole"
lagogo = pF "lagogo"
lclap = pF "lclap"
lclaves = pF "lclaves"
lclhat = pF "lclhat"
lcrash = pF "lcrash"
leslie = pF "leslie"
lrate = pF "lrate"
lsize = pF "lsize"
lfo = pF "lfo"
lfocutoffint = pF "lfocutoffint"
lfodelay = pF "lfodelay"
lfoint = pF "lfoint"
lfopitchint = pF "lfopitchint"
lfoshape = pF "lfoshape"
lfosync = pF "lfosync"
lhitom = pF "lhitom"
lkick = pF "lkick"
llotom = pF "llotom"
{- | A pattern of numbers. Specifies whether delaytime is calculated relative to cps. When set to 1, delaytime is a direct multiple of a cycle.
-}
lock = pF "lock"
-- | loops the sample (from `begin` to `end`) the specified number of times.
loop = pF "loop"
lophat = pF "lophat"
lsnare = pF "lsnare"
-- | specifies the sample or note number to be used
n = pF "n"
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

nudge = pF "nudge"
octave = pI "octave"
offset = pF "offset"
ophatdecay = pF "ophatdecay"
{- | a pattern of numbers. An `orbit` is a global parameter context for patterns. Patterns with the same orbit will share hardware output bus offset and global effects, e.g. reverb and delay. The maximum number of orbits is specified in the superdirt startup, numbers higher than maximum will wrap around.
-}
orbit = pI "orbit"
-- | a pattern of numbers between 0 and 1, from left to right (assuming stereo), once round a circle (assuming multichannel)
pan = pF "pan"
-- | a pattern of numbers between -inf and inf, which controls how much multichannel output is fanned out (negative is backwards ordering)
panspan = pF "span"
-- | a pattern of numbers between 0.0 and 1.0, which controls the multichannel spread range (multichannel only)
pansplay = pF "splay"
-- | a pattern of numbers between 0.0 and inf, which controls how much each channel is distributed over neighbours (multichannel only)
panwidth = pF "panwidth"
-- | a pattern of numbers between -1.0 and 1.0, which controls the relative position of the centre pan in a pair of adjacent speakers (multichannel only)
panorient = pF "orientation"

pitch1 = pF "pitch1"
pitch2 = pF "pitch2"
pitch3 = pF "pitch3"
portamento = pF "portamento"
-- | a pattern of numbers to specify the release time (in seconds) of an envelope applied to each sample. Only takes effect if `attack` is also specified.
release = pF "release"
-- | a pattern of numbers from 0 to 1. Specifies the resonance of the low-pass filter.
resonance = pF "resonance"
-- | a pattern of numbers from 0 to 1. Sets the level of reverb.
room = pF "room"
sagogo = pF "sagogo"
sclap = pF "sclap"
sclaves = pF "sclaves"
scrash = pF "scrash"
semitone = pF "semitone"
-- | wave shaping distortion, a pattern of numbers from 0 for no distortion up to 1 for loads of distortion.
shape = pF "shape"
-- | a pattern of numbers from 0 to 1. Sets the perceptual size (reverb time) of the `room` to be used in reverb.
size = pF "size"
slide = pF "slide"
-- | a pattern of numbers which changes the speed of sample playback, i.e. a cheap way of changing pitch. Negative values will play the sample backwards!
speed = pF "speed"
-- | a pattern of strings. Selects the sample to be played.
s' = pS "s"
stutterdepth = pF "stutterdepth"
stuttertime = pF "stuttertime"
sustain = pF "sustain"
tomdecay = pF "tomdecay"
{- | used in conjunction with `speed`, accepts values of "r" (rate, default behavior), "c" (cycles), or "s" (seconds).
Using `unit "c"` means `speed` will be interpreted in units of cycles, e.g. `speed "1"` means samples will be stretched to fill a cycle.
Using `unit "s"` means the playback speed will be adjusted so that the duration is the number of seconds specified by `speed`.
-}
unit = pS "unit"
velocity = pF "velocity"
vcfegint = pF "vcfegint"
vcoegint = pF "vcoegint"
voice = pF "voice"
-- | formant filter to make things sound like vowels, a pattern of either `a`, `e`, `i`, `o` or `u`. Use a rest (`~`) for no effect.
vowel = pS "vowel"

-- MIDI-specific params

dur = pF "dur"
modwheel = pF "modwheel"
expression = pF "expression"
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
att, chdecay, ctf, ctfg, delayfb, delayt, lbd, lch, lcl, lcp, lcr, lfoc, lfoi
   , lfop, lht, llt, loh, lsn, ohdecay, phasdp, phasr, pit1, pit2, pit3, por, sag, scl, scp
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

array = pF "array"
midichan = pF "midichan"
control = pF "control"

ccn = pF "ccn"
ccv = pF "ccv"

ctlNum = pF "ctlNum"

frameRate = pF "frameRate"
frames = pF "frames"
hours = pF "hours"

midicmd = pS "midicmd"
command = midicmd

minutes = pF "minutes"
progNum = pF "progNum"
seconds = pF "seconds"
songPtr = pF "songPtr"
uid = pF "uid"
val = pF "val"


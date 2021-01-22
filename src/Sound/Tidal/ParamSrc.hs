module Sound.Tidal.ParamMaker where

{-
    ParamsMaker.hs - Functions for generating Params.hs
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

import qualified Data.Map.Strict as Map

import Sound.Tidal.Pattern
import Sound.Tidal.Core ((#))
import Sound.Tidal.Utils
import Data.Maybe (fromMaybe)
import Data.Word (Word8)

-- | group multiple params into one
grp :: [String -> ControlMap] -> Pattern String -> ControlPattern
grp [] _ = empty
grp fs p = splitby <$> p
  where splitby name = Map.unions $ map (\(v, f) -> f v) $ zip (split name) fs
        split :: String -> [String]
        split = wordsBy (==':')

mF :: String -> String -> ControlMap
mF name v = fromMaybe Map.empty $ do f <- readMaybe v
                                     return $ Map.singleton name (VF f)

mI :: String -> String -> ControlMap
mI name v = fromMaybe Map.empty $ do i <- readMaybe v
                                     return $ Map.singleton name (VI i)

mS :: String -> String -> ControlMap
mS name v = Map.singleton name (VS v)

-- | Grouped params

  ("s", "sound", ""),
sound = grp [mS "s", mF "n"]

  ("s", "s", ""),
s = sound

  ("s", "cc", ""),
cc = grp [mF "ccn", mF "ccv"]

  ("s", "nrpn", ""),
nrpn = grp [mI "nrpn", mI "val"]

grain' :: Pattern String -> ControlPattern
grain' = grp [mF "begin", mF "end"]

-- | Singular params

pF :: String -> Pattern Double -> ControlPattern
pF name = fmap (Map.singleton name . VF)

pI :: String -> Pattern Int -> ControlPattern
pI name = fmap (Map.singleton name . VI)

pB :: String -> Pattern Bool -> ControlPattern
pB name = fmap (Map.singleton name . VB)
 
pR :: String -> Pattern Rational -> ControlPattern
pR name = fmap (Map.singleton name . VR)

-- | param maker for note
pN :: String -> Pattern Note -> ControlPattern
pN name = fmap (Map.singleton name . VN)

pS :: String -> Pattern String -> ControlPattern
pS name = fmap (Map.singleton name . VS)

pX :: String -> Pattern [Word8] -> ControlPattern
pX name = fmap (Map.singleton name . VX)

genericParams = [
  ("s", "toArg", "for internal sound routing"),
  ("f", "from", "for internal sound routing"),
  ("f", "to", "for internal sound routing"),
  ("f", "accelerate", "a pattern of numbers that speed up (or slow down) samples while they play."),
  ("f", "amp", "like @gain@, but linear."),
  ("f", "attack", "a pattern of numbers to specify the attack time (in seconds) of an envelope applied to each sample. Only takes effect if `release` is also specified."),
  ("f", "bandf", "a pattern of numbers from 0 to 1. Sets the center frequency of the band-pass filter."),
  ("f", "bandq", "a pattern of anumbers from 0 to 1. Sets the q-factor of the band-pass filter."),
  ("f", "begin", "a pattern of numbers from 0 to 1. Skips the beginning of each sample, e.g. `0.25` to cut off the first quarter from each sample."),
  ("f", "legato", "controls the amount of overlap between two adjacent sounds"),
  ("f", "clhatdecay", ""),
  ("f", "crush", "bit crushing, a pattern of numbers from 1 (for drastic reduction in bit-depth) to 16 (for barely no reduction)."),
  ("f", "coarse", "fake-resampling, a pattern of numbers for lowering the sample rate, i.e. 1 for original 2 for half, 3 for a third and so on."),
  ("i", "channel", "choose the channel the pattern is sent to in superdirt"),
  ("i", "cut", "In the style of classic drum-machines, `cut` will stop a playing sample as soon as another samples with in same cutgroup is to be played. An example would be an open hi-hat followed by a closed one, essentially muting the open."),
  ("f", "cutoff", "a pattern of numbers from 0 to 1. Applies the cutoff frequency of the low-pass filter."),
  ("f", "cutoffegint", ""),
  ("f", "decay", ""),
  ("f", "delay", "a pattern of numbers from 0 to 1. Sets the level of the delay signal."),
  ("f", "delayfeedback", "a pattern of numbers from 0 to 1. Sets the amount of delay feedback."),
  ("f", "delaytime", "a pattern of numbers from 0 to 1. Sets the length of the delay."),
  ("f", "detune", ""),
  ("f", "djf", "DJ filter, below 0.5 is low pass filter, above is high pass filter."),
  ("f", "dry", "when set to `1` will disable all reverb for this pattern. See `room` and `size` for more information about reverb."),
  ("f", "end", "the same as `begin`, but cuts the end off samples, shortening them; e.g. `0.75` to cut off the last quarter of each sample."),
  ("f", "freq", ""),
  ("f", "gain", "a pattern of numbers that specify volume. Values less than 1 make the sound quieter. Values greater than 1 make the sound louder. For the linear equivalent, see @amp@."),
  ("f", "gate", ""),
  ("f", "hatgrain", ""),
  ("f", "hcutoff", "a pattern of numbers from 0 to 1. Applies the cutoff frequency of the high-pass filter. Also has alias @hpf@"),
  ("f", "hold", "a pattern of numbers to specify the hold time (in seconds) of an envelope applied to each sample. Only takes effect if `attack` and `release` are also specified."),
  ("f", "hresonance", "a pattern of numbers from 0 to 1. Applies the resonance of the high-pass filter. Has alias @hpq@"),
  ("f", "lagogo", ""),
  ("f", "lclap", ""),
  ("f", "lclaves", ""),
  ("f", "lclhat", ""),
  ("f", "lcrash", ""),
  ("f", "leslie", ""),
  ("f", "lrate", ""),
  ("f", "lsize", ""),
  ("f", "lfo", ""),
  ("f", "lfocutoffint", ""),
  ("f", "lfodelay", ""),
  ("f", "lfoint", ""),
  ("f", "lfopitchint", ""),
  ("f", "lfoshape", ""),
  ("f", "lfosync", ""),
  ("f", "lhitom", ""),
  ("f", "lkick", ""),
  ("f", "llotom", ""),
  ("f", "lock", "A pattern of numbers. Specifies whether delaytime is calculated relative to cps. When set to 1, delaytime is a direct multiple of a cycle."),
  ("f", "loop", "loops the sample (from `begin` to `end`) the specified number of times."),
  ("f", "lophat", ""),
  ("f", "lsnare", ""),
  ("note", "n", "The note or sample number to choose for a synth or sampleset"),
  ("note", "note", "The note or pitch to play a sound or synth with"),
degree, mtranspose, ctranspose, harmonic, stepsPerOctave, octaveR
degree = pF "degree"
mtranspose = pF "mtranspose"
ctranspose = pF "ctranspose"
harmonic = pF "ctranspose"
stepsPerOctave = pF "stepsPerOctave"
octaveRatio = pF "octaveRatio"

  ("f", "nudge", "Nudges events into the future by the specified number of seconds. Negative numbers work up to a point as well (due to internal latency)"),
  ("i", "octave", ""),
  ("f", "offset", ""),
  ("f", "ophatdecay", ""),
  ("i", "orbit", "a pattern of numbers. An `orbit` is a global parameter context for patterns. Patterns with the same orbit will share hardware output bus offset and global effects, e.g. reverb and delay. The maximum number of orbits is specified in the superdirt startup, numbers higher than maximum will wrap around."),
  ("f", "overgain", ""),
  ("f", "overshape", ""),
  ("f", "pan", "a pattern of numbers between 0 and 1, from left to right (assuming stereo), once round a circle (assuming multichannel)"),
  ("f", "panspan", "a pattern of numbers between -inf and inf, which controls how much multichannel output is fanned out (negative is backwards ordering)"),
  ("f", "pansplay", "a pattern of numbers between 0.0 and 1.0, which controls the multichannel spread range (multichannel only)"),
  ("f", "panwidth", "a pattern of numbers between 0.0 and inf, which controls how much each channel is distributed over neighbours (multichannel only)"),
  ("f", "panorient", "a pattern of numbers between -1.0 and 1.0, which controls the relative position of the centre pan in a pair of adjacent speakers (multichannel only)"),
  ("f", "pitch1", ""),
  ("f", "pitch2", ""),
  ("f", "pitch3", ""),
  ("f", "portamento", ""),
  ("f", "rate", "used in SuperDirt softsynths as a control rate or 'speed'"),
  ("f", "release", "a pattern of numbers to specify the release time (in seconds) of an envelope applied to each sample. Only takes effect if `attack` is also specified."),
  ("f", "resonance", "a pattern of numbers from 0 to 1. Specifies the resonance of the low-pass filter."),
  ("f", "room", "a pattern of numbers from 0 to 1. Sets the level of reverb."),
  ("f", "sagogo", ""),
  ("f", "sclap", ""),
  ("f", "sclaves", ""),
  ("f", "scrash", ""),
  ("f", "semitone", ""),
  ("f", "shape", "wave shaping distortion, a pattern of numbers from 0 for no distortion up to 1 for loads of distortion."),
  ("f", "size", "a pattern of numbers from 0 to 1. Sets the perceptual size (reverb time) of the `room` to be used in reverb."),
  ("f", "slide", ""),
  ("f", "speed", "a pattern of numbers which changes the speed of sample playback, i.e. a cheap way of changing pitch. Negative values will play the sample backwards!"),
  ("f", "squiz", ""),
  ("f", "stutterdepth", ""),
  ("f", "stuttertime", ""),
  ("f", "sustain", ""),
  ("f", "tomdecay", ""),
  ("s", "unit", "used in conjunction with `speed`, accepts values of "r" (rate, default behavior), "c" (cycles), or "s" (seconds). Using `unit "c"` means `speed` will be interpreted in units of cycles, e.g. `speed "1"` means samples will be stretched to fill a cycle. Using `unit "s"` means the playback speed will be adjusted so that the duration is the number of seconds specified by `speed`."),
  ("f", "velocity", ""),
  ("f", "vcfegint", ""),
  ("f", "vcoegint", ""),
  ("f", "voice", ""),
  ("s", "vowel", "formant filter to make things sound like vowels, a pattern of either `a`, `e`, `i`, `o` or `u`. Use a rest (`~`) for no effect."),
  ("f", "waveloss", ""),
  ("f", "dur", ""),
  ("f", "modwheel", ""),
  ("f", "expression", ""),
  ("f", "sustainpedal", ""),
  ("f", "tremolodepth", "Tremolo Audio DSP effect | params are 'tremolorate' and 'tremolodepth'"),
  ("f", "tremolorate", "Tremolo Audio DSP effect | params are 'tremolorate' and 'tremolodepth'"),
  ("f", "phaserdepth", "Phaser Audio DSP effect | params are 'phaserrate' and 'phaserdepth'"),
  ("f", "phaserrate", "Phaser Audio DSP effect | params are 'phaserrate' and 'phaserdepth'"),
  ("f", "fshift", "frequency shifter"),
  ("f", "fshiftnote", "frequency shifter"),
  ("f", "fshiftphase", "frequency shifter"),

-- triode (tube distortion)
  ("f", "triode", ""),
triode = pF "triode"
-- krush (like Sonic Pi's shape/bass enhancer)
krush,   ("f", "kcutoff", ""),
krush = pF "krush"
kcutoff = pF "kcutoff"
-- octer (like Sonic Pi's octaver effect)
octer, octersub,   ("f", "octersubsub", ""),
octer = pF "octer"
octersub = pF "octersub"
octersubsub = pF "octersubsub"
-- ring modulation
ring, ringf,   ("f", "ringdf", ""),
ring = pF "ring"
ringf = pF "ringf"
ringdf = pF "ringdf"
-- noisy fuzzy distortion
  ("f", "distort", ""),
distort = pF "distort"

-- Spectral freeze
  ("f", "freeze", ""),
freeze = pF "freeze"

-- Spectral delay
  ("f", "xsdelay", ""),
xsdelay = pF "xsdelay"

  ("f", "tsdelay", ""),
tsdelay = pF "tsdelay"

-- Spectral conform
  ("f", "real", ""),
real = pF "real"

  ("f", "imag", ""),
imag = pF "imag"

-- Spectral enhance
  ("f", "enhance", ""),
enhance = pF "enhance"

  ("f", "partials", ""),
partials = pF "partials"

-- Spectral comb
  ("f", "comb", ""),
comb = pF "comb"

-- Spectral smear
  ("f", "smear", ""),
smear = pF "smear"

-- Spectral scramble
  ("f", "scram", ""),
scram = pF "scram"

-- Spectral binshift
  ("f", "binshift", ""),
binshift = pF "binshift"

-- High pass sort of spectral filter
  ("f", "hbrick", ""),
hbrick = pF "hbrick"

-- Low pass sort of spectral filter
  ("f", "lbrick", ""),
lbrick = pF "lbrick"

-- aliases
att, bpf, bpq, chdecay, ctf, ctfg, delayfb, dfb, delayt, dt, det, gat, hg, hpf, hpq, lag, lbd, lch, lcl, lcp, lcr, lfoc, lfoi
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
dfb = delayfeedback
delayt = delaytime
dt = delaytime
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

midinote :: Pattern Note -> ControlPattern
midinote = note . (subtract 60 <$>)

  ("s", "drum", ""),
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
drumN "oh" = 63
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


-- SuperDirt MIDI Params

array :: Pattern [Word8] -> ControlPattern
array = pX "array"
  ("f", "midichan", ""),
midichan = pF "midichan"
  ("f", "control", ""),
control = pF "control"

  ("f", "ccn", ""),
ccn = pF "ccn"
  ("f", "ccv", ""),
ccv = pF "ccv"

polyT  ("f", "ouch", ""),
polyTouch = pF "polyTouch"

  ("f", "midibend", ""),
midibend = pF "midibend"

  ("f", "miditouch", ""),
miditouch = pF "miditouch"

  ("i", "nrpnn", ""),
nrpnn = pI "nrpn"
  ("i", "nrpnv", ""),
nrpnv = pI "val"

ctlN  ("f", "um", ""),
ctlNum = pF "ctlNum"

frameR  ("f", "ate", ""),
frameRate = pF "frameRate"
  ("f", "frames", ""),
frames = pF "frames"
  ("f", "hours", ""),
hours = pF "hours"

  ("s", "midicmd", ""),
midicmd = pS "midicmd"
  ("s", "command", ""),
command = midicmd

  ("f", "minutes", ""),
minutes = pF "minutes"
progN  ("f", "um", ""),
progNum = pF "progNum"
  ("f", "seconds", ""),
seconds = pF "seconds"
songP  ("f", "tr", ""),
songPtr = pF "songPtr"
  ("f", "uid", ""),
uid = pF "uid"
  ("f", "val", ""),
val = pF "val"

{- | `up` is now an alias of `note`. -}
up :: Pattern Note -> ControlPattern
up = note

  ("f", "cps", ""),
cps = pF "cps"

-- generic names for mapping to e.g. midi controls
  ("f", "button0", ""),
button0 = pF "button0"
  ("f", "button1", ""),
button1 = pF "button1"
  ("f", "button2", ""),
button2 = pF "button2"
  ("f", "button3", ""),
button3 = pF "button3"
  ("f", "button4", ""),
button4 = pF "button4"
  ("f", "button5", ""),
button5 = pF "button5"
  ("f", "button6", ""),
button6 = pF "button6"
  ("f", "button7", ""),
button7 = pF "button7"
  ("f", "button8", ""),
button8 = pF "button8"
  ("f", "button9", ""),
button9 = pF "button9"
  ("f", "button10", ""),
button10 = pF "button10"
  ("f", "button11", ""),
button11 = pF "button11"
  ("f", "button12", ""),
button12 = pF "button12"
  ("f", "button13", ""),
button13 = pF "button13"
  ("f", "button14", ""),
button14 = pF "button14"
  ("f", "button15", ""),
button15 = pF "button15"
  ("f", "button16", ""),
button16 = pF "button16"
  ("f", "button17", ""),
button17 = pF "button17"
  ("f", "button18", ""),
button18 = pF "button18"
  ("f", "button19", ""),
button19 = pF "button19"
  ("f", "button20", ""),
button20 = pF "button20"
  ("f", "button21", ""),
button21 = pF "button21"
  ("f", "button22", ""),
button22 = pF "button22"
  ("f", "button23", ""),
button23 = pF "button23"
  ("f", "button24", ""),
button24 = pF "button24"
  ("f", "button25", ""),
button25 = pF "button25"
  ("f", "button26", ""),
button26 = pF "button26"
  ("f", "button27", ""),
button27 = pF "button27"
  ("f", "button28", ""),
button28 = pF "button28"
  ("f", "button29", ""),
button29 = pF "button29"
  ("f", "button30", ""),
button30 = pF "button30"
  ("f", "button31", ""),
button31 = pF "button31"

  ("f", "slider0", ""),
slider0 = pF "slider0"
  ("f", "slider1", ""),
slider1 = pF "slider1"
  ("f", "slider2", ""),
slider2 = pF "slider2"
  ("f", "slider3", ""),
slider3 = pF "slider3"
  ("f", "slider4", ""),
slider4 = pF "slider4"
  ("f", "slider5", ""),
slider5 = pF "slider5"
  ("f", "slider6", ""),
slider6 = pF "slider6"
  ("f", "slider7", ""),
slider7 = pF "slider7"
  ("f", "slider8", ""),
slider8 = pF "slider8"
  ("f", "slider9", ""),
slider9 = pF "slider9"
  ("f", "slider10", ""),
slider10 = pF "slider10"
  ("f", "slider11", ""),
slider11 = pF "slider11"
  ("f", "slider12", ""),
slider12 = pF "slider12"
  ("f", "slider13", ""),
slider13 = pF "slider13"
  ("f", "slider14", ""),
slider14 = pF "slider14"
  ("f", "slider15", ""),
slider15 = pF "slider15"
  ("f", "slider16", ""),
slider16 = pF "slider16"
  ("f", "slider17", ""),
slider17 = pF "slider17"
  ("f", "slider18", ""),
slider18 = pF "slider18"
  ("f", "slider19", ""),
slider19 = pF "slider19"
  ("f", "slider20", ""),
slider20 = pF "slider20"
  ("f", "slider21", ""),
slider21 = pF "slider21"
  ("f", "slider22", ""),
slider22 = pF "slider22"
  ("f", "slider23", ""),
slider23 = pF "slider23"
  ("f", "slider24", ""),
slider24 = pF "slider24"
  ("f", "slider25", ""),
slider25 = pF "slider25"
  ("f", "slider26", ""),
slider26 = pF "slider26"
  ("f", "slider27", ""),
slider27 = pF "slider27"
  ("f", "slider28", ""),
slider28 = pF "slider28"
  ("f", "slider29", ""),
slider29 = pF "slider29"
  ("f", "slider30", ""),
slider30 = pF "slider30"
  ("f", "slider31", ""),
slider31 = pF "slider31"

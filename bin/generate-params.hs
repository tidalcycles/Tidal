
-- This can be run with e.g.:
-- runhaskell generate-params.txt > ../src/Sound/Tidal/Params.hs

import Data.List
import Data.Function
import System.IO

toType :: String -> String
toType "s" = "Pattern String"
toType "f" = "Pattern Double"
toType "i" = "Pattern Int"
toType "note" = "Pattern Note"
toType "[word8]" = "Pattern [Word8]"

toFunc :: String -> String
toFunc "s" = "pS"
toFunc "f" = "pF"
toFunc "i" = "pI"
toFunc "note" = "pN"
toFunc "[word8]" = "pX"

main :: IO ()
main = do header
          putStr controls
          putStr "\n\n\n-- aliases\n\n"
          putStr aliases

header :: IO ()
header = do x <- openFile "params-header.hs" ReadMode
            y <- hGetContents x
            putStr y

controls = intercalate "\n" $ map fs $ sortBy (flip compare `on` (\(_,x,_) -> x)) genericParams
  where fs x = control x ++ bus x
        control (t, name, desc) =
          concat ["-- | " ++ desc ++ "\n",
                  name, " :: ", toType t, " -> ControlPattern\n",
                  name, " = ", toFunc t, " \"", name, "\"\n"
                 ]
        bus (t,name,desc) | elem name nobus = concat [
                              name, "bus :: Pattern Int -> ", toType t, " -> ControlPattern\n",
                              name, "bus _ _ = error $ \"Control parameter '" ++ name ++ "' can't be sent to a bus.\"\n"
                              ]
                          | otherwise = 
          concat [name, "bus :: Pattern Int -> ", toType t, " -> ControlPattern\n",
                  name, "bus busid pat = (", toFunc t, " \"", name, "\" pat) # (pI \"^", name, "\" busid)\n"
                 ]

aliases = intercalate "\n" $ map fs $ sortBy (flip compare `on` (\(_,x,_) -> x)) aliasParams
  where fs (t, from, to) =
          concat [from, " :: ", toType t, " -> ControlPattern\n",
                  from, " = ", to, "\n"
                 ]

nobus = ["midinote",
         "note",
         "n",
         "octave",
         "begin ",
         "end",
         "sustain",
         "legato",
         "loop",
         "unit",
         "length",
         "fadeTime",
         "fadeInTime",
         "speed",
         "endSpeed",
         "gain",
         "overgain",
         "channel",
         "lag",
         "offset"
        ]

genericParams :: [(String, String, String)]
genericParams = [
  ("s", "toArg", "for internal sound routing"),
  ("f", "from", "for internal sound routing"),
  ("f", "to", "for internal sound routing"),
  ("f", "accelerate", "a pattern of numbers that speed up (or slow down) samples while they play."),
  ("f", "amp", "like @gain@, but linear."),
  ("f", "attack", "a pattern of numbers to specify the attack time (in seconds) of an envelope applied to each sample."),
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
  ("f", "degree", ""),
  ("f", "mtranspose", ""),
  ("f", "ctranspose", ""),
  ("f", "harmonic", ""),
  ("f", "stepsPerOctave", ""),
  ("f", "octaveR", ""),
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
  ("f", "release", "a pattern of numbers to specify the release time (in seconds) of an envelope applied to each sample."),
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
  ("s", "unit", "used in conjunction with `speed`, accepts values of \"r\" (rate, default behavior), \"c\" (cycles), or \"s\" (seconds). Using `unit \"c\"` means `speed` will be interpreted in units of cycles, e.g. `speed \"1\"` means samples will be stretched to fill a cycle. Using `unit \"s\"` means the playback speed will be adjusted so that the duration is the number of seconds specified by `speed`."),
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
  ("f", "triode", "tube distortion"),
  ("f", "krush", "shape/bass enhancer"),
  ("f", "kcutoff", ""),
  ("f", "octer", "octaver effect"),
  ("f", "octersub", "octaver effect"),
  ("f", "octersubsub", "octaver effect"),
  ("f", "ring", "ring modulation"),
  ("f", "ringf", "ring modulation"),
  ("f", "ringdf", "ring modulation"),
  ("f", "distort", "noisy fuzzy distortion"),
  ("f", "freeze", "Spectral freeze"),
  ("f", "xsdelay", ""),
  ("f", "tsdelay", ""),
  ("f", "real", "Spectral conform"),
  ("f", "imag", ""),
  ("f", "enhance", "Spectral enhance"),
  ("f", "partials", ""),
  ("f", "comb", "Spectral comb"),
  ("f", "smear", "Spectral smear"),
  ("f", "scram", "Spectral scramble"),
  ("f", "binshift", "Spectral binshift"),
  ("f", "hbrick", "High pass sort of spectral filter"),
  ("f", "lbrick", "Low pass sort of spectral filter"),
  -- SuperDirt MIDI Params
  ("[word8]", "array", ""),
  ("f", "midichan", ""),
  ("f", "control", ""),
  ("f", "ccn", ""),
  ("f", "ccv", ""),
  ("f", "polyTouch", ""),
  ("f", "midibend", ""),
  ("f", "miditouch", ""),
  ("i", "nrpnn", ""),
  ("i", "nrpnv", ""),
  ("f", "ctlNum", ""),
  ("f", "frameRate", ""),
  ("f", "frames", ""),
  ("f", "hours", ""),
  ("s", "midicmd", ""),
  ("f", "minutes", ""),
  ("f", "progNum", ""),
  ("f", "seconds", ""),
  ("f", "songPtr", ""),
  ("f", "uid", ""),
  ("f", "val", ""),
  ("f", "cps", "")
 ]
 ++ (map (\i -> ("f", "slider" ++ show i, "")) [0 .. 63])
 ++ (map (\i -> ("f", "button" ++ show i, "")) [0 .. 63])

aliasParams :: [(String, String, String)]
aliasParams =
  [
  ("s", "s", "sound"),
  ("note", "up", "note"),
  ("f", "att", "attack"),
  ("f", "bpf", "bandf"),
  ("f", "bpq", "bandq"),
  ("f", "chdecay", "clhatdecay"),
  ("f", "ctf", "cutoff"),
  ("f", "ctfg", "cutoffegint"),
  ("f", "delayfb", "delayfeedback"),
  ("f", "dfb", "delayfeedback"),
  ("f", "delayt", "delaytime"),
  ("f", "dt", "delaytime"),
  ("f", "det", "detune"),
  ("f", "gat", "gate"),
  ("f", "hg", "hatgrain"),
  ("f", "hpf", "hcutoff"),
  ("f", "hpq", "hresonance"),
  ("f", "lag", "lagogo"),
  ("f", "lbd", "lkick"),
  ("f", "lch", "lclhat"),
  ("f", "lcl", "lclaves"),
  ("f", "lcp", "lclap"),
  ("f", "lcr", "lcrash"),
  ("f", "lfoc", "lfocutoffint"),
  ("f", "lfoi", "lfoint"),
  ("f", "lfop", "lfopitchint"),
  ("f", "lht", "lhitom"),
  ("f", "llt", "llotom"),
  ("f", "loh", "lophat"),
  ("f", "lpf", "cutoff"),
  ("f", "lpq", "resonance"),
  ("f", "lsn", "lsnare"),
  ("f", "ohdecay", "ophatdecay"),
  ("f", "phasdp", "phaserdepth"),
  ("f", "phasr", "phaserrate"),
  ("f", "pit1", "pitch1"),
  ("f", "pit2", "pitch2"),
  ("f", "pit3", "pitch3"),
  ("f", "por", "portamento"),
  ("f", "rel", "release"),
  ("f", "sag", "sagogo"),
  ("f", "scl", "sclaves"),
  ("f", "scp", "sclap"),
  ("f", "scr", "scrash"),
  ("f", "sz", "size"),
  ("f", "sld", "slide"),
  ("f", "std", "stutterdepth"),
  ("f", "stt", "stuttertime"),
  ("f", "sus", "sustain"),
  ("f", "tdecay", "tomdecay"),
  ("f", "tremdp", "tremolodepth"),
  ("f", "tremr", "tremolorate"),
  ("f", "vcf", "vcfegint"),
  ("f", "vco", "vcoegint"),
  ("f", "voi", "voice")
  ]

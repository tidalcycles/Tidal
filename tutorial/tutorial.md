% Tidal tutorial

Welcome to the Tidal tutorial. Tidal is a mini-language for exploring pattern, designed for use in live coding performance. In this tutorial we'll step through different levels of abstraction, starting with sounds and filters, then sequences of sounds and filters, and moving up to functions for manipulating those sequences, and ending up looking at functions which manipulate other functions. Fun stuff!

# Sounds and effects

With a bit of fiddling, Tidal can be used to pattern the input to any
device which takes MIDI or Open Sound Control input, but the default is the Dirt software sampler. If you followed the install process, you should have Dirt installed and it should be running.

To test it, run the following by typing it into your text editor, holding down ctrl and pressing enter:

```haskell
d1 $ sound "can"
```

You should be able to hear a repeating sample of someone hitting a can. Tidal is designed with repetitive dance music in mind, and will repeat the pattern forever, although you can build a great deal of variety in a single pattern, and also change it while it is running (i.e. live code).

The `can` in the above is the name of the sample you are playing. Well actually it is the name of a folder full of samples. You can find them in the `samples` subfolder of your dirt folder. You can specify a different sample by number, using the colon:

```haskell
d1 $ sound "can:1"
```

Try some different numbers to hear all the different can samples that
come with dirt.

Dirt comes with a wide range of samples to work with, here's some of
them:

```
    flick sid can metal future gabba sn mouth co gretsch mt arp h cp
    cr newnotes bass crow hc tabla bass0 hh bass1 bass2 oc bass3 ho
    odx diphone2 house off ht tink perc bd industrial pluck trump
    printshort jazz voodoo birds3 procshort blip drum jvbass psr
    wobble drumtraks koy rave bottle kurt latibro rm sax lighter lt
    arpy feel less stab ul
```

Replace `can` with one of these to explore.

## Effects

You can also apply a range of effects to change what your sound, er,
sounds like. For example a vowel-like 'formant filter':

```haskell
d1 $ sound "can:1" |+| vowel "a"
```

The `|+|` operator in the above is what binds the sound with the vowel parameter.

Try changing the "a" for other vowels. You can also play the sample faster, which makes it higher in pitch:

```haskell
d1 $ sound "can:1" |+| speed "2"
```

Or slower:

```haskell
d1 $ sound "can:1" |+| speed "0.5"
```

Or even backwards:

```haskell
d1 $ sound "can:1" |+| speed "-1"
```

You can also apply several effects at the same time:

```haskell
d1 $ sound "can:1" |+| vowel "a" |+| speed "-1"
```

Here is the full list of effects you can play with.

Name          | Description
------------- | -----------
accelerate    | a pattern of numbers that speed up (or slow down) samples while they play.
bandf         | a pattern of numbers from 0 to 1. Sets the center frequency of the band-pass filter.
bandq         | a pattern of numbers from 0 to 1. Sets the q-factor of the band-pass filter.
begin         | a pattern of numbers from 0 to 1. Skips the beginning of each sample, e.g. 0.25 to cut off the first quarter from each sample.
coarse        | fake-resampling, a pattern of numbers for lowering the sample rate, i.e. 1 for original 2 for half, 3 for a third and so on.
crush         | bit crushing, a pattern of numbers from 1 for drastic reduction in bit-depth to 16 for barely no reduction.
cutoff        | a pattern of numbers from 0 to 1. Applies the cutoff frequency of the low-pass filter.
delay         | a pattern of numbers from 0 to 1. Sets the level of the delay signal.
delayfeedback | a pattern of numbers from 0 to 1. Sets the amount of delay feedback.
delaytime     | a pattern of numbers from 0 to 1. Sets the length of the delay.
end           | the same as begin, but cuts the end off samples, shortening them; e.g. 0.75 to cut off the last quarter of each sample.
gain          | a pattern of numbers that specify volume. Values less than 1 make the sound quieter. Values greater than 1 make the sound louder.
hcutoff       | a pattern of numbers from 0 to 1. Applies the cutoff frequency of the high-pass filter.
hresonance    | a pattern of numbers from 0 to 1. Applies the resonance of the high-pass filter.
pan           | a pattern of numbers between 0 and 1, from left to right (assuming stereo)
resonance     | a pattern of numbers from 0 to 1. Applies the resonance of the low-pass filter.
shape         | wave shaping distortion, a pattern of numbers from 0 for no distortion up to 1 for loads of distortion (watch your speakers!)
sound         | a pattern of strings representing sound sample names (required)
speed         | a pattern of numbers from 0 to 1, which changes the speed of sample playback, i.e. a cheap way of changing pitch
vowel         | formant filter to make things sound like vowels, a pattern of either a, e, i, o or u. Use a rest (~) for no effect.

# Sequences

You can get a long way with working with

# Functions

# Meta-functions


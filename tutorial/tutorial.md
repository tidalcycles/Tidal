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

You're probably bored of hearing the same sample over and over by now, let's quickly move on to 
sequences. Tidal sequences allow you to string samples together, stretch the sequences out and 
stack them up in a variety of interesting ways, as well as start mixing in randomisation.

You can make a tidal cycle with more than one sample just like this:

```haskell
d1 $ sound "drum drum:1"
```

Kick and snare forever!

You'll notice that however many things you put into a Tidal pattern, it still takes up the same 
amount of time. For example the following fits three sounds into the same cycle duration:

```haskell
d1 $ sound "drum drum:1 can"
```

The `~` symbol represents a rest, or pause, e.g.:

```haskell
d1 $ sound "drum drum:1 ~"
```

You can play around with some more off-kilter patterns, for example this one which has seven steps in it:

```haskell
d1 $ sound "drum ~ can ~ ~ drum:1 ~"
```

You can take one step in a pattern and subdivide it into substeps, for example in the following the 
three `can` samples are played inside the same amount of time that each `drum` sample does:

```haskell
d1 $ sound "drum drum [can can:4 can:5] drum"
```

As you can see the square brackets give the start and end of a subdivision. Actually you can keep going, 
and subdivide a step within a subdivision:

```haskell
d1 $ sound "drum drum [can [can:4 can:6 can:3] can:5] drum"
```

Square brackets allow you to specify more than one pattern in a subdivision by separating them with 
a comma:

```haskell
d1 $ sound "drum [can cp, can bd can:5]"
```

As you can hear, the two patterns are layered up. Because they are different lengths (one with two 
sounds, the other with three), you can get an interesting polyrhythmic effect. You can hear this better
if you just have a single subdivision like this:

```haskell
d1 $ sound "[can cp, can bd can:5]"
```

If you use curly brackets rather than square brackets the subpatterns are layered up in a different way, 
so that the sounds inside align:

```haskell
d1 $ sound "{can can:2, can bd can:5}"
```


# Functions

# Meta-functions


<img src="https://raw2.github.com/yaxu/Tidal/master/doc/tidal.png" />

# Tidal: Domain specific language for live coding of pattern

Homepage and mailing list: <http://yaxu.org/tidal/>

Tidal is a language for live coding pattern, embedded in the Haskell
language. You don't really have to learn Haskell to use Tidal, but it
might help to pick up an introduction. You could try Graham Hutton's
"Programming in Haskell" or Miran Lipovača's "Learn you a Haskell for
Great Good" (which has a free online version). Or, you could just try
learning enough syntax just by playing around with Tidal.

# Installation

Linux installation:
<https://github.com/yaxu/Tidal/blob/master/doc/install-linux.md>

Mac OS X installation:
<https://github.com/yaxu/Tidal/blob/master/doc/install-osx.md>

Windows installation:
<https://github.com/yaxu/Tidal/blob/master/doc/install-windows.md>

Feel free to ask questions and share problems and success stories on
the mailing list.

The above instructions, and the rest of this document, assumes you're using the emacs editor. Emacs is a long-lived and rather complex beast. If you're new to emacs, you can bring up a tutorial by pressing `ctrl-h`, and then `t`. If you're looking for a VIM plugin, check <a href="http://lurk.org/groups/tidal/messages/topic/5F3bHtJPs6NRmm0b2VyQ8Z/">this forum thread</a>.

# Sequences

Tidal starts with nine connections to the dirt synthesiser, named from
`d1` to `d9`. Here's a minimal example, that plays a bass drum every loop:

~~~~ {.haskell}
d1 $ sound "bd"
~~~~

In the above, `sound` tells us we're making a pattern of sounds, and
`"bd"` is a pattern that contains a single sound. `bd` is a sample of
a bass drum. To run the code, use `Ctrl-C` then `Ctrl-C`.

*In case you're wondering, the `$` character in the above is Haskell
syntax, which just means "give the result of the right to the function
on the left". An alternative would have been to do without the `$` by
wrapping everything on the right in parenthesis: `d1 (sound "bd")`*

We can pick variations of a sound by adding a colon (`:`) then a
number, for example this picks the fourth bass drum (it counts from
0, so :3 gives you the fourth sound):

~~~~ {.haskell}
d1 $ sound "bd:3"
~~~~

Putting things in quotes actually defines a sequence. For example, the
following gives you a pattern of bass drum then snare:

~~~~ {.haskell}
d1 $ sound "bd sn"
~~~~

When you do `Ctrl-C Ctrl-C` on the above, you are replacing the
previous pattern with another one on-the-fly. Congratulations, you're
live coding.

The `sound` function in the above is just one possible parameter that
we can send to the synth. Below show a couple more, `pan` and `vowel`:

~~~~ {.haskell}
d1 $ sound "bd sn sn"
   |+| vowel "a o e"
   |+| pan "0 0.5 1"
~~~~

NOTE: `Ctrl-C Ctrl-C` won't work on the above, because it goes over
more than one line. Instead, do `Ctrl-C Ctrl-E` to run the whole
block. However, note that there must be blank lines surrounding the
block (which also means that patterns cannot contain blank lines).

Note that for `pan`, when working in stereo, that `0` means hard left,
`1` means hard right, and `0.5` means centre.

When specifying a sequence you can group together several events to
play inside a single event by using square brackets:

~~~~ {.haskell}
d1 $ sound "[bd sn sn] sn"
~~~~

This is good for creating compound time signatures (sn = snare, cp = clap):

~~~~ {.haskell}
d1 $ sound "[bd sn sn] [cp cp]"
~~~~

And you put events inside events to create any level of detail:

~~~~ {.haskell}
d1 $ sound "[bd bd] [bd [sn [sn sn] sn] sn]"
~~~~

You can also layer up several loops, by using commas to separate the
different parts:

~~~~ {.haskell}
d1 $ sound "[bd bd bd, sn cp sn cp]"
~~~~

This would play the sequence `bd bd bd` at the same time as `sn cp sn
cp`. Note that the first sequence only has three events, and the
second one has four. Because tidal ensures both loops fit inside same
duration, you end up with a polyrhythm.

Try replacing the square brackets with curly brackets:

~~~~ {.haskell}
d1 $ sound "{bd ht lt, sn cp}"
~~~~

This is a different way of specifying a polyrhythm. Instead of both
parts taking up the same amount of time, each event within the second
part takes up the same amount of time as one (top-level) event in the
first part. You can embed these different forms inside each other:

~~~~ {.haskell}
d1 $ sound "{bd [ht sn, lt mt ht] lt, sn cp}"
~~~~

By default, the number of steps in the first part (in this case, 3) is taken as the number of events per cycle used for the other parts. To specify a different number of steps per cycle, you can use `%`, like this:

~~~~ {.haskell}
d1 $ sound "{bd [ht sn, lt mt ht] lt, sn cp}%5"
~~~~

In the above example, five events will be played from each part, in rotation, every cycle.

You can make parts of patterns repeat by using `*`, for example the
following expressions produce the same pattern:

~~~~ {.haskell}
d1 $ sound "[bd bd bd, sn cp sn cp]"

d1 $ sound "[bd*3, [sn cp]*2]"
~~~~

Conversely, you can slow down patterns by using `/`, the following
pattern plays part of each subpattern each cycle:

~~~~ {.haskell}
d1 $ sound "[bd sn sn*3]/2 [bd sn*3 bd*4]/3"
~~~~

# Peace and quiet with silence and hush

An empty pattern is defined as `silence`, so if you want to 'switch
off' a pattern, you can just set it to that:

~~~~ {.haskell}
d1 silence
~~~~

If you want to set all the connections (from `d1` to `d9`) to silence
at once, there's a single-word shortcut for that:

~~~~ {.haskell}
hush
~~~~

You can also isolate a single connection and silence all others with
the `solo` function:

~~~~ {.haskell}
solo $ d1 $ sound "bd sn"
~~~~

# Tempo

You can change the cycles per second (cps) like this:

~~~~ {.haskell}
cps 0.5
~~~~

The above would give a rate of one cycle every two seconds. If you prefer to think in cycles per minute, simply divide by 60:

~~~~ {.haskell}
cps (30 / 60)
~~~~

If you wish to think in terms of 'beats' rather than cycles, then decide how many beats per cycle you expect to work with, and divide accordingly.

# Samples

All the samples can be found in the `samples` subfolder of the Dirt
distribution.  Here's some you could try:

    flick sid can metal future gabba sn mouth co gretsch mt arp h cp
    cr newnotes bass crow hc tabla bass0 hh bass1 bass2 oc bass3 ho
    odx diphone2 house off ht tink perc bd industrial pluck trump
    printshort jazz voodoo birds3 procshort blip drum jvbass psr
    wobble drumtraks koy rave bottle kurt latibro rm sax lighter lt
    arpy feel less stab ul

Each one is a folder containing one or more wav files. For example
when you put `bd:1` in a sequence, you're picking up the second wav
file in the `bd` folder. If you ask for the ninth sample and there are
only seven in the folder, it'll wrap around and play the second one.

If you want to add your own samples, just create a new folder in the
samples director, and put `wav` files in it.

# Continuous patterns

As well as making patterns as sequences, we can also use continuous
patterns. This makes particular sense for parameters such as `pan`
(for panning sounds between speakers) and `shape` (for adding
distortion) which are patterns of numbers.

~~~~ {.haskell}
d1 $ sound "[bd bd] [bd [sn [sn sn] sn] sn]"
   |+| pan sinewave1
   |+| shape sinewave1
~~~~

The above uses the pattern `sinewave1` to continuously pan between the
left and right speaker. You could also try out `triwave1` and
`squarewave1`.  The functions `sinewave`, `triwave` and `squarewave`
also exist, but they go between `-1` and `1`, which is often not what
you want.

# Transforming patterns

Tidal comes into its own when you start building things up with
functions which transform the patterns in various ways.

For example, `rev` reverses a pattern:

~~~~ {.haskell}
d1 $ rev (sound "[bd bd] [bd [sn [sn sn] sn] sn]")
~~~~

That's not so exciting, but things get more interesting when this is
used in combination with another function. For example `every` takes two
parameters, a number, a function and a pattern to apply the function
to. The number specifies how often the function is applied to the
pattern. For example, the following reverses the pattern every fourth
repetition:

~~~~ {.haskell}
d1 $ every 4 (rev) (sound "bd*2 [bd [sn sn*2 sn] sn]")
~~~~

You can also slow down or speed up the playback of a pattern, this
makes it a quarter of the speed:

~~~~ {.haskell}
d1 $ slow 4 $ sound "bd*2 [bd [sn sn*2 sn] sn]"
~~~~

And this four times the speed:

~~~~ {.haskell}
d1 $ density 4 $ sound "bd*2 [bd [sn sn*2 sn] sn]"
~~~~

Note that `slow 0.25` would do exactly the same as `density 4`.

Again, this can be applied selectively:

~~~~ {.haskell}
d1 $ every 4 (density 4) $ sound "bd*2 [bd [sn sn*2 sn] sn]"
~~~~

Note the use of parenthesis around `(density 4)`, this is needed, to
group together the function `density` with its parameter `4`, before
being passed as a parameter to the function `every`.

Instead of putting transformations up front, separated by the pattern
by the `$` symbol, you can put them inside the pattern, for example:

~~~~ {.haskell}
d1 $ sound (every 4 (density 4) "bd*2 [bd [sn sn*2 sn] sn]")
   |+| pan sinewave1
~~~~

In the above example the transformation is applied inside the `sound`
parameter to d1, and therefore has no effect on the `pan`
parameter. Again, parenthesis is required to both group together
`(density 4)` before passing as a parameter to `every`, and also
around `every` and its parameters before passing to its function
`sound`.

~~~~ {.haskell}
d1 $ sound (every 4 (density 4) "bd*2 [bd [sn sn*2 sn] sn]")
   |+| pan (slow 16 sinewave1)
~~~~

In the above, the sinewave pan has been slowed down, so that the
transition between speakers happens over 16 loops.

# Mapping over patterns

Sometimes you want to transform all the events inside a pattern, and
not the time structure of the pattern itself. For example, if you
wanted to pass a sinewave to `shape`, but wanted the sinewave to go
from `0` to `0.5` rather than from `0` to `1`, you could do this:

~~~~ {.haskell}
d1 $ sound "bd*2 [bd [sn sn*2 sn] sn]"))
   |+| shape ((/ 2) <$> sinewave1)
~~~~

The above applies the function `(/ 2)` (which simply means divide by
two), to all the values inside the `sinewave1` pattern.

# Synth Parameters

Synth parameters generate or affect sample playback. These are the
synthesis parameters you can use:

* `accelerate` - a pattern of numbers that speed up (or slow down) samples while they play.
* `bandf` - a pattern of numbers from 0 to 1. Sets the center frequency of the band-pass filter.
* `bandq` - a pattern of numbers from 0 to 1. Sets the q-factor of the band-pass filter.
* `begin` - a pattern of numbers from 0 to 1. Skips the beginning of each sample, e.g. `0.25` to cut off the first quarter from each sample.
* `crush` - bit crushing, a pattern of numbers from 1 for drastic reduction in bit-depth to 16 for barely no reduction.
* `coarse` - fake-resampling, a pattern of numbers for lowering the sample rate, i.e. 1 for original 2 for half, 3 for a third and so on.
* `cutoff` - a pattern of numbers from 0 to 1. Applies the cutoff frequency of the low-pass filter.
* `delay` - a pattern of numbers from 0 to 1. Sets the level of the delay signal.
* `delayfeedback` - a pattern of numbers from 0 to 1. Sets the amount of delay feedback.
* `delaytime` - a pattern of numbers from 0 to 1. Sets the length of the delay.
* `end` - the same as `begin`, but cuts the end off samples, shortening them;
  e.g. `0.75` to cut off the last quarter of each sample.
* `gain` - a pattern of numbers that specify volume. Values less than 1 make the sound quieter. Values greater than 1 make the sound louder.
* `hcutoff` - a pattern of numbers from 0 to 1. Applies the cutoff frequency of the high-pass filter.
* `hresonance` - a pattern of numbers from 0 to 1. Applies the resonance of the high-pass filter.
* `pan` - a pattern of numbers between 0 and 1, from left to right (assuming stereo)
* `resonance` - a pattern of numbers from 0 to 1. Applies the resonance of the low-pass filter.
* `shape` - wave shaping distortion, a pattern of numbers from 0 for no distortion up to 1 for loads of distortion
* `sound` - a pattern of strings representing sound sample names (required)
* `speed` - a pattern of numbers from 0 to 1, which changes the speed of sample playback, i.e. a cheap way of changing pitch
* `vowel` - formant filter to make things sound like vowels, a pattern of either `a`, `e`, `i`, `o` or `u`. Use a rest (`~`) for no effect.

# Pattern transformers

Pattern transformers are functions that take a pattern as input and transform
it into a new pattern.

In the following, functions are shown with their Haskell type and a
short description of how they work.

## Beat rotation

~~~~ {.haskell}
(<~) :: Time -> Pattern a -> Pattern a
~~~~

or

~~~~ {.haskell}
(~>) :: Time -> Pattern a -> Pattern a
~~~~

(The above means that `<~` and `~>` are functions that are given a
time value and a pattern of any type, and returns a pattern of the
same type.)

Rotate a loop either to the left or the right.

Example:

~~~~ {.haskell}
d1 $ every 4 (0.25 <~) $ sound (density 2 "bd sn kurt")
~~~~

## brak

~~~~ {.haskell}
brak :: Pattern a -> Pattern a
~~~~

(The above means that `brak` is a function from patterns of any type,
to a pattern of the same type.)

Make a pattern sound a bit like a breakbeat

Example:

~~~~ {.haskell}
d1 $ sound (brak "bd sn kurt")
~~~~


## chop

~~~~ {.haskell}
chop :: Int -> OscPattern -> OscPattern
~~~~

`chop` granualizes every sample in place as it is played. Use an integer
value to specify how manu granules each sample is chopped into:

~~~~ {.haskell}
d1 $ chop 16 $ sound "arpy arp feel*4 arpy*4"
~~~~

Different values of `chop` can yield very different results, depending
on the samples used:

~~~~ {.haskell}
d1 $ chop 16 $ sound (samples "arpy*8" (run 16))
d1 $ chop 32 $ sound (samples "arpy*8" (run 16))
d1 $ chop 256 $ sound "bd*4 [sn cp] [hh future]*2 [cp feel]"
~~~~

## degrade and degradeBy

~~~~ {.haskell}
degrade :: Pattern a -> Pattern a
degradeBy :: Double -> Pattern a -> Pattern a
~~~~

`degrade` randomly removes events from a pattern 50% of the time:

~~~~ {.haskell}
d1 $ slow 2 $ degrade $ sound "[[[feel:5*8,feel*3] feel:3*8], feel*4]"
   |+| accelerate "-6"
   |+| speed "2"
~~~~

The shorthand syntax for `degrade` is a question mark: `?`. Using `?`
will allow you to randomly remove events from a portion of a pattern:

~~~~ {.haskell}
d1 $ slow 2 $ sound "bd ~ sn bd ~ bd? [sn bd?] ~"
~~~~

You can also use `?` to randomly remove events from entire sub-patterns:

~~~~ {.haskell}
d1 $ slow 2 $ sound "[[[feel:5*8,feel*3] feel:3*8]?, feel*4]"
~~~~

`degradeBy` allows you to control the percentage of events that
are removed. For example, to remove events 90% of the time:

~~~~ {.haskell}
d1 $ slow 2 $ degradeBy 0.9 $ sound "[[[feel:5*8,feel*3] feel:3*8], feel*4]"
   |+| accelerate "-6"
   |+| speed "2"
~~~~

## density

~~~~ {.haskell}
density :: Time -> Pattern a -> Pattern a
~~~~


Speed up a pattern.

Example:

~~~~ {.haskell}
d1 $ sound (density 2 "bd sn kurt")
   |+| density 3 (vowel "a e o")
~~~~

Also, see `slow`.

## every nth repetition, do this

~~~~ {.haskell}
every :: Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
~~~~

(The above means `every` is a function that is given an integer number, a
function which transforms a pattern, and an actual pattern, and
returns a pattern of the same type.)

Transform the given pattern using the given function, but only every
given number of repetitions.

Example:

~~~~ {.haskell}
d1 $ sound (every 3 (density 2) "bd sn kurt")
~~~~

Also, see `whenmod`.

## interlace

~~~~ {.haskell}
interlace :: OscPattern -> OscPattern -> OscPattern
~~~~

(A function that takes two OscPatterns, and blends them together into
a new OscPattern. An OscPattern is basically a pattern of messages to
a synthesiser.)

Shifts between the two given patterns, using distortion.

Example:

~~~~ {.haskell}
d1 $ interlace (sound  "bd sn kurt") (every 3 rev $ sound  "bd sn:2")
~~~~

## iter

~~~~ {.haskell}
iter :: Int -> Pattern a -> Pattern a
~~~~

Divides a pattern into a given number of subdivisions, plays the subdivisions
in order, but increments the starting subdivision each cycle. The pattern
wraps to the first subdivision after the last subdivision is played.

Example:

~~~~ {.haskell}
d1 $ iter 4 $ sound "bd hh sn cp"
~~~~

This will produce the following over four cycles:

~~~~ {.haskell}
bd hh sn cp
hh sn cp bd
sn cp bd hh
cp bd hh sn
~~~~

## rev

~~~~ {.haskell}
rev :: Pattern a -> Pattern a
~~~~

Reverse a pattern

Examples:

~~~~ {.haskell}
d1 $ every 3 (rev) $ sound (density 2 "bd sn kurt")
~~~~

## slow

~~~~ {.haskell}
slow :: Time -> Pattern a -> Pattern a
~~~~

Slow down a pattern.

Example:

~~~~ {.haskell}
d1 $ sound (slow 2 "bd sn kurt")
   |+| slow 3 (vowel "a e o")
~~~~

Slow also accepts numbers between 0 and 1, which causes the pattern to speed up:

~~~~ {.haskell}
d1 $ sound (slow 0.5 "bd sn kurt")
   |+| slow 0.75 (vowel "a e o")
~~~~

Also, see `density`.

## slowspread

~~~~ {.haskell}
slowspread :: (a -> t -> Pattern b) -> [a] -> t -> Pattern b
~~~~

`slowspread` takes a list of pattern transforms and applies them one at a time, per cycle,
then repeats.

Example:

~~~~ {.haskell}
d1 $ slowspread ($) [density 2, rev, slow 2, striate 3, (|+| speed "0.8")]
    $ sound "[bd*2 [~ bd]] [sn future]*2 cp jvbass*4"
~~~~

Above, the pattern will have these transforms applied to it, one at a time, per cycle:

* cycle 1: `density 2` - pattern will increase in speed
* cycle 2: `rev` - pattern will be reversed
* cycle 3: `slow 2` - pattern will decrease in speed
* cycle 4: `striate 3` - pattern will be granualized
* cycle 5: `(|+| speed "0.8")` - pattern samples will be played back more slowly

After `(|+| speed "0.8")`, the transforms will repeat and start at `density 2` again.

## smash

~~~~ {.haskell}
smash :: Int -> [Time] -> OscPattern -> OscPattern
~~~~

Smash is a combination of `spread` and `striate` - it cuts the samples
into the given number of bits, and then cuts between playing the loop
at different speeds according to the values in the list.

So this:

~~~~ {.haskell}
  d1 $ smash 3 [2,3,4] $ sound "ho ho:2 ho:3 hc"
~~~~

Is a bit like this:

~~~~ {.haskell}
  d1 $ spread (slow) [2,3,4] $ striate 3 $ sound "ho ho:2 ho:3 hc"
~~~~

This is quite dancehall:

~~~~ {.haskell}
d1 $ (spread' slow "1%4 2 1 3" $ spread (striate) [2,3,4,1] $ sound
"sn:2 sid:3 cp sid:4")
  |+| speed "[1 2 1 1]/2"
~~~~

## sometimesBy

~~~~ {.haskell}
sometimesBy :: Double -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
~~~~

Use `sometimesBy` to apply a given function "sometimes". For example, the
following code results in `density 2` being applied about 25% of the time:

~~~~ {.haskell}
d1 $ sometimesBy 0.25 (density 2) $ sound "bd*8"
~~~~

There are some aliases as well:

~~~~ {.haskell}
sometimes = sometimesBy 0.5
often = sometimesBy 0.75
rarely = sometimesBy 0.25
almostNever = sometimesBy 0.1
almostAlways = sometimesBy 0.9
~~~~




## spread

~~~~ {.haskell}
spread :: (a -> t -> Pattern b) -> [a] -> t -> Pattern b
~~~~

(The above is difficult to describe, if you don't understand Haskell,
just read the description and examples..)

The `spread` function allows you to take a pattern transformation
which takes a parameter, such as `slow`, and provide several
parameters which are switched between. In other words it 'spreads' a
function across several values.

Taking a simple high hat loop as an example:

~~~~ {.haskell}
d1 $ sound "ho ho:2 ho:3 hc"
~~~~

We can slow it down by different amounts, such as by a half:

~~~~ {.haskell}
  d1 $ slow 2 $ sound "ho ho:2 ho:3 hc"
~~~~

Or by four thirds (i.e. speeding it up by a third; `4%3` means four over
three):

~~~~ {.haskell}
  d1 $ slow (4%3) $ sound "ho ho:2 ho:3 hc"
~~~~

But if we use `spread`, we can make a pattern which alternates between
the two speeds:

~~~~ {.haskell}
d1 $ spread slow [2,4%3] $ sound "ho ho:2 ho:3 hc"
~~~~

There's a version of this function, `spread'` (pronounced "spread prime"), which takes a *pattern* of parameters, instead of a list:

~~~~ {.haskell}
d1 $ spread' slow "2 4%3" $ sound "ho ho:2 ho:3 hc"
~~~~

This is quite a messy area of Tidal - due to a slight difference of
implementation this sounds completely different! One advantage of
using `spread'` though is that you can provide polyphonic parameters, e.g.:

~~~~ {.haskell}
d1 $ spread' slow "[2 4%3, 3]" $ sound "ho ho:2 ho:3 hc"
~~~~

## striate

~~~~ {.haskell}
striate :: Int -> OscPattern -> OscPattern
~~~~

Striate is a kind of granulator, for example:

~~~~ {.haskell}
d1 $ striate 3 $ sound "ho ho:2 ho:3 hc"
~~~~

This plays the loop the given number of times, but triggering
progressive portions of each sample. So in this case it plays the loop
three times, the first time playing the first third of each sample,
then the second time playing the second third of each sample, etc..
With the highhat samples in the above example it sounds a bit like
reverb, but it isn't really.

You can also use striate with very long samples, to cut it into short
chunks and pattern those chunks. This is where things get towards
granular synthesis. The following cuts a sample into 128 parts, plays
it over 8 cycles and manipulates those parts by reversing and rotating
the loops.

~~~~ {.haskell}
d1 $  slow 8 $ striate 128 $ sound "bev"
~~~~

The `striate'` function is a variant of `striate` with an extra
parameter, which specifies the length of each part. The `striate'`
function still scans across the sample over a single cycle, but if
each bit is longer, it creates a sort of stuttering effect. For
example the following will cut the bev sample into 32 parts, but each
will be 1/16th of a sample long:

~~~~ {.haskell}
d1 $ slow 32 $ striate' 32 (1/16) $ sound "bev"
~~~~

Note that `striate` uses the `begin` and `end` parameters
internally. This means that if you're using `striate` (or `striate'`)
you probably shouldn't also specify `begin` or `end`.

## stut

~~~~ {.haskell}
stut :: Integer -> Double -> Rational -> OscPattern -> OscPattern
~~~~

Stut applies a type of delay to a pattern. It has three parameters,
which could be called depth, feedback and time. Depth is an integer
and the others floating point. This adds a bit of echo:

~~~~ {.haskell}
d1 $ stut 4 0.5 0.2 $ sound "bd sn"
~~~~

The above results in 4 echos, each one 50% quieter than the last,
with 1/5th of a cycle between them. It is possible to reverse the echo:

~~~~ {.haskell}
d1 $ stut 4 0.5 (-0.2) $ sound "bd sn"
~~~~

## trunc

~~~~ {.haskell}
trunc :: Time -> Pattern a -> Pattern a
~~~~

Truncates a pattern so that only a fraction of the pattern is played.
The following example plays only the first three quarters of the pattern:

~~~~ {.haskell}
d1 $ trunc 0.75 $ sound "bd sn*2 cp hh*4 arpy bd*2 cp bd*2"
~~~~

## wedge

~~~~{.haskell}
wedge :: Time -> Pattern a -> Pattern a -> Pattern a
~~~~

`wedge` combines two patterns by squashing two patterns into a single pattern cycle.
It takes a ratio as the first argument. The ratio determines what percentage of the
pattern cycle is taken up by the first pattern. The second pattern fills in the
remainder of the pattern cycle.

~~~~{.haskell}
d1 $ wedge (1/4) (sound "bd*2 arpy*3 cp sn*2") (sound "odx [feel future]*2 hh hh")
~~~~

## whenmod

~~~~ {.haskell}
whenmod :: Int -> Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
~~~~

`whenmod` has a similar form and behavior to `every`, but requires an
additional number. Applies the function to the pattern, when the
remainder of the current loop number divided by the first parameter,
is less than the second parameter.

For example the following makes every other block of four loops twice
as dense:

~~~~ {.haskell}
d1 $ whenmod 8 4 (density 2) (sound "bd sn kurt")
~~~~


## within

~~~~{.haskell}
within :: Arc -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
~~~~

Use `within` to apply a function to only a part of a pattern. For example, to
apply `density 2` to only the first half of a pattern:

~~~~{.haskell}
d1 $ within (0, 0.5) (density 2) $ sound "bd*2 sn lt mt hh hh hh hh"
~~~~

Or, to apply `(|+| speed "0.5") to only the last quarter of a pattern:

~~~~{.haskell}
d1 $ within (0.75, 1) (|+| speed "0.5") $ sound "bd*2 sn lt mt hh hh hh hh"
~~~~


# Combining patterns

Because Tidal patterns are defined as something called an "applicative
functor", it's easy to combine them. For example, if you have two
patterns of numbers, you can combine the patterns by, for example,
multiplying the numbers inside them together, like this:

~~~~ {.haskell}
d1 $ (brak (sound "bd sn:2 bd sn"))
   |+| pan ((*) <$> sinewave1 <*> (slow 8 $ "0 0.25 0.75"))
~~~~

In the above, the `sinewave1` and the `(slow 8 $ "0 0.25 0.75")`
pattern are multiplied together. Using the <$> and the <*> in this way
turns the `*` operator, which normally works with two numbers, into a
function that instead works on two *patterns* of numbers.

Here's another example of this technique:

~~~~ {.haskell}
d1 $ sound (pick <$> "kurt mouth can*3 sn" <*> slow 7 "0 1 2 3 4")
~~~~

The `pick` function normally just takes the name of a set of samples
(such as `kurt`), and a number, and returns a sample with that
number. Again, using <$> and <*> turns `pick` into a function that
operates on patterns, rather than simple values. In practice, this
means you can pattern sample numbers separately from sample
sets. Because the sample numbers have been slowed down in the above,
an interesting texture results.

By the way, "0 1 2 3 4" in the above could be replaced with the
pattern generator `run 5`.

# Stacking

~~~~ {.haskell}
stack :: [Pattern a] -> Pattern a
~~~~

`stack` takes a list of patterns and combines them into a new pattern by
playing all of the patterns in the list simultaneously.

~~~~ {.haskell}
d1 $ stack [
  sound "bd bd*2",
  sound "hh*2 [sn cp] cp future*4",
  sound (samples "arpy*8", (run 16))
]
~~~~

This is useful if you want to use a transform or synth parameter on the entire
stack:

~~~~ {.haskell}
d1 $ whenmod 5 3 (striate 3) $ stack [
  sound "bd bd*2",
  sound "hh*2 [sn cp] cp future*4",
  sound (samples "arpy*8", (run 16))
] |+| speed "[[1 0.8], [1.5 2]*2]/3"
~~~~

There is a similar function named `seqP` which allows you to define when
a sound within a list starts and ends. The code below contains three
separate patterns in a "stack", but each has different start times
(zero cycles, eight cycles, and sixteen cycles, respectively). All
patterns stop after 128 cycles:

~~~~ {.haskell}
d1 $ seqP [
  (0, 128, sound "bd bd*2"),
  (8, 128, sound "hh*2 [sn cp] cp future*4"),
  (16, 128, sound (samples "arpy*8" (run 16)))
]
~~~~



# Juxtapositions

The `jux` function creates strange stereo effects, by applying a
function to a pattern, but only in the right-hand channel. For
example, the following reverses the pattern on the righthand side:

~~~~ {.haskell}
d1 $ slow 32 $ jux (rev) $ striate' 32 (1/16) $ sound "bev"
~~~~

When passing pattern transforms to functions like `jux` and `every`,
it's possible to chain multiple transforms together with `.`, for
example this both reverses and halves the playback speed of the
pattern in the righthand channel:

~~~~ {.haskell}
d1 $ slow 32 $ jux ((|+| speed "0.5") . rev) $ striate' 32 (1/16) $ sound "bev"
~~~~


# Quantization, event shifting, and patterns as sequences

Patterns are not _really_ sequences, but sometimes you may wish they
were. The functions `preplace` and `protate` let you treat them as
sequences.

Tidal patterns are formally infinite, but usually they repeat after some
time (which is often some integer multiple of cycles). Since a pattern does not
know how long it is, you'll have to specify. Each function comes with a
companion operator which assumes your pattern repeats after one cycle. This
is the case with pattern literals (i.e., `sound "bd bd"` is always a
one cycle pattern).

## preplace and `<~>`

~~~~ {.haskell}
preplace :: (Time, Time) -> Pattern a -> Pattern a -> Pattern a
~~~~

The `preplace` (shorthand `prep`) function combines the timing of one
pattern (the trigger pattern) with event values of another (the sequence
pattern). It does so by replacing the trigger pattern event values with
values, repeating the latter until it has enough. Note that it does not
matter what the values of the trigger pattern are, but the patterns must be
of the same _type_.

Additionally, `preplace` takes a `(Time, Time)` tuple of pattern
lengths (doesn't have to be correct). If you are fine with constraining this
effect to a single cycle you can use `<~>`:

~~~~ {.haskell}
(<~>) :: Pattern a -> Pattern a -> Pattern a
~~~~

Example replacements (`=>` means "becomes"):

~~~~ {.haskell}
"x x" <~> "bd"          => "bd bd"
"x" <~> "bd sn"         => "bd"
"x x ~ x" <~> "bd sn"   => "bd sn ~ bd sn bd ~ sn"
"x(3,8)" <~> "bd"       => "bd ~ ~ bd ~ ~ b ~"
"x(3,8)" <~> "bd bd sn" => "bd ~ ~ bd ~ ~ sn ~"

preplace (1,1) "x [~ x] x x" "bd sn"
  => "bd {~ sn] bd sn"

preplace (1,3) "x x x ~" $ samples "bd sn" $ slow 3 $ run 3
  => "bd:0 sn:0 bd:1 ~ sn:1  bd:2 sn:2 ~ bd:3  ..."
~~~~

Any pattern will do:

~~~~ {.haskell}
d1 $ sound "[jvbass jvbass:5]*3" |+| (shape $ "1 1 1 1 1" <~> "0.2 0.9")
~~~~

## preplaceWith (prw)

`preplaceWith` (shorthand `prw`) is similar to `replace` but takes an
additional combinator function:

~~~~ {.haskell}
preplaceWith :: (a -> b -> c) -> (Time, Time) -> Pattern a -> Pattern b -> Pattern c
preplaceWith1 :: (a -> b -> c) -> Pattern a -> Pattern b -> Pattern c

prw = preplaceWith
prw1 = preplaceWith1
~~~~

`preplaceWith1` assumes both patterns are of length 1. Also note that
patterns don't have to be of the same type anymore.

This can be quite powerful:

~~~~ {.haskell}
prw1 (+) "1 2 3" "2"    => "3 4 5"
prw1 (+) "1 2 3" "1 -1" => "2 1 4 0 3 2"
~~~~

## protate and `<<~`/`~>>`

~~~~ {.haskell}
protate :: Time -> Int -> Pattern a -> Pattern a
protate len rot p = ...
~~~~

The `protate` (shorthand `prot`) function takes a pattern of length `len`
and shifts the events `rot` times to the left without disturbing the time signature.

~~~~ {.haskell}
prot 1 "1 2 ~ 3"                => "2 3 ~ 1"
1 <<~ "1 2 ~ 3"                 => "2 3 ~ 1"
1 ~>> "1 2 ~ 3"                 => "3 1 ~ 2"
2 ~>> "[bd bd] sn ~ hh [~ can]" => "[hh can] bd ~ bd [~ sn]"
~~~~

## prr and prrw

All the above functions rely on `prrw` (pattern replate rotate with) and its
alias `prr`. If you find yourself having to do combinations of the above,
you can use these directly.

~~~~ {.haskell}
-- | @prr rot (blen, vlen) beatPattern valuePattern@: pattern rotate/replace.
prrw :: (a -> b -> c) -> Int -> (Time, Time) -> Pattern a -> Pattern b -> Pattern c
prrw f rot (blen, vlen) beatPattern valuePattern = ...

prr :: Int -> (Time, Time) -> Pattern a -> Pattern a -> Pattern a
~~~~

# Plus more to be discovered!

You can find a stream of minimal cycles written in Tidal in the
following twitter feed:
  <http://twitter.com/tidalcycles/>

You can look for additional information in the tidal wiki:
  <https://github.com/yaxu/Tidal/wiki>

# Acknowledgments

Special thanks to l'ull cec (<http://lullcec.org>) and hangar
(<http://hangar.org>) for supporting the documentation and release of
tidal as part of the ADDICTED2RANDOM project.

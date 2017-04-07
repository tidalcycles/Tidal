% TidalCycles: Patterns I Have Known And Loved

# Introduction: What is TidalCycles?

TidalCycles (or *Tidal* for short) is a strange piece of
software. It's 'embedded' in a programming language (called Haskell),
but many people using it would not describe themselves as
programmers. It's generally thought of as a system for making music,
but many people use it for other things -- making visuals, or
controlling lights, or making fixed images.

What Tidal is really all about is *pattern*. The reason why it looks
like a programming language is that code is all about pattern
too.. But Tidal code probably has more in common with a knitting
pattern than the sourcecode of (say) an e-commerce system.

Pattern is everywhere, in textiles, language, mathematics and of
course music. But what do we mean by pattern? Pattern can be thought
about in four different aspects of behaviour:

[//]: # (TODO Examples for each)

<span class="index" term="pattern" />
*Repetition:* Essential to rhythm, repetition forms the backbone of a
  great deal of music. The clue is in the name - TidalCycles is all
  about repetitive cycles. However that doesn't mean that each cycle
  has to be the same, or that everything has to be locked to a single
  cycle. Far from it, Tidal's conceptual of time is very flexible,
  with great support for polyrhythm, complex meters, and flowing
  (continuous) and grid-based (discrete) patterns, able to be
  subdivided to whatever level of complexity you like.

*Symmetry:* By this we mean rotation as well as reflection, and
  relatedly self-similarity at multiple scales.

*Interference:* Where two or more patterns can be combined, creating
  often astonishing 'higher-order' patterns out of simple parts.  For
  example the moire patterns seen when netting overlaps, or where the
  structure of a textile weave interferes with colour patternings of
  warp and weft threads.

*Deviation:* Where we use the above to set up expectation and sweet
  anticipation, we all too often purposefully break those
  expectations, playing with people's minds. In computing, we call
  this 'random numbers' but it's really about choosing not to make
  decisions, or adding glitches and imperfections.

It's possible to use Tidal to notate music you already have in
mind. However, by exploring and combining such patternings together
you can find music that is beyond your imagination. This is really
what Tidal is made for - exploration and improvisation.

In exploring these different flavours and levels of patternings,
you'll realise that they are never independent. For example how many
different kinds of symmetry can you add to a pattern before they
starts to interefere with each other? How much deviation or
interference can you add before the sense of repetition breaks down?
How long can a repetition be before you stop perceiving it as such?
The answer to all these is generally "not much", and there is much
music to be found on these boundaries.

First of all though we need to cover some basics. Before we get on to
patternings, we need to work out how to play sounds, and how to modify
them through sonic filters and effects. Lets do that now.

# Sounds

<span class="index" term="SuperDirt" />

Tidal does not make sound itself, but sends messages to synthesisers
(hardware or software) to make sound. For the following lets assume
you followed the install instructions and have Tidal set up with
SuperDirt, a synthesiser made for use with Tidal (and vice-versa).

SuperDirt makes sound using two different methods. One is to play
pre-recorded sounds or sound 'samples', usually short ones. The other
is to play sounds which SuperDirt synthesises from scratch. Lets
concentrate on the former, first.

Lets start by playing a bass drum sound:

```
d1 $ sound "bd"
```

> To run the above, paste this in to your editor (probably Atom), and
> hold down ctrl and press enter. If it didn't work, make sure the
> blinking cursor is on the line, and also that there is a blank line
> above and below the line, separating it from any other code.

You should hear a bass drum sound repeating forever. The next thing
you'll want to do is stop it!

## Silence

<span class="index" term="silence" />

To stop a pattern from playing, replace it with silence. Run the
following in the same way as the above:

```
d1 $ silence
```

> You don't have to delete or think about the other code in your
> editor before you run this, by pressing ctrl-enter you are only
> sending Tidal the block of code that the cursor is on. You can see
> which block this is because it flashes momentarily when you press
> ctrl-enter.

## Anatomy of a simple pattern

<span class="index" term="sound" />

Lets go back to our bass drum sound, and try to understand what the
different bits mean. Don't worry if it doesn't make total sense yet,
things will become clearer as you play with them.

```
d1 $ sound "bd"
```

* `d1` is a 'connection' to superdirt. You'll see how to have
  different patterns using different connections later, but for now
  we'll just deal with the first connection, `d1`. It takes one and
  one and only one 'parameter' - a pattern to send to SuperDirt.
* `$` is a bit of useful 'syntax', which you'll get the hang of later on.
  <span class="index" term="sample files" />
  > If you're curious now, think about `$` as telling tidal to pass
  > whatever is on the right hand side of it (in this case `sound "bd"`)
  > to the left of it (`d1`). In other words, `d1` is a 'function' with a
  > single 'parameter', which is the pattern `sound "bd"`. If you didn't
  > have the `$` here, Tidal would try to send `sound` on its own to `d1`,
  > get very confused, and you'd get an error message. An alternative is
  > to wrap the parameter in parenthesis like this `d1 (sound "bd")`, but
  > with more complicated patterns you usually want to reduce the number
  > of brackets.
* `sound` is the name of a synthesiser parameter. There are others for
  changing the sound in different ways, which you'll see later.
* `"bd"` is a pattern of sound names, in this case the single name
  `bd`, which is the name of a bass drum samplebank.

So reading it backwards, we have a pattern `"bd"` which we treat as a
pattern of `sound`s, which we pass to `d1`, which sends it to
superdirt for us.

You can try other sound names in there, for example a snaredrum:

```
d1 $ sound "sd"
```

An important point is that `sd` is the name of not a single sound
samples but a whole pack of samples. You can access the others using a
`:` followed by a number, starting at `0` for the first one. For
example, to play the third sample in the `sd` pack:

```
d1 $ sound "sd:2"
```

> But where are these files? They are simply folders on your
> computer. Have a look at them - open supercollider, and from the
> menu select `File` > `Open user support directory`. A folder window
> should pop up, open `downloaded-quarks` subfolder and then
> `Dirt-Samples` in that. You should now see a large folder full of
> samplepack folders. The sounds are all recorded in the `wav` format,
> and as they're called up by number, the names of the wav files don't
> matter.

<span class="index" term="s" />

You'll also sometimes see the sound specified with the shorthand `s`:

```
d1 $ s "sd:2"
```

<span class="index" term="n" />

Shorter, but perhaps less clear. It's also possible to give the sample
number as a separate pattern, labeled `n` and joined together with
the `#` symbol, like this:

```
d1 $ sound "sd" # n "2"
```

You'll see much more of this `#` symbol later, but we'll leave it to
the side for now.

# Sequencing

<span class="index" term="sequences" />

So far we've just been listening to single, repeating sounds. How
about stringing together different sounds, in order to sequence a
rhythm? Tidal contains a highly expressive mini-language for
describing sequences, which is accessible wherever you see double
quotes.

## Simple sequences

Most simply, you can just list out the sounds you want, and tidal will
play them in turn:

```
d1 $ sound "bd sd"
```

You can put as many things as you like in a sequence:

```
d1 $ sound "bd bd ht lt"
```

You'll notice in the above that if you put four sounds in there, it
goes twice as fast as if you put two in. This is because the sequence
is being squashed to fit into the cycle length, so the more sounds you
put in, the faster they are played:

```
d1 $ sound "bd bd lt sd sd cp bd sd"
```

<span class="index" term="clock" />
<span class="index" term="cps" />
<span class="index" term="bpm" />

> Tidal has a global 'clock', which keeps track of the cycles, which
> you can imagine continually running in the background even when no
> sound is playing. By default, the clock runs at one cycle per
> second. You can control how fast this goes with the `cps` function,
> for example type `cps 0.7` on its own line and run it to slow the
> clock down a bit.  You might be more used to thinking in 'beats per
> minute', but as we have just seen above, Tidal does not have a fixed
> number of beats per bar (or cycle) - that kind of depends on how
> many events you decide to cram in. However if you are making 4/4
> music, you can get a rough approximation by dividing the bpm by 60
> to get beats per second, and the result by 4 to get beats per
> cycle. For example `cps (150 / 60 / 4)` to get 150 bpm.

## Rests and gaps

<span class="index" term="rests" />
<span class="index" term="silence" />

Silence is of course central to music, and in Tidal sequences you can
insert empty gaps with the `~` character.

```
d1 $ sound "bd bd ~ sd sd ht ~ lt"
```

## Grouping and subpatterns

<span class="index" term="subpatterns" />
<span class="index" term="grouping" />

Earlier we said Tidal's conception of time was flexible, lets look at
some of that. First of all, there's the idea of marking out rhythmic
'feet' with a `.` symbol:

```
d1 $ sound "bd bd sd . arpy mt:1 lt sd:1"
```

The above `.` divides the sequence into two halves, but the first half
has three events, and the second has four. This gives an off-kilter
rhythm that you might find pleasing. You can add more feet if
you want:

```
d1 $ sound "bd bd sd . mt sd lt ~ . bd sd"
```

An alternative way of writing the above is this:

```
d1 $ sound "[bd bd sd] [mt sd lt ~] [bd sd]"
```

Instead of marking out the feet with `.`, the sounds are grouped
together by wrapping them in `[` and `]`. The end result is exactly
the same, but the latter approach allows you to put groups within
groups, e.g:

```
d1 $ sound "[bd ~ sd] [hc [mt:1 lt] ~ hc]"
```

You can think of the above as being a sequence with two steps, the first one broken down into a subpattern with three steps `bd`, `~` and `sd` each lasting a sixth of a cycle each, and the second one broken down into four steps `hc`, `[mt:1 lt]`, `~` and `hc`, each lasting an eigth of a cycle each. In the latter subpattern, the second step is broken down further into two steps, each lasting a sixteenth of a cycle.

You can keep going if you want:

```
d1 $ sound "[bd ~ sd] [bd [mt:1 [lt [lt [mt bd]]]] ~ lt:4]"
```

## Parallel patterns, polyphony and polyrhythm

<span class="index" term="polyrhythm" />
<span class="index" term="polyphony" />

Another good thing about grouping with `[` and `]` is that you can put more than one subpattern in there, which will
then be played at the same time:

```
d1 $ sound "[bd bd cp, arpy arpy:1 arpy:3]"
``` 

In the simple example above the sounds line up nicely, but they don't have to, here we put four sounds against three, creating a kind of triplet or polyrhythm:

```
d1 $ sound "[bd bd cp hc, arpy arpy:1 arpy:3]"
``` 

Tidal takes care of spreading them out to fit the enclosing step perfectly. As the subpatterns are the only step in the above pattern, they fill the whole cycle.

We can keep layering up subpatterns simply by using more commas:

```
d1 $ sound "[bd bd cp hc, arpy arpy:1 arpy:3, ~ off]"
``` 

### Polymeter

<span class="index" term="polymeter" />

The subpatterns in `[` and `]` fill all the space available, but there are other ways of arranging subpatterns.

One is `{` and `}`, which is similar to our friends `[` and `]`, but instead of squashing the subpatterns into the same space, it matches up the sub-steps.

Compare this:

```
d1 $ sound "[bd cp, arpy arpy:1 arpy:2 arpy:3]"
```
with this:
```
d1 $ sound "{bd cp, arpy arpy:1 arpy:2 arpy:3}"
``` 

The two subpatterns in the first example line up like this:

|             |                |
|-------------|----------------|
| bd          | cp             |
| arpy arpy:1 | arpy:2  arpy:3 |

Whereas in the second they line up like this:

|      |        |
|------|--------|
| bd   | cp     | 
| arpy | arpy:1 | 

This means some events are left over from the second subpattern, but don't worry, they make an appearance on the following (and then every other) cycle, like this:

|        |        |
|--------|--------|
| bd     | cp     | 
| arpy:2 | arpy:3 | 

In brief, `[]` matches the length of the subpatterns, whereas `{}` does this with the first subpattern, but then the others are matched up event-by-event. This lack of alignment between patterns with different lengths can be broadly thought of in musical terms as 'polymeter.

# Dimensions of sound - SuperDirt parameters

<span class="index" term="SuperDirt parameters" />

We have so far seen two parameters that can be sent to SuperDirt, the `sound` (aka `s`) one, and briefly, the `n` one. There's a whole lot more of them, listed out later in this section. Feel free to have a sneak peak ahead play with some of them.

Lets spend some time now though looking at how different parameters combine. The first thing to bear in mind is that Tidal is all about combining patterns. Each parameter is itself a pattern, and contains a pattern. 

```
d1 $ sound "rash rash" # n "50 38"
```

Breaking the above down, we can actually isolate five patterns from the above:

1. `"50 52"` - A pattern of whole numbers
2. `n "50 52"` - A pattern of synth parameters (containing sample or note numbers)
3. `"rash rash"` - A pattern of words
4. `s "rash rash"` - Another pattern of synth parameters (containing sample or synth names)
5. `n "50 52" # sound "rash rash"` - The two patterns of synth parameters combined into the final pattern of synth parameters, which contains both sample/synth numbers and names.

## Structure of composition

In the previous example, both the `sound` and `n` patterns had the same, simple *structure* -- two events that matched up in an obvious way. A nice thing about Tidal though is that it will take care of matching up patterns that have very different structures. Even though Tidal takes care of this, it's good to get a feel for exactly what it's doing.

A general rule of thumb is that when Tidal combines patterns, the *structure comes from the left*. As a simple example, the following only plays one drum sound per cycle:

```
d1 $ sound "drum" # n "0 1 [2 4]"
```

The structure of the pattern on the left is a single event, `drum` which matches up with the first `n` value, which is `0`. So that's all we hear. To get four sounds to play, lets just swap the parameters around:

```
d1 $ n "0 1 [2 4]" # sound "drum"
```

That's better. The structure comes from the `n` pattern, and all of them match up with the single `drum` event, so we get to hear four sounds. At this point an advantage of using the `n` parameter is clear. Using the `:` trick, we could have specified everything inside the `sound` parameter, but that would have involved typing `drum` four times:

```
d1 $ sound "drum:0 drum:1 [drum:2 drum:4]"
```

Not only is the above more typing, but it's less flexible - if we wanted to change the sampleset to something else that would have involved editing the sound name four times as well.

Although the structure comes from the pattern on the left, that doesn't mean that the structure of the pattern on the right doesn't matter. For example we can make the `sound` pattern play from one sampleset for the first half of the cycle, and another for the second half:

```
d1 $ n "0 1 [2 4]" # sound "drum arpy"
```

Or alternate between samplesets from cycle to cycle:

```
d1 $ n "0 1 [2 4]" # sound "<drum arpy>"
```

Or play from two samplesets at once:

```
d1 $ n "0 1 [2 4]" # sound "[drum, arpy]"
```

You might wonder what the mechanics of this are, looking closely at the first example, there are three steps in the pattern on the left (`0`, `1`, and the subpattern `[2 4]`), and two on the right (`drum` and `arpy`).

```
d1 $ n "0 1 [2 4]" # sound "drum arpy"
```

You can think about this in terms of Tidal lining up both cycles, and matching up where the events start in the pattern on the left, with the timespan of the events in the pattern on the right. Working the above example, `0` starts at the beginning of the pattern, and so does `drum`, so it's clear that they match. However `1` starts one third of the way through the pattern, and no event in the pattern on the right starts at the same point in time. But because there are two events, `drum` occupies the first half of the pattern on the right, and because `1/3` is between `0` and `1/2`, the `0` sample number matches with the `drum`. Likewise the `2` sample number begins at position `2/3`, and the `4` sample number begins at position `5/6`, which both lie within the timespan of the second event, which lasts from `1/2` to `1`.

This is difficult to get your head around, especially for more complex patterns, but don't worry too much. Tidal takes care of all this for you, and you'll get a feel for it over time.

## Effects

<span class="index" term="parameters, effect" />
<span class="index" term="effect parameters" />

| name   | description |
|--------|-------------|
| attack | |
| bpf | |
| bpq | |
| coarse | |
| crush | |
| lpf | |
| lpq | |
| gain | |
| hcutoff | |
| hpf | |
| hpq | |
| hold | |
| phaserdepth | |
| phaserrate | |
| release | |
| shape | |
| tremolodepth | |
| tremolorate | |
| vowel | |

## Control

<span class="index" term="parameters, control" />
<span class="index" term="control parameters" />

| name   | description |
|--------|-------------|
| accelerate | |
| begin | |
| buffer | |
| channel | |
| cut | |
| delayAmp | |
| delta | |
| end | |
| endSpeed | |
| fadeInTime | |
| fadeTime | |
| lag | |
| latency | |
| legato | |
| loop | |
| midinote | |
| n | |
| octave | |
| offset | |
| orbit | |
| pan | |
| sound / s | |
| sustain | |
| synthGroup | |
| unit | |

<span class="index" term="parameters, global" />
<span class="index" term="global parameters" />

| name   | description |
|--------|-------------|
| delayAmp | |
| delayfeedback | |
| delaytime | |
| leslie | |
| lrate | |
| lsize | |
| room | |
| size | |

# Patterns of numbers

# Built-in Synths

<span class="index" term="synths" />
The `bd`, `sd` etc sounds are recorded samples, but with SuperDirt it's
also possible to synthesise sounds on the fly. There are a few synths
built-in, lets have a quick listen to one of them.

```
d1 $ n "0 7 [12 5] 4" # sound "supermandolin"
```

Available built-in synths include super808, superchip, superclap,
supercomparator, superfork, supergong, superhammond, superhat,
superhoover, superkick, supermandolin, supernoise, superpiano,
superpwm, supersaw, supersiren, supersnare, supersquare, superstatic,
supervibe and superzow. Try them out!

<span class="index" term="note names" />

In the `n` pattern of the previous example, `0` is by default middle c in the western scale, and the others notes relative to that. If you'd rather deal with note names rather than notes, you can. For example the above can be written like this:

```
d1 $ n "c g [c6 f] e" # sound "supermandolin"
```

The `c6` means `c` in the sixth octave (the default octave being the fifth). You can mix and match these, for example you might want to add together a pattern of note names with a pattern of note transpositions:

```
d1 $ n ("c g [c6 f] e" + "<0 0 7 5>") # sound "supermandolin"
```

For sharps put `s` after the note (and before the octave number, if present), and for flats use `f`:

```
d1 $ n "c gf4 [cs6 fs6] ef7" # sound "superpiano"
```

You can even do sharp-sharps and flat-sharp-sharps, if that makes any sense to you.

d1 $ n "cs6 css6 csssfs7" # sound "superpiano"

Remember that you can use just about all the same effects with synths as you can with samples:

```
d1 $ every 2 (# vowel "a") $
  off 0.5 (# (s "superpiano" # gain 0.8))
    $ n (off 0.25 (+7) $ slow 2 $ "c(3,8) g [c6*2 f] e*2" + "<0 0 7 5>") # sound "supermandolin"
  # room 0.4
  # size 0.9
  # shape 0.3
```

# Patternings

## Repetition

## Symmetry

## Interference

## Deviation

# Syntax

# TidalCycles in Practice

## Improvisation

## Composition

# Interoperation

## SuperDirt

## MIDI

## Custom Open Sound Control

# Personal accounts

# Index

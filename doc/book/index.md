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

##

# Effects and other parameters

We have so far seen two parameters, the `sound` (aka `s`) one, and

## Synths

<span class="index" term="synths" />
The `bd`, `sd` etc sounds are recorded samples, but with SuperDirt it's
also possible to synthesise sounds on the fly. There are a few synths
built-in, lets have a quick listen to one of them.

```
d1 $ n "0 7 12 5" # sound "supermandolin"
```

In the above `0` is middle c, the others notes relative to that. 

note names ...

# Patternings

## Repetition

## Symmetry

## Interference

## Deviation

# Syntax

# MIDI

# TidalCycles in Practice

## Improvisation

## Composition

# Personal accounts

# Index

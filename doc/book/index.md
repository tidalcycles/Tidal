% TidalCycles

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

But where does this `bd` and `sd` sound come from? They are sound
samples, which exist in folders on your computer. Have a look at
them - open supercollider, and from the menu select `File` > `Open
user support directory`. A folder window should pop up, open
`downloaded-quarks` subfolder and then `Dirt-Samples` in that. You
should now see a large folder full of samples.

# Sequences

# Effects

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

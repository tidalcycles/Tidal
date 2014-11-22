---
category: sequences
layout: default
---

Tidal starts with nine connections to the dirt synthesiser, named from
`d1` to `d9`. Here's a minimal example, that plays a bass drum every loop:

~~~~ {haskell}
d1 $ sound "bd"
~~~~

In the above, `sound` tells us we're making a pattern of sounds, and
`"bd"` is a pattern that contains a single sound. `bd` is a sample of
a bass drum. To run the code, use `Ctrl-C` then `Ctrl-C`.

{% capture dollarop %}
In case you're wondering, the `$` character in the above is Haskell syntax, which just means _give the result of the right to the function on the left_. An alternative would have been to do without the `$` by wrapping everything on the right in parenthesis: `d1 (sound "bd")`
{% endcapture %}

{% include alert.html content=dollarop %}

We can pick variations of a sound by adding a colon (`:`) then a
number, for example this picks the fourth bass drum (it counts from
0, so :3 gives you the fourth sound):

~~~~ {haskell}
d1 $ sound "bd:3"
~~~~

Putting things in quotes actually defines a sequence. For example, the
following gives you a pattern of bass drum then snare:

~~~~ {haskell}
d1 $ sound "bd sn"
~~~~

When you do `Ctrl-C Ctrl-C` on the above, you are replacing the
previous pattern with another one on-the-fly. Congratulations, you're
live coding.

The `sound` function in the above is just one possible parameter that
we can send to the synth. Below show a couple more, `pan` and `vowel`:

~~~~ {haskell}
d1 $ sound "bd sn sn"
   |+| vowel "a o e"
   |+| pan "0 0.5 1"
~~~~

{% capture multiline %}
__Note:__ `Ctrl-C Ctrl-C` won't work on the above, because it goes over more than one line. Instead, do `Ctrl-C Ctrl-E` to run the whole block. However, note that there must be blank lines surrounding the block (which also means that patterns cannot contain blank lines).
{% endcapture %}
{% include alert.html content=multiline %}

Note that for `pan`, when working in stereo, that `0` means hard left,
`1` means hard right, and `0.5` means centre.

When specifying a sequence you can group together several events to
play inside a single event by using square brackets:

~~~~ {haskell}
d1 $ sound "[bd sn sn] sn"
~~~~

This is good for creating compound time signatures (sn = snare, cp = clap):

~~~~ {haskell}
d1 $ sound "[bd sn sn] [cp cp]"
~~~~

And you put events inside events to create any level of detail:

~~~~ {haskell}
d1 $ sound "[bd bd] [bd [sn [sn sn] sn] sn]"
~~~~

You can also layer up several loops, by using commas to separate the
different parts:

~~~~ {haskell}
d1 $ sound "[bd bd bd, sn cp sn cp]"
~~~~

This would play the sequence `bd bd bd` at the same time as `sn cp sn
cp`. Note that the first sequence only has three events, and the
second one has four. Because tidal ensures both loops fit inside same
duration, you end up with a polyrhythm.

Try replacing the square brackets with curly brackets:

~~~~ {haskell}
d1 $ sound "{bd ht lt, sn cp}"
~~~~

This is a different way of specifying a polyrhythm. Instead of both
parts taking up the same amount of time, each event within the second
part takes up the same amount of time as one (top-level) event in the
first part. You can embed these different forms inside each other:

~~~~ {haskell}
d1 $ sound "{bd [ht sn, lt mt ht] lt, sn cp}"
~~~~

By default, the number of steps in the first part (in this case, 3) is taken as the number of events per cycle used for the other parts. To specify a different number of steps per cycle, you can use `%`, like this:

~~~~ {haskell}
d1 $ sound "{bd [ht sn, lt mt ht] lt, sn cp}%5"
~~~~

In the above example, five events will be played from each part, in rotation, every cycle.

You can make parts of patterns repeat by using `*`, for example the
following expressions produce the same pattern:

~~~~ {haskell}
d1 $ sound "[bd bd bd, sn cp sn cp]"

d1 $ sound "[bd*3, [sn cp]*2]"
~~~~

Conversely, you can slow down patterns by using `/`, the following
pattern plays part of each subpattern each cycle:

~~~~ {haskell}
d1 $ sound "[bd sn sn*3]/2 [bd sn*3 bd*4]/3"
~~~~

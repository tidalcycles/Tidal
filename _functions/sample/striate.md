---
title: striate
category: sample
---

~~~~ {haskell}
striate :: Int -> OscPattern -> OscPattern
~~~~

Striate is a kind of granulator, for example:

~~~~ {haskell}
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

~~~~ {haskell}
d1 $  slow 8 $ striate 128 $ sound "bev"
~~~~

The `striate'` function is a variant of `striate` with an extra
parameter, which specifies the length of each part. The `striate'`
function still scans across the sample over a single cycle, but if
each bit is longer, it creates a sort of stuttering effect. For
example the following will cut the bev sample into 32 parts, but each
will be 1/16th of a sample long:

~~~~ {haskell}
d1 $ slow 32 $ striate' 32 (1/16) $ sound "bev"
~~~~

Note that `striate` uses the `begin` and `end` parameters
internally. This means that if you're using `striate` (or `striate'`)
you probably shouldn't also specify `begin` or `end`.
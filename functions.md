---
title: Functions
category: functions
layout: default
---

To change patterns, either sequences or continuous ones, you can select from a variety of functions to apply.

Because Tidal patterns are defined as something called an "applicative
functor", it's easy to combine them. For example, if you have two
patterns of numbers, you can combine the patterns by, for example,
multiplying the numbers inside them together, like this:

~~~~ {haskell}
d1 $ (brak (sound "bd sn:2 bd sn"))
   |+| pan ((*) <$> sinewave1 <*> (slow 8 $ "0 0.25 0.75"))
~~~~

In the above, the `sinewave1` and the `(slow 8 $ "0 0.25 0.75")`
pattern are multiplied together. Using the `<$>` and the `<*>` in this way
turns the `*` operator, which normally works with two numbers, into a
function that instead works on two *patterns* of numbers.

Here's another example of this technique:

~~~~ {haskell}
d1 $ sound (pick <$> "kurt mouth can*3 sn" <*> slow 7 "0 1 2 3 4")
~~~~

The `pick` function normally just takes the name of a set of samples
(such as `kurt`), and a number, and returns a sample with that
number. Again, using `<$>` and `<*>` turns `pick` into a function that
operates on patterns, rather than simple values. In practice, this
means you can pattern sample numbers separately from sample
sets. Because the sample numbers have been slowed down in the above,
an interesting texture results.

By the way, "0 1 2 3 4" in the above could be replaced with the
pattern generator `run 5`.

In the following sections contain functions for various applications, some will transform the pattern itself (make slower, faster), change the samples within the pattern (chop them in to tiny bits) and others will combine two patterns into a new one.


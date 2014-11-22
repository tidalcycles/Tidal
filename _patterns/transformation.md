---
title: Transforming patterns
category: docs 
layout: default
---

Tidal comes into its own when you start building things up with
functions which transform the patterns in various ways.

For example, `rev` reverses a pattern:

~~~~ {haskell}
d1 $ rev (sound "[bd bd] [bd [sn [sn sn] sn] sn]")
~~~~

That's not so exciting, but things get more interesting when this is
used in combination with another function. For example `every` takes two
parameters, a number, a function and a pattern to apply the function
to. The number specifies how often the function is applied to the
pattern. For example, the following reverses the pattern every fourth
repetition:

~~~~ {haskell}
d1 $ every 4 (rev) (sound "bd*2 [bd [sn sn*2 sn] sn]")
~~~~

You can also slow down or speed up the playback of a pattern, this
makes it a quarter of the speed:

~~~~ {haskell}
d1 $ slow 4 $ sound "bd*2 [bd [sn sn*2 sn] sn]"
~~~~

And this four times the speed:

~~~~ {haskell}
d1 $ density 4 $ sound "bd*2 [bd [sn sn*2 sn] sn]"
~~~~

Note that `slow 0.25` would do exactly the same as `density 4`.

Again, this can be applied selectively:

~~~~ {haskell}
d1 $ every 4 (density 4) $ sound "bd*2 [bd [sn sn*2 sn] sn]"
~~~~

Note the use of parenthesis around `(density 4)`, this is needed, to
group together the function `density` with its parameter `4`, before
being passed as a parameter to the function `every`. 

Instead of putting transformations up front, separated by the pattern
by the `$` symbol, you can put them inside the pattern, for example:

~~~~ {haskell}
d1 $ sound (every 4 (density 4) "bd*2 [bd [sn sn*2 sn] sn]")
   |+| pan sinewave1
~~~~

In the above example the transformation is applied inside the `sound`
parameter to d1, and therefore has no effect on the `pan`
parameter. Again, parenthesis is required to both group together
`(density 4)` before passing as a parameter to `every`, and also
around `every` and its parameters before passing to its function
`sound`.

~~~~ {haskell}
d1 $ sound (every 4 (density 4) "bd*2 [bd [sn sn*2 sn] sn]")
   |+| pan (slow 16 sinewave1)
~~~~

In the above, the sinewave pan has been slowed down, so that the
transition between speakers happens over 16 loops.
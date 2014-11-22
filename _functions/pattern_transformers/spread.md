---
title: spread
category: pattern_transformers
---

~~~~ {haskell}
spread :: (a -> t -> Pattern b) -> [a] -> t -> Pattern b
~~~~

(The above is difficult to describe, if you don't understand Haskell,
just read the description and examples..)

The `spread` function allows you to take a pattern transformation
which takes a parameter, such as `slow`, and provide several
parameters which are switched between. In other words it 'spreads' a
function across several values.

Taking a simple high hat loop as an example:

~~~~ {haskell}
d1 $ sound "ho ho:2 ho:3 hc"
~~~~

We can slow it down by different amounts, such as by a half:

~~~~ {haskell}
  d1 $ slow 2 $ sound "ho ho:2 ho:3 hc"
~~~~

Or by four thirds (i.e. speeding it up by a third; `4%3` means four over
three):

~~~~ {haskell}
  d1 $ slow (4%3) $ sound "ho ho:2 ho:3 hc"
~~~~

But if we use `spread`, we can make a pattern which alternates between
the two speeds:

~~~~ {haskell}
d1 $ spread slow [2,4%3] $ sound "ho ho:2 ho:3 hc"
~~~~

There's a version of this function, `spread'` (pronounced "spread prime"), which takes a *pattern* of parameters, instead of a list:

~~~~ {haskell}
d1 $ spread' slow "2 4%3" $ sound "ho ho:2 ho:3 hc"
~~~~

This is quite a messy area of Tidal - due to a slight difference of
implementation this sounds completely different! One advantage of
using `spread'` though is that you can provide polyphonic parameters, e.g.:

~~~~ {haskell}
d1 $ spread' slow "[2 4%3, 3]" $ sound "ho ho:2 ho:3 hc"
~~~~
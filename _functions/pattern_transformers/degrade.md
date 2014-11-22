---
title: degrade
category: pattern_transformers
---

~~~~ {haskell}
degrade :: Pattern a -> Pattern a
degradeBy :: Double -> Pattern a -> Pattern a
~~~~

`degrade` randomly removes events from a pattern 50% of the time:

~~~~ {haskell}
d1 $ slow 2 $ degrade $ sound "[[[feel:5*8,feel*3] feel:3*8], feel*4]"
   |+| accelerate "-6"
   |+| speed "2"
~~~~

The shorthand syntax for `degrade` is a question mark: `?`. Using `?`
will allow you to randomly remove events from a portion of a pattern:

~~~~ {haskell}
d1 $ slow 2 $ sound "bd ~ sn bd ~ bd? [sn bd?] ~"
~~~~

You can also use `?` to randomly remove events from entire sub-patterns:

~~~~ {haskell}
d1 $ slow 2 $ sound "[[[feel:5*8,feel*3] feel:3*8]?, feel*4]"
~~~~

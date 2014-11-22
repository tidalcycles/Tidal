---
title: slowspread
category: pattern_transformers
---

~~~~ {haskell}
slowspread :: (a -> t -> Pattern b) -> [a] -> t -> Pattern b
~~~~

`slowspread` takes a list of pattern transforms and applies them one at a time, per cycle, 
then repeats.

Example:

~~~~ {haskell}
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

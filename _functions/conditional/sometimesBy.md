---
title: sometimesBy
category: conditional
---

~~~~ {haskell}
sometimesBy :: Double -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
~~~~

Use `sometimesBy` to apply a given function "sometimes". For example, the 
following code results in `density 2` being applied about 25% of the time:

~~~~ {haskell}
d1 $ sometimesBy 0.25 (density 2) $ sound "bd*8"
~~~~

There are some aliases as well:

~~~~ {haskell}
sometimes = sometimesBy 0.5
often = sometimesBy 0.75
rarely = sometimesBy 0.25
almostNever = sometimesBy 0.1
almostAlways = sometimesBy 0.9
~~~~


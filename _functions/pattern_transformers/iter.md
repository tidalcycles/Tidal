---
title: iter
category: pattern_transformers
---

~~~~ {haskell}
iter :: Int -> Pattern a -> Pattern a
~~~~

Divides a pattern into a given number of subdivisions, plays the subdivisions
in order, but increments the starting subdivision each cycle. The pattern
wraps to the first subdivision after the last subdivision is played.

Example:

~~~~ {haskell}
d1 $ iter 4 $ sound "bd hh sn cp"
~~~~

This will produce the following over four cycles:

~~~~ {haskell}
bd hh sn cp
hh sn cp bd
sn cp bd hh
cp bd hh sn
~~~~

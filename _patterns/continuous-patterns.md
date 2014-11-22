---
category: continuous
layout: default
---


As well as making patterns as sequences, we can also use continuous
patterns. This makes particular sense for parameters such as `pan`
(for panning sounds between speakers) and `shape` (for adding
distortion) which are patterns of numbers.

~~~~ {haskell}
d1 $ sound "[bd bd] [bd [sn [sn sn] sn] sn]"
   |+| pan sinewave1
   |+| shape sinewave1
~~~~

The above uses the pattern `sinewave1` to continuously pan between the
left and right speaker. You could also try out `triwave1` and
`squarewave1`.  The functions `sinewave`, `triwave` and `squarewave`
also exist, but they go between `-1` and `1`, which is often not what
you want.
---
title: jux
category: compositions
---

The `jux` function creates strange stereo effects, by applying a
function to a pattern, but only in the right-hand channel. For
example, the following reverses the pattern on the righthand side:

~~~~ {haskell}
d1 $ slow 32 $ jux (rev) $ striate' 32 (1/16) $ sound "bev"
~~~~

When passing pattern transforms to functions like `jux` and `every`,
it's possible to chain multiple transforms together with `.`, for
example this both reverses and halves the playback speed of the
pattern in the righthand channel:

~~~~ {haskell}
d1 $ slow 32 $ jux ((|+| speed "0.5") . rev) $ striate' 32 (1/16) $ sound "bev"
~~~~
---
title: Mapping over patterns
category: docs
layout: default

---

Sometimes you want to transform all the events inside a pattern, and
not the time structure of the pattern itself. For example, if you
wanted to pass a sinewave to `shape`, but wanted the sinewave to go
from `0` to `0.5` rather than from `0` to `1`, you could do this:

~~~~ {haskell}
d1 $ sound "bd*2 [bd [sn sn*2 sn] sn]"))
   |+| shape ((/ 2) <$> sinewave1)
~~~~

The above applies the function `(/ 2)` (which simply means divide by
two), to all the values inside the `sinewave1` pattern.
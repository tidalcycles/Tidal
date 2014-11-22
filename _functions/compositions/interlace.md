---
title: interlace
category: compositions
---

~~~~ {haskell}
interlace :: OscPattern -> OscPattern -> OscPattern
~~~~

(A function that takes two OscPatterns, and blends them together into
a new OscPattern. An OscPattern is basically a pattern of messages to
a synthesiser.)

Shifts between the two given patterns, using distortion.

Example:

~~~~ {haskell}
d1 $ interlace (sound  "bd sn kurt") (every 3 rev $ sound  "bd sn:2")
~~~~

---
category: pattern_transformers
title: brak
---

~~~~ {haskell}
brak :: Pattern a -> Pattern a
~~~~

(The above means that `brak` is a function from patterns of any type,
to a pattern of the same type.)

Make a pattern sound a bit like a breakbeat

Example:

~~~~ {haskell}
d1 $ sound (brak "bd sn kurt")
~~~~

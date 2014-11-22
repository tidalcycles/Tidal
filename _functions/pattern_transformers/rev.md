---
title: rev
category: pattern_transformers
---

~~~~ {haskell}
rev :: Pattern a -> Pattern a
~~~~

Reverse a pattern

Examples:

~~~~ {haskell}
d1 $ every 3 (rev) $ sound (density 2 "bd sn kurt")
~~~~
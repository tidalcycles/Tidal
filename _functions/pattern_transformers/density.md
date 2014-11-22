---
title: density
category: pattern_transformers
---

~~~~ {haskell}
density :: Time -> Pattern a -> Pattern a
~~~~


Speed up a pattern.

Example:

~~~~ {haskell}
d1 $ sound (density 2 "bd sn kurt")
   |+| density 3 (vowel "a e o")
~~~~

Also, see `slow`.
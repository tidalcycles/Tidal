---
title: slow
category: pattern_transformers
---

~~~~ {haskell}
slow :: Time -> Pattern a -> Pattern a
~~~~

Slow down a pattern.

Example:

~~~~ {haskell}
d1 $ sound (slow 2 "bd sn kurt")
   |+| slow 3 (vowel "a e o")
~~~~

Slow also accepts numbers between 0 and 1, which causes the pattern to speed up:

~~~~ {haskell}
d1 $ sound (slow 0.5 "bd sn kurt")
   |+| slow 0.75 (vowel "a e o")
~~~~

Also, see `density`.
---
title: every
category: conditional
---

~~~~ {haskell}
every :: Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
~~~~

(The above means `every` is a function that is given an integer number, a
function which transforms a pattern, and an actual pattern, and
returns a pattern of the same type.)

Transform the given pattern using the given function, but only every
given number of repetitions.

Example:

~~~~ {haskell}
d1 $ sound (every 3 (density 2) "bd sn kurt")
~~~~

Also, see `whenmod`.
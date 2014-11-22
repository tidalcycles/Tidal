---
title: Beat rotation
category: pattern_transformers

---
~~~~ {haskell}
(<~) :: Time -> Pattern a -> Pattern a
~~~~

or

~~~~ {haskell}
(~>) :: Time -> Pattern a -> Pattern a
~~~~

(The above means that `<~` and `~>` are functions that are given a
time value and a pattern of any type, and returns a pattern of the
same type.)

Rotate a loop either to the left or the right.

Example:

~~~~ {haskell}
d1 $ every 4 (0.25 <~) $ sound (density 2 "bd sn kurt")
~~~~
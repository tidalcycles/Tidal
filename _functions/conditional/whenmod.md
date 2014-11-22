---
title: whenmod
category: conditional
---

~~~~ {haskell}
whenmod :: Int -> Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
~~~~

`whenmod` has a similar form and behavior to `every`, but requires an 
additional number. Applies the function to the pattern, when the
remainder of the current loop number divided by the first parameter,
is less than the second parameter.

For example the following makes every other block of four loops twice
as dense:

~~~~ {haskell}
d1 $ whenmod 8 4 (density 2) (sound "bd sn kurt")
~~~~
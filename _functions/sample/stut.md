---
title: stut
category: sample
---


~~~~ {haskell}
stut :: Integer -> Double -> Rational -> OscPattern -> OscPattern
~~~~

Stut applies a type of delay to a pattern. It has three parameters, 
which could be called depth, feedback and time. Depth is an integer
and the others floating point. This adds a bit of echo:

~~~~ {haskell}
d1 $ stut 4 0.5 0.2 $ sound "bd sn"
~~~~

The above results in 4 echos, each one 50% quieter than the last, 
with 1/5th of a cycle between them. It is possible to reverse the echo:

~~~~ {haskell}
d1 $ stut 4 0.5 (-0.2) $ sound "bd sn"
~~~~
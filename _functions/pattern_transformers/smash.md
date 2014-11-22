---
title: smash
category: pattern_transformers
---


~~~~ {haskell}
smash :: Int -> [Time] -> OscPattern -> OscPattern
~~~~

Smash is a combination of `spread` and `striate` - it cuts the samples
into the given number of bits, and then cuts between playing the loop
at different speeds according to the values in the list.

So this:

~~~~ {haskell}
  d1 $ smash 3 [2,3,4] $ sound "ho ho:2 ho:3 hc"
~~~~

Is a bit like this:

~~~~ {haskell}
  d1 $ spread (slow) [2,3,4] $ striate 3 $ sound "ho ho:2 ho:3 hc"
~~~~

This is quite dancehall:

~~~~ {haskell}
d1 $ (spread' slow "1%4 2 1 3" $ spread (striate) [2,3,4,1] $ sound
"sn:2 sid:3 cp sid:4")
  |+| speed "[1 2 1 1]/2"
~~~~
---
title: stack
category: compositions
---

~~~~ {haskell}
stack :: [Pattern a] -> Pattern a
~~~~

`stack` takes a list of patterns and combines them into a new pattern by
playing all of the patterns in the list simultaneously.

~~~~ {haskell}
d1 $ stack [ 
  sound "bd bd*2", 
  sound "hh*2 [sn cp] cp future*4", 
  sound (samples "arpy*8", (run 16))
]
~~~~

This is useful if you want to use a transform or synth parameter on the entire 
stack:

~~~~ {haskell}
d1 $ whenmod 5 3 (striate 3) $ stack [ 
  sound "bd bd*2", 
  sound "hh*2 [sn cp] cp future*4", 
  sound (samples "arpy*8", (run 16))
] |+| speed "[[1 0.8], [1.5 2]*2]/3"
~~~~
---
title: seqP
category: compositions
---

There is a similar function named `seqP` which allows you to define when
a sound within a list starts and ends. The code below contains three
separate patterns in a "stack", but each has different start times 
(zero cycles, eight cycles, and sixteen cycles, respectively). All
patterns stop after 128 cycles:

~~~~ {haskell}
d1 $ seqP [ 
  (0, 128, sound "bd bd*2"), 
  (8, 128, sound "hh*2 [sn cp] cp future*4"), 
  (16, 128, sound (samples "arpy*8", (run 16)))
]
~~~~
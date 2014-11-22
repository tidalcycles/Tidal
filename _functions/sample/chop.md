---
title: chop
category: sample
---

~~~~ {haskell}
chop :: Int -> OscPattern -> OscPattern
~~~~

`chop` granualizes every sample in place as it is played. Use an integer
value to specify how manu granules each sample is chopped into:

~~~~ {haskell}
d1 $ chop 16 $ sound "arpy arp feel*4 arpy*4"
~~~~

Different values of `chop` can yield very different results, depending
on the samples used:

~~~~ {haskell}
d1 $ chop 16 $ sound (samples "arpy*8" (run 16))
d1 $ chop 32 $ sound (samples "arpy*8" (run 16))
d1 $ chop 256 $ sound "bd*4 [sn cp] [hh future]*2 [cp feel]"
~~~~
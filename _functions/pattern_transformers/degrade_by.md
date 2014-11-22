---
title: degrade_by
category: pattern_transformers
---

`degradeBy` allows you to control the percentage of events that
are removed. For example, to remove events 90% of the time:

~~~~ {haskell}
d1 $ slow 2 $ degradeBy 0.9 $ sound "[[[feel:5*8,feel*3] feel:3*8], feel*4]"
   |+| accelerate "-6"
   |+| speed "2"
~~~~
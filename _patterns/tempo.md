---
title: Tempo
category: docs
layout: default
---


You can change the cycles per second (cps) like this:

~~~~ {haskell}
cps 0.5
~~~~

The above would give a rate of one cycle every two seconds. If you prefer to think in cycles per minute, simply divide by 60:

~~~~ {haskell}
cps (30 / 60)
~~~~

If you wish to think in terms of 'beats' rather than cycles, then decide how many beats per cycle you expect to work with, and divide accordingly.

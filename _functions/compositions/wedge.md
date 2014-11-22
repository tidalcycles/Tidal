---
title: wedge
category: compositions
---

~~~~{haskell}
wedge :: Time -> Pattern a -> Pattern a -> Pattern a
~~~~

`wedge` combines two patterns by squashing two patterns into a single pattern cycle.
It takes a ratio as the first argument. The ratio determines what percentage of the
pattern cycle is taken up by the first pattern. The second pattern fills in the
remainder of the pattern cycle.

~~~~{haskell}
d1 $ wedge (1/4) (sound "bd*2 arpy*3 cp sn*2") (sound "odx [feel future]*2 hh hh")
~~~~
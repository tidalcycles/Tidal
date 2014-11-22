---
title: within
category: conditional
---

~~~~{haskell}
within :: Arc -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
~~~~

Use `within` to apply a function to only a part of a pattern. For example, to
apply `density 2` to only the first half of a pattern:

~~~~{haskell}
d1 $ within (0, 0.5) (density 2) $ sound "bd*2 sn lt mt hh hh hh hh"
~~~~

Or, to apply `(|+| speed "0.5") to only the last quarter of a pattern:

~~~~{haskell}
d1 $ within (0.75, 1) (|+| speed "0.5") $ sound "bd*2 sn lt mt hh hh hh hh"
~~~~
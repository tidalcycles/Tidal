---
title: Silence
category: docs
layout: default
---

An empty pattern is defined as `silence`, so if you want to 'switch
off' a pattern, you can just set it to that:

~~~~ {haskell}
d1 silence
~~~~

If you want to set all the connections (from `d1` to `d9`) to silence
at once, there's a single-word shortcut for that:

~~~~ {haskell}
hush
~~~~

You can also isolate a single connection and silence all others with
the `solo` function:

~~~~ {haskell}
solo $ d1 $ sound "bd sn"
~~~~
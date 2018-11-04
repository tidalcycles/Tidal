{-|
Module: Compositions
Description: compose multiple pattern into more complex patterns

Some functions work with multiple sets of patterns, interlace them or play them successively.
-}
module Sound.Tidal.Compositions (append,
                                 append',
                                 cat,
                                 randcat,
                                 seqP,
                                 slowcat,
                                 stack,
                                 superimpose,
                                 wedge,
                                 interlace,
                                 spin,
                                 weave) where

import Sound.Tidal.Pattern
import Sound.Tidal.Strategies

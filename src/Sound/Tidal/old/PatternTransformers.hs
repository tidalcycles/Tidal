{- |
Module: PatternTransformers
Description: Transform patterns

Pattern transformers are functions that take a pattern as input and transform it into a new pattern.

In the following, functions are shown with their Haskell type and a short description of how they work.
-}
module Sound.Tidal.PatternTransformers ((<~), (~>),
                                        brak,
                                        choose,
                                        degrade,
                                        degradeBy,
                                        density,
                                        fit,
                                        fit',
                                        iter,
                                        palindrome,
                                        rev,
                                        slow,
                                        slowspread,
                                        spread,
                                        trunc,
                                        zoom,
                                        jux,
                                        jux',
                                        juxBy,
                                        jux4,
                                        smash) where

import Sound.Tidal.Strategies
import Sound.Tidal.Pattern

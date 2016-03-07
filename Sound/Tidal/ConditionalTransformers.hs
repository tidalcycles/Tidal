{-|
Module: ConditionalTransformers
Description: conditionally apply other transformations to pattern

Conditional transformers are functions that apply other transformations under certain cirumstances. These can be based upon the number of cycles, probability or time-range within a pattern.-}
module Sound.Tidal.ConditionalTransformers (every,
                                            foldEvery,
                                            sometimesBy,
                                            whenmod,
                                            within) where
import Sound.Tidal.Pattern

{-|
Module: SampleTransformers
Description: transform individual samples

The following functions manipulate each sample within a pattern, some granularize them, others echo.
-}
module Sound.Tidal.SampleTransformers (chop,
                                       gap,
                                       striate,
                                       striate',
                                       striateL,
                                       striateL',
                                       striateO,
                                       stut,
                                       stut') where
import Sound.Tidal.Strategies
import Sound.Tidal.Dirt

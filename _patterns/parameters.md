---
title: Synth Parameters
category: continuous
layout: default
---

Synth parameters generate or affect sample playback. These are the 
synthesis parameters you can use:

* `accelerate` - a pattern of numbers that speed up (or slow down) samples while they play.
* `bandf` - a pattern of numbers from 0 to 1. Sets the center frequency of the band-pass filter.
* `bandq` - a pattern of numbers from 0 to 1. Sets the q-factor of the band-pass filter.
* `begin` - a pattern of numbers from 0 to 1. Skips the beginning of each sample, e.g. `0.25` to cut off the first quarter from each sample.
* `crush` - bit crushing, a pattern of numbers from 1 for drastic reduction in bit-depth to 16 for barely no reduction.
* `coarse` - fake-resampling, a pattern of numbers for lowering the sample rate, i.e. 1 for original 2 for half, 3 for a third and so on.
* `cutoff` - a pattern of numbers from 0 to 1. Applies the cutoff frequency of the low-pass filter. 
* `delay` - a pattern of numbers from 0 to 1. Sets the level of the delay signal.
* `delayfeedback` - a pattern of numbers from 0 to 1. Sets the amount of delay feedback.
* `delaytime` - a pattern of numbers from 0 to 1. Sets the length of the delay.
* `end` - the same as `begin`, but cuts the end off samples, shortening them;
  e.g. `0.75` to cut off the last quarter of each sample.
* `gain` - a pattern of numbers that specify volume. Values less than 1 make the sound quieter. Values greater than 1 make the sound louder.
* `hcutoff` - a pattern of numbers from 0 to 1. Applies the cutoff frequency of the high-pass filter. 
* `hresonance` - a pattern of numbers from 0 to 1. Applies the resonance of the high-pass filter.
* `pan` - a pattern of numbers between 0 and 1, from left to right (assuming stereo)
* `resonance` - a pattern of numbers from 0 to 1. Applies the resonance of the low-pass filter.
* `shape` - wave shaping distortion, a pattern of numbers from 0 for no distortion up to 1 for loads of distortion
* `sound` - a pattern of strings representing sound sample names (required)
* `speed` - a pattern of numbers from 0 to 1, which changes the speed of sample playback, i.e. a cheap way of changing pitch
* `vowel` - formant filter to make things sound like vowels, a pattern of either `a`, `e`, `i`, `o` or `u`. Use a rest (`~`) for no effect.
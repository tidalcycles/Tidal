# TidalCycles log of changes

## 1.5.2 - Rivelin
        * Fix streamAll

## 1.5.1 - Blacka Moor
	* Bugfix splice

## 1.5.0 - Active travel
	* Export drawLineSz @mxmxyz
	* tidal-parse additions (bite, splice, pickF, select, squeeze; fixed slice) @dktr0
	* New, more efficient pseudorandom number generator @dktr0
	* Pattern first someCyclesBy param @bgold-cosmos
	* Refactored, more flexible OSC targetting @yaxu
	* Simplify tidal-boot-script in tidal.el @jamagin
	* Support state substitution in mininotation #530 @yaxu
	* Pattern first parameter of splice #656 @yaxu
	* Pattern first parameter of chew @yaxu
	* add 'x' fraction alias for 1/6 @yaxu
	* add dfb alias for delayfeedback param, dt for delaytime @yaxu
	* add unmuteAll and only to BootTidal.hs @yaxu

## 1.4.9 - Housebound spirit
	* Simplify 'show'ing of patterns @yaxu
	* New `draw` function for drawing a pattern of single characters as a text-based diagram,
	  with friends `drawLine` and `drawLineSz` for drawing multiple cycles @yaxu
	* Fixes and expansions of ratio aliases - s should be a sixteenth @mxmxyz, w is now 1, f is now 0.2
	* Simplify definition of `accumulate` using scanl @benjwadams
	* The first parameter of `someCyclesBy` is now patternable @bgold-cosmos

## 1.4.8 - Limerick
        * Add ratio shorthand to floating point patterns @yaxu
        * Support fractional scales, add Arabic scales @quakehead
        * Additions to tidal-parse including support from overgain, overshape adn rot @dktr0
        * Move prompt-cont setting to end of BootTidal.hs (older versions of Haskell crash out at this point) @ndr-brt
## 1.4.7 - Bleep

	* Fix BootTidal.hs - make loadable in atom @bgold-cosmos
	* More additions to tidal-parse @dktr0

## 1.4.6 - Megatron

	* Experimental ratio shorthand ref #573 @yaxu
	* Store mininotation source location(s) in events ref #245 @yaxu
	* Add more things to tidal-parse @dktr0 @yaxu
	* Separate out haskell parser from tidal-parse into new hackage module called 'haskellish' @dktr0
	* Support patterning polyrhythmic % steps in mininotation @yaxu
	* Fixes to emacs plugin (tidal.el) @xmacex
	* New parameters for freq, overgain, overshape, and missing midi params including the new nrpn ones @yaxu

## 1.4.5 - Porter Brook

* Mini notation - `@` (and its alias `_`) now accepts rational relative durations. E.g. `a b@0.5 c d` to make `b` have a half step (that would be the same as `a@2 b c@2 c@d`). This can also be patterned `a b@<0.5 2> c d` @yaxu #435
* Experimental `reset` function - stick in a pattern so it acts as though the cycle number was reset to 0, from the next cycle @yaxu
* Bugfix for setR in BootTidal.hs @yaxu
* Mini notation - `!`, `@` and `_` now work properly within `{}` and `<>`, e.g. `<a b ! c!3 d>` will repeat every 7 cycles @yaxu #369 #248
* Mini notation - `@` and `_` are now aliases for each other, e.g. `a_3` is the same as `a@3` as are `_` and `@` @yaxu #369
* Frame skipping on clock jumps now configurable @yaxu #567
* Sync between tidal instances now works straight away, without having to setcps @yaxu #569
* New `while` function for applying a function selectively according to a binary pattern @yaxu
* Lowercases aliases `slowappend` and `fastappend` for `slowAppend` and `fastAppend` respectively @yaxu
* Many tidal-parse updates @dktr0

## 1.4.4 - Chee Dale

* wrandcat (weighted randcat) @yaxu
* MIDI Sysex support #558 @yaxu
* Elements in an Open Sound Control path address can now be patterned #557
* 'once' now chooses a random cycle to play. To get the old behaviour of playing the first cycle, use 'first' @yaxu #476
* Make random choices in mini-notation behave independently @yaxu #560
* Add [a|b|c] syntax to mini notation for randomly choosing between subsequences @yaxu #555
* Add power pattern operators |**, **| and |**| @yaxu

## 1.4.3 - Stanage Edge

* Fix for xfade / xfadein transition
* New function plyWith

## 1.4.2 - Higger Tor

* Fix for 'nudge'

## 1.4.1 - Carl Wark

* improvements to handling of cps changes @yaxu #501
* fix for parameter patterning in 'range' @yaxu #547

## 1.4.0 - Padley Gorge

* fix representation to handle continuous and analog events properly @yaxu

## 1.3.0 - rolled back to 1.1.2

## 1.2.0 - Hunters Bar

* Simplify <* and *>, removing any distinction between analogue and digital patterns

## 1.1.2 - Eccy Road

* Usability fix for `binary` / `binaryN` (use squeezeJoin on input pattern)

## 1.1.1 - Chelsea Park

* Usability fixes for `binary` / `binaryN` / `ascii` @yaxu

## 1.1.0 - Brincliffe Edge

* `binary` and `ascii` functions for playing with bit patterns @yaxu
* support chord inversions in chord parser @bgold-cosmos
* skip ticks when system clock jumps @yaxu
* fix crash bugs in mini notation parser and grp @yaxu
* new stitch function @yaxu
* |++, ++| and |++| for combining patterns of strings by concatenation @yaxu
* send best effort of a sound id to dirt / superdirt if sendParts is on, allowing parameter adjustment of previously triggered sound (without chopping) @yaxu
* qtrigger - quantise trigger to nearest cycle @yaxu
* add setI, setF et al to BootTidal.hs for setting state variables @yaxu
* BootTidal.hs now sends d1 .. d12 to orbits 0 .. 11 respectively @yaxu
* markov chain support with runMarkov and markovPat @bgold-cosmos
* simplify / fix mask and sew @yaxu
* Adjust <* and *> (and therefore |+, +| etc) to be closer to <*>, explanation here: https://penelope.hypotheses.org/1722 @yaxu
* extract minitidal into its own package tidal-parse (using cabal multipackages), renaming to Sound.Tidal.Parse @yaxu @dktr0
* benchmarking @nini-faroux
* minitidal refactor, support for parsing more of tidal, tests @dktr0

## 1.0.14 - IICON

* 'chew' - like bite, but speeds playback rate up and down
* variable probability for ? in mini notation
* chooseBy takes modulo of index to avoid out of bounds errors
* 'rate' control param
* Fix dependencies for parsec/colour

## 1.0.13 - 🐝⌛️🦋 #2

* Simplify espgrid support - @yaxu

## 1.0.12 - 🐝⌛️🦋

* Fix ESPGrid support - @dktr0
* Add 'snowball' function - @XiNNiW

## 1.0.11 - Cros Bríde

2019-04-17  Alex McLean  <alex@slab.org>
	* Add `bite` function for slicing patterns (rather than samples)
	* Tweak tidal.el to attempt to infer location of default BootTidal.hs
	* Skip time (forward or backward) if the reference clock jumps suddenly
	* Fix `fit` - @bgold-cosmos
	* Remove 'asap'
	* Add cB for boolean control input
	* `pickF` for choosing between functions with a pattern of integers
	* `select` for choosing between list of patterns with a floating point pattern
	* `squeeze` for choosing between list of patterns with a pattern of integers, where patterns are squeezed into the integer event duration
	* `splice` for choosing between slices of a pattern, where the slices are squeezed into event duration
	* Ord and Eq instances for value type @bgold-cosmos
	* `trigger` - support for resetting envelopes on evaluation
	* Support for rational event values
	* Tweak how `*>` and `<*` deal with analog patterns
	* Caribiner link bridge support

## 1.0.10 - This machine also kills fascists
* Add exports to Sound.Tidal.Scales for `getScale` and `scaleTable`

## 1.0.9 - This machine kills fascists
* sec and msec functions for converting from seconds to cycles (for stut etc) @yaxu
* template haskell upper bounds @yaxu
* fix for multi-laptop sync/tempo sharing @yaxu
* fix toScale so it doesn't break on empty lists @bgold-cosmos
* `deconstruct` function for displaying patterns stepwise @yaxu
* `djf` control ready for new superdirt dj filter @yaxu
* `getScale` for handrolling/adding scales to `scale` function	* Add `djf` control for upcoming superdirt dj filter @yaxu

## 1.0.8 (trying to get back to doing these, 
## see also https://tidalcycles.org/index.php/Changes_in_Tidal_1.0.x 
## for earlier stuff)

* Add 'to', 'toArg' and 'from' controls for new superdirt routing experiments - @telephon
* Fixes for squeezeJoin (nee unwrap') - @bgold-cosmos
* Simplify `cycleChoose`, it is now properly discrete (one event per cycle) - @yaxu
* The return of `<>`, `infix alias for overlay` - @yaxu
* Fix for `wedge` to allow 0 and 1 as first parameter  - @XiNNiW
* Support for new spectral fx - @madskjeldgaard
* Fix for _euclidInv - @dktr0
* `chordList` for listing chords - @XiNNiW
* new function `soak` - @XiNNiW
* tempo fixes - @bgold-cosmos
* miniTidal developments - @dktr0
* potentially more efficient euclidean patternings - @dktr0
* unit tests for euclid - @yaxu
* fix for `sometimesBy` - @yaxu

## 0.9.10 (and earlier missing versions from this log)

* arpg, a function to arpeggiate
* within', an alternate within with a different approach to time, following discussion here https://github.com/tidalcycles/Tidal/issues/313
* sine et al are now generalised so can be used as double or rational patterns
* New Sound.Tidal.Simple module with a range of simple transformations (faster, slower, higher, lower, mute, etc)
* slice upgraded to take a pattern of slice indexes
* espgrid support
* lindenmayerI
* sew function, for binary switching between two patterns
* somecycles alias for someCycles
* ply function, for repeating each event in a pattern a given number
  of times within their original timespan
* patternify juxBy, e, e', einv, efull, eoff

## 0.9.7

### Enhancements

* The `note` pattern parameter is no longer an alias for `midinote`,
  but an independent parameter for supercollider to handle (in a manner
  similar to `up`)
  
## 0.9.6

### Enhancements

* Added `chord` for chord patterns and `scaleP` for scale patterns
* The `n` pattern parameter is now floating point

## 0.9.5

### Enhancements

* Added `hurry` which both speeds up the sound and the pattern by the given amount.
* Added `stripe` which repeats a pattern a given number of times per
  cycle, with random but contiguous durations.
* Added continuous function `cosine`
* Turned more pattern transformation parameters into patterns - spread', striateX, every', inside, outside, swing
* Added experimental datatype for Xenakis sieves
* Correctly parse negative rationals
* Added `breakUp` that finds events that share the same timespan, and spreads them out during that timespan, so for example (breakUp "[bd,sn]") gets turned into the "bd sn"
* Added `fill` which 'fills in' gaps in one pattern with events from another. 

## 0.9.4

### Fixes

* Swapped `-` for `..` in ranges as quick fix for issue with parsing negative numbers
* Removed overloaded list thingie for now, unsure whether it's worth the dependency

## 0.9.3

### Enhancements

* The sequence parser can now expand ranges, e.g. `"0-3 4-2"` is
  equivalent to `"[0 1 2 3] [4 3 2]"`
* Sequences can now be described using list syntax, for example `sound ["bd", "sn"]` is equivalent to `sound "bd sn"`. They *aren't* lists though, so you can't for example do `sound (["bd", "sn"] ++ ["arpy", "cp"])` -- but can do `sound (append ["bd", "sn"]  ["arpy", "cp"])`
* New function `linger`, e.g. `linger (1/4)` will only play the first quarter of the given pattern, four times to fill the cycle. 
* `discretise` now takes time value as its first parameter, not a pattern of time, which was causing problems and needs some careful thought.
* a `rel` alias for the `release` parameter, to match the `att` alias for `attack`
* `_fast` alias for `_density`
* The start of automatic testing for a holy bug-free future

### Fixes

* Fixed bug that was causing events to double up or get lost,
  e.g. where `rev` was combined with certain other functions.

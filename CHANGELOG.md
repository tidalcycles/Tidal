# TidalCycles log of changes

## 1.9.0

## What's Changed

### General enhancements
* Rename linux binary name by @ndr-brt in https://github.com/tidalcycles/Tidal/pull/911
* Fix echo by @ndr-brt in https://github.com/tidalcycles/Tidal/pull/910
* If first argument of euclid is negative, apply euclidInv by @polymorphicengine in https://github.com/tidalcycles/Tidal/pull/916
* Add squeeze operators (`||+`, `*||`, etc) by @yaxu in https://github.com/tidalcycles/Tidal/pull/919
* Make chunk reverse direction with negative number by @ndr-brt in https://github.com/tidalcycles/Tidal/pull/918
* overhaul of the chord parser by @polymorphicengine in https://github.com/tidalcycles/Tidal/pull/931

### New ableton link support
* Use ableton link for scheduling by @Zalastax in https://github.com/tidalcycles/Tidal/pull/898
* tidal-link: add link source files to extra-source-files, fixes #924 by @yaxu in https://github.com/tidalcycles/Tidal/pull/925
* Use target latency in all send modes by @Zalastax in https://github.com/tidalcycles/Tidal/pull/927
* Rename cCyclesPerBeat -> cBeatsPerCycle by @Zalastax in https://github.com/tidalcycles/Tidal/pull/939

### Emacs plugin updates
* formatting fixes in tidal.el by @zzkt in https://github.com/tidalcycles/Tidal/pull/932
* a window-excursion for tidal.el by @zzkt in https://github.com/tidalcycles/Tidal/pull/933
* autoloads for tidal.el by @zzkt in https://github.com/tidalcycles/Tidal/pull/934
* avoid race in loading bootscript during restart by @zzkt in https://github.com/tidalcycles/Tidal/pull/937

### Tidal-parse (estuary) updates
* Various updates to tidal-parse by @dktr0 in https://github.com/tidalcycles/Tidal/pull/913 and  https://github.com/tidalcycles/Tidal/pull/941

### Maintenance
* Increase upper bounds of text dependency by @yaxu in https://github.com/tidalcycles/Tidal/pull/948
* Use system-cxx-std-lib by @Zalastax in https://github.com/tidalcycles/Tidal/pull/944
* Use c++ extra library for MacOS by @giuseppelillo in https://github.com/tidalcycles/Tidal/pull/946

## New Contributors
* @Zalastax made their first contribution in https://github.com/tidalcycles/Tidal/pull/898
* @zzkt made their first contribution in https://github.com/tidalcycles/Tidal/pull/932
* @giuseppelillo made their first contribution in https://github.com/tidalcycles/Tidal/pull/946

**Full Changelog**: https://github.com/tidalcycles/Tidal/compare/v1.8.0...v1.9.0

## 1.8.1

* Removed executable from tidal.cabal file (while latest ghc on windows fails to build it)

## 1.8.0 - Sanquhar

* move tidal-listener code by @polymorphicengine in #885
* Fix a typo incurred (I assume) by a filename change. by @JeffreyBenjaminBrown in #886
* Hide contexts from Events per default by @polymorphicengine in #887
* Allow sending/receiving of broadcasted OSC control messages by @yaxu in #894
* tidal-listener: Add minimal install notes by @gamar3is in #895
* Add rolled function with variants by @thgrund in #820
* Valuable instance for Note by @yaxu in #899
* Add parsers for ints and floats that don't consume trailing whitespace by @polymorphicengine in #900
* Introduce echo and echoWith, deprecate stut and stutWith by @ndr-brt in #904
* tidal-listener: Optional WITH_GHC environment variable by @mindofmatthew in #903

Commit list: https://github.com/tidalcycles/Tidal/compare/1.7.10...v1.8.0

## 1.7.10 - Tama b

* Derive RealFrac for Note by @ndr-brt in https://github.com/tidalcycles/Tidal/pull/876
* Add timescale and timewin control params by @yaxu in https://github.com/tidalcycles/Tidal/pull/878
* deriving typeclass memberships for ID by @polymorphicengine in https://github.com/tidalcycles/Tidal/pull/879
* Adds the jumpMod' transition by @th-four in https://github.com/tidalcycles/Tidal/pull/881
* Import safe-tidal-cli ghci simulator as tidal binary by @yaxu in https://github.com/tidalcycles/Tidal/pull/880
* Tweaks to tidal binary, and add to linux workflow by @yaxu in https://github.com/tidalcycles/Tidal/pull/882
* add tidal ghci replacement to macos and windows workflows by @yaxu in https://github.com/tidalcycles/Tidal/pull/883

## 1.7.9 - Tama

### What's Changed
* Show lists of values by @polymorphicengine in https://github.com/tidalcycles/Tidal/pull/838
* Port listener wiki content as README by @ndr-brt in https://github.com/tidalcycles/Tidal/pull/851
* Chords - consistent naming cont'd by @cleary in https://github.com/tidalcycles/Tidal/pull/840
* Formalise pattern IDs by @mindofmatthew in https://github.com/tidalcycles/Tidal/pull/807
* Switch to non-blocking pMap updates for transitions by @bgold-cosmos in https://github.com/tidalcycles/Tidal/pull/858
* Add listener build workflows by @ndr-brt in https://github.com/tidalcycles/Tidal/pull/852
* Fix tidal-listener install process by @ndr-brt in https://github.com/tidalcycles/Tidal/pull/861
* Fix mod by @bgold-cosmos in https://github.com/tidalcycles/Tidal/pull/860
* Fix stack ci using last lts resolver by @ndr-brt in https://github.com/tidalcycles/Tidal/pull/862
* Make Pattern instance Monoid by @fbous in https://github.com/tidalcycles/Tidal/pull/865
* Added OSC playback control for silencing individual patterns by @njanssen in https://github.com/tidalcycles/Tidal/pull/863
* Use better show instances for notes & rationals by @mindofmatthew in https://github.com/tidalcycles/Tidal/pull/857
* tidal-listener: Separate build from release by @ndr-brt in https://github.com/tidalcycles/Tidal/pull/866
* Exponential double by @ndr-brt in https://github.com/tidalcycles/Tidal/pull/871
* Comments by @JeffreyBenjaminBrown in https://github.com/tidalcycles/Tidal/pull/877
* Fix for classic dirt support by @yaxu https://github.com/tidalcycles/Tidal/commit/15b5b8b91af08ebad39efe2a7e0712b21f606ca4
* New alias `number` for `n` by @yaxu https://github.com/tidalcycles/Tidal/commit/ac0be63d686ab37f7b2dcd440d4bd8f3898453e8
* Allow "0..8" to be parsed as a range of doubles in mininotation by @yaxu https://github.com/tidalcycles/Tidal/commit/55f8ad9b0091b43fdd364eced25bc9c655d157cc
* Bugfix for `timeLoop` by @yaxu in https://github.com/tidalcycles/Tidal/commit/29f28ed637a7c17ad2b22558d097a694da604e2d
* Retire pre ghc 8.4 support by @yaxu 
* Pattern first parameter of `wedge` by @yaxu
* Allow `|**|` et al to work on control patterns by @yaxu in https://github.com/tidalcycles/Tidal/commit/7142775c2039cae4cde9bdd6f68b3e0cbae2de9d
* Add alias `timecat` for timeCat` by @yaxu in https://github.com/tidalcycles/Tidal/commit/363889bdc963d9357daf1893d18ab9dfc33ca5ac

### New Contributors
* @fbous made their first contribution in https://github.com/tidalcycles/Tidal/pull/865
* @njanssen made their first contribution in https://github.com/tidalcycles/Tidal/pull/863
* @JeffreyBenjaminBrown made their first contribution in https://github.com/tidalcycles/Tidal/pull/877

**Full Changelog**: https://github.com/tidalcycles/Tidal/compare/1.7.8...1.7.9

## 1.7.8 - Ayatakedai
   * Add pattern id to patterns as _id_, and send it with /code/highlights

## 1.7.7 - Caramel wafer
   * Support hosc 0.19 in test suite @yaxu

## 1.7.6 - Tunnocks
   * Fix dot shorthand in int patterns so floating point numbers raise an error @ndr-brt
   * Support factors in ratio shorthand for rational patterns @ndr-brt
   * support hosc 0.19 @yaxu

## 1.7.5 - Dalbeattie
   * Minor change to _splice so that it respects if the speed parameter was already declared @onthepeakofnormal
   * Some tests for chords @cleary
   * Remove direct semigroups dependency @yaxu
   * Derive functor for pattern @yaxu
   * Handle negative ratio shorthands for rationals @ndr_brt
   * drawLine - draw non-events with periods @yaxu
   * Parse chord without root note @yaxu
   * Fix clock sharing between tidal processes @yaxu

## 1.7.4 - Symonds Yat b
   * Fixes for bipolar waveforms (sine2, etc) @mindofmatthew
   * More playback controls for OSC API @mindofmatthew
   * Disable bus variants for MIDI controls @mindofmatthew

## 1.7.3 - Symonds Yat
   * Signed ratio shorthands now supported @ndr_brt
   * OSC API - mute/unmute stream @mindofmatthew
   * improve performance of playFor/seqP @yaxu
   * Expand tests with autodiscovered laws @RSWilli
   * Add `getState` to BootTidal @mindofmatthew
   * add fadeTime / fadeInTime / fadeOutTime params for grain envelopes @yaxu
   * Fix for nrpnn and nrpnv params @yaxu
   * Disable busses for MIDI parameters @mindofmatthew
   
## 1.7.2 - IKLECTIK c
   * tidal-parse additions and improved errors, haskellish dependency fix @dktr0
   * spring cleaning and delinting, recreated Time.hs, renamed ControlMap toValueMAp @yaxu
   * start of [major refactoring of chords](https://club.tidalcycles.org/t/rfc-working-on-making-chord-naming-chordlist-more-consistent/2717/56) @cleary
   * fix `.` mininotation operator @zudov
   * stateful event parameters, for counting @yaxu
   * Move some stuff from BootTidal.hs to the tidal library, to reduce dependencies there @yaxu

## 1.7.1 - IKLECTIK b
        * Fix stack build @yaxu
        * Add tidal_status command @yaxu
	* Add cVerbose config setting (default on) @yaxu
	* Fix for emacs plugin @jwaldmann
	* Snowball bugfix @yaxu
	* Migrate to github actions, tested against additional ghc versions @ndr_brt
	* Fix mininotation bug - allow leading/trailing spaces @ndr_brt
	* Make linger work with negative numbers (to linger on end of cycle rather than start) @yaxu
	* Friendlier startup messages @yaxu
	* Compatibility with ghc 9.0.1 @yaxu

## 1.7 - IKLECTIK
	* Added drum aliases from GM2 percussion spec to `drum` function @lvm
	* `getcps` helper function now in BootTidal.hs @yaxu
	* `getnow` helper function back in BootTidal.hs (returns current cycle) @bgold-cosmos
	* Developments towards tidal api, @yaxu et al
	* `coarse` parameter is now floating point @lwlsn
	* `irand` parameter now patternable @ndr-brt
 	* `note` now produces its own type to avoid conflicts between parsing note names and duration shorthands  @ndr-brt
	* Numerous tidal-parse improvements and additions @dktr0 et al
	* `grain` function for combining begin and end (in terms of begin and duration) @khoparzi
	* Added missing pB and pR functions @thgrund
	* Emacs plugin bugfixes @jwaldmann
	* `binaryN` parameters now fully patternable @ndr-brt
	* `press` and `pressBy` functions for syncopation @yaxu
	* `bite`'s first parameter is now patternable @ndr-brt
	* Most SuperDirt control/effect parameters can now be modified while a sound is playing, using 'bus' functions. @telefon + @yaxu
	* Sound.Tidal.Params is now generated by a script in bin/generate-params.hs @yaxu
	* `qtrigger` is now an alias for `ctrigger`, which now quantises to the _next_ cycle (via ceiling) @thgund / @yaxu
	* There is now also `ftrigger` and `rtrigger` for floor (previous) and round (nearest) cycle
	* `whenmod`'s first two parameters are now rationals, and patternable. @th-four / @yaxu
        * `brand` and `brandBy` for continuous patterns of boolean values @yaxu
        * Two-way protocol between Tidal and SuperDirt, initially to receive available busses @telefon / @yaxu
        * bipolar waveforms - sine2, square2, tri2, saw2, cosine2 @yaxu
  

## 1.6.1 - We are not DJs
	* Patternise first parameter of chunk @lwlsn
	* Patternise fit parameter @bgold-cosmos
	* Increase upper bounds of random @yaxu
	* Switch travis to ubuntu bionic @yaxu

## 1.6.0 - Keep live coding live
	* Rollback to previous pattern on parse error @jwaldmann
	* Increased strictness to catch parse errors earlier @jwaldmann @yaxu
	* Support for superdirt 'panic' @yaxu
	* Increase hosc upper bounds to admin 0.18
	* New function 'splat' @yaxu
	* `quantise` now uses round, add qfloor, qceiling variants and qround alis @lwlsn
	* Add ghc 8.8.3 to travis @jwaldmann
	* Switch `substruct` to use binary pattern @yaxu

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

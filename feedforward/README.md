Feedforward, an experimental editor for TidalCycles
(c) Alex McLean 2025
Released under the terms of the GNU Public Licence version 3

Demo: https://www.youtube.com/watch?v=hkgKuD6Yvog

## Installation

### Installation on Debian

First install `ncurses`:

```
apt-get install libncurses5-dev
```

Then from the tidal folder, run 

```
cabal run feedforward
```

If that doesn't work, at the time of writing the latest releases of ghc 
require a tweak of the 'time' dependency:

```
cabal run --allow-newer=time feedforward
```

## VU meters

To get in-text VU meters on patterns, switch on RMS sending from SuperDirt.

Before starting SuperDirt, do this: `s.options.maxLogins = 8;`

After starting SuperDirt (e.g. with `SuperDirt.start`), do this:
`~dirt.startSendRMS;`.

Do all this *before* starting feedforward.

The VU meters will also work if you use the latest develop version of
superdirt.

## Usage

At the moment it sends a lot of info to stderr, which you'll need to
direct somewhere, e.g. by running like this:

`cabal run feedforward 2> err.txt`

At the time of writing..

* Some emacs keys work for navigation. (^n, ^p, ^a, ^e, ^b, ^f, ..)
* Alt-Enter to evaluate all the code in the buffer
* F10 quits
* ctrl-h hushes everything
* ctrl-l redraws the screen (in case something gets corrupted)
* Different patterns automatically get sent to different orbits
* There is no cut and paste yet
* alt-0 .. alt-10 toggles patterns on and off


By default, feedforward will work with SuperDirt, running on the same
computer. To switch between Classic dirt and SuperDirt, edit
Feedback/Edit.hs and look for `dirt = `... Set it to either `dirt =
Classic` or `dirt = Super`.

The stuff in `Drum/` is a server for laptop ensembles, for centralised
recording of keystrokes and for taking coordinated snapshots. Probably
not interesting to you just yet.


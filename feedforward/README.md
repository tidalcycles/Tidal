Feedforward, an experimental editor for TidalCycles
(c) Alex McLean 2021
Released under the terms of the GNU Public Licence version 3

https://github.com/yaxu/feedforward

Demo: https://www.youtube.com/watch?v=hkgKuD6Yvog

## Installation

Please let me (alex@slab.org) know if you get it
installed on Windows 10 and how. 

### Installation on Debian

First install `ncurses`:

```
apt-get install libncurses5-dev
```

Install feedforward (into ~/.cabal/bin) with:

```
git clone http://github.com/yaxu/feedforward
cd feedforward
cabal install
```

### Installation on Arch

Set these in `~/.cabal/config`

```
library-vanilla: True
shared: True
executable-dynamic: True

program-default-options
  ghc-options:
    -dynamic
```

Then do a `cabal update`

Then clone latest Tidal repository and do a `cabal install` there.

```
git clone https://github.com/tidalcycles/Tidal.git
cd Tidal
cabal install
```

Then clone this repository, and execute the following

```
git clone https://github.com/yaxu/feedforward.git
cd feedforward
cabal update
cabal install c2hs
cabal install ncurses --flag force-narrow-library
cabal install
```

### Mac OSX

If you do not have `ncurses` installed already, 
you can install it on OSX with:

```
brew install ncurses
```

`ncurses` install info available at 
https://gist.github.com/cnruby/960344#gistcomment-3236294

Next, install feedforward into `~/.cabal/bin` with:

```
git clone http://github.com/yaxu/feedforward
cd feedforward
cabal install
```

Then follow the usage instructions below.

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

`feedforward 2> err.txt`

If .cabal/bin isn't in your path, you could run it with:

`~/.cabal/bin/feedforward 2> err.txt`

At the time of writing..

* Some emacs keys work for navigation. (^n, ^p, ^a, ^e, ^b, ^f, ..)
* Alt-Enter to evaluate all the code in the buffer
* F2 replays a previous session (experimental)
* F10 quits
* ctrl-h hushes everything
* ctrl-l redraws the screen (in case something gets corrupted)
* Different patterns automatically get sent to different orbits
* There is no cut and paste yet
* Mouse clicks position the cursor on some terminals, but it's a bit
  clunky
* alt-0 .. alt-10 toggles patterns on and off


By default, feedforward will work with SuperDirt, running on the same
computer. To switch between Classic dirt and SuperDirt, edit
Feedback/Edit.hs and look for `dirt = `... Set it to either `dirt =
Classic` or `dirt = Super`.

The stuff in `Drum/` is a server for laptop ensembles, for centralised
recording of keystrokes and for taking coordinated snapshots. Probably
not interesting to you just yet.

## Contributions

This is very early stage software, but I'm nonetheless happy to hear
bug reports:

https://github.com/yaxu/feedforward/issues

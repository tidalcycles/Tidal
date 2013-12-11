# Installing Tidal under Linux

The following currently assumes a Debian, or Debian-derived Linux
distribution such as Ubuntu or Mint, although it should be quite easy
to adapt to another distribution.

Unless otherwise specified, you will need to run the commands below in
a terminal window.

## Installing Dirt

Tidal does not include a synthesiser, but instead communicates with an
external synthesiser using the Open Sound Control protocol. It has
been developed for use with a particular software sampler called
"dirt". You'll need to run it with "jack audio".

~~~~
    sudo apt-get install build-essential libsndfile1-dev libsamplerate0-dev \
                         liblo-dev libjack-jackd2-dev qjackctl jackd git
    git clone https://github.com/yaxu/Dirt.git
    cd Dirt
    make clean; make
~~~~
(On MacOS X, you should: `brew install liblo` before attempting to compile)

Then you'll have to start jack, using the 'qjackctl' app under Linux,
or otherwise from the commandline:

~~~~
    jackd -d alsa &
~~~~

(On MacOS X, you would do this instead: jackd -d coreaudio & )

If that doesn't work, you might well have something called
"pulseaudio" in control of your sound. In that case, this should work:

~~~~
    /usr/bin/pasuspender -- jackd -d alsa &
~~~~

And finally you should be able to start dirt with this:

~~~~
    ./dirt &
~~~~

If you have problems with jack, try enabling realtime audio, and
adjusting the settings by installing and using the "qjackctl"
software. Some more info is here: <https://help.ubuntu.com/community/HowToJACKConfiguration>


## Tidal

Tidal is embedded in the Haskell language, so you'll have to install
the haskell interpreter and some libraries, including tidal
itself. Under debian, you'd install haskell like this:

~~~~
   sudo apt-get install ghc6 zlib1g-dev cabal-install
~~~~

Or otherwise you could grab it from <http://www.haskell.org/platform/>

Once Haskell is installed, you can install tidal like this:

~~~~
   cabal update
   cabal install tidal
~~~~

## Emacs

Currently about the only interface to Tidal is the emacs
editor. Debian users can install emacs, along with its haskell
front-end, this way:

~~~~
    sudo apt-get install emacs24 haskell-mode
~~~~

To install haskell-mode on Mac OS X, either install a recent version of
emacs with brew, or install `package.el` manually, and then install 
haskell-mode via Marmalade. See haskell-mode documentation for details: <https://github.com/haskell/haskell-mode>

To install the emacs interface to tidal, you'll need to edit a
configuration file in your home folder called `.emacs`. If it doesn't
exist, create it. Then, add the following, replacing
`~/projects/tidal` with the location of the `tidal.el` file.

~~~~
    (add-to-list 'load-path "~/projects/tidal")
    (require 'tidal)
~~~~

If tidal.el did not come with this document, you can grab it here: <https://raw.github.com/yaxu/Tidal/master/tidal.el>

## Testing, testing...

Now start emacs, and open a new file called something like
"helloworld.tidal". Once the file is opened, you still have to start
tidal, you do that by typing `Ctrl-C` then `Ctrl-S`.

To check everything is working, type the following line, then type
`Ctrl-C` followed by `Ctrl-C`:

~~~~
    d1 $ brak $ sound "bd sn/2"
~~~~

All being well, you can progress to the introductory tutorial here:
  <https://github.com/yaxu/Tidal/blob/master/doc/tidal.md>

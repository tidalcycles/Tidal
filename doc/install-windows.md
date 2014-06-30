# Installing Tidal on Windows

This is a rough set of instructions based on information from the Tidal 
mailing list.


## Cygwin
First, install Cygwin from https://www.cygwin.com. In Cygwin, make sure the
following packages are installed:

~~~~
emacs
git
gcc
make
g++
libsndfile
libsndfile-devel
libsamplerate
libsamplerate-devel
~~~~

There might have been others (sorry)

## Portaudio

In Windows (not Cygwin) download Portaudio from http://www.portaudio.com. Unpack
the download with `tar fxvz`. After unpacking, from Cygwin, go to the directory
where you unpacked Portaudio and then run:

~~~~
./configure && make && make install
~~~~

## Liblo

In Windows (not Cygwin) download Liblo from http://liblo.sourceforge.net.
Unpack Liblo with `tar fxvz`, then in Cygwin go to the directory where you
unpakced Liblo and then run:

~~~~
./configure && make && make install
~~~~

## Dirt

In Cygwin:

~~~~
git clone http://github.com/yaxu/Dirt.git
~~~~

Then:

~~~~
cd Dirt
make dirt-pa
~~~~

Then you get a dirt-pa.exe that works. Maybe this even works on any
windows system without having to compile.. You'd need cygwin1.dll at
least though.

## Haskell

Download and install the haskell platform from https://www.haskell.org/platform/.

You will then either need to restart Cygwin or reload your Cygwin profile to re-load `cabal` your path.

Then back in Cygwin:

~~~~
cabal update; cabal install tidal
~~~~

## Run Tidal

Run Tidal in ghci or Emacs. More details tbd...


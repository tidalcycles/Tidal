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

## Portaudio

Download Portaudio from http://www.portaudio.com. In Cygwin, Unpack
the download with `tar fxvz`. After unpacking, from Cygwin, go to the directory
where you unpacked Portaudio and then run:

~~~~
./configure && make && make install
~~~~

## Liblo

Download Liblo from http://liblo.sourceforge.net.
In Cygwin, unpack Liblo with `tar fxvz`, then in Cygwin go to the directory where you
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

*You will then either need to restart Cygwin or reload your Cygwin profile to re-load `cabal` on your path.*

Then back in Cygwin:

~~~~
cabal update
cabal install tidal
~~~~

## Download and Configure Emacs

Download Emacs for Windows from http://ftp.gnu.org/gnu/emacs/windows/. Extract the zip file, then simply
run Emacs from `bin\runemacs.exe`.

### create .emacs

Next you will need to find or create the `.emacs` file located in your home directory. This is the Emacs config file. Your exact location may vary depending on how Emacs is installed/run, and which version of Windows you are using. On Windows 8, this location is probably: `C:\Users\<username>\AppData\Roaming`. If the `.emacs` file is not there, create it.

### Haskell-Mode

Haskell-mode needs to be installed in Emacs. The easiest way to do this in Windows is add the Marmalade package manager in Emacs. There are other ways to install haskell-mode (detailed at https://github.com/haskell/haskell-mode), but Marmalade is probably easiest.

Enable Marmalade by adding this to your `.emacs` file:

~~~~
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
~~~~

### Enable Tidal

In `.emacs` add this to enable Tidal:

~~~~
(add-to-list 'load-path "c:/projects/tidal")
  (require 'haskell-mode)
  (require 'tidal)
~~~~

... but replace `c:/projects/tidal` with the path to the folder that contains `tidal.el`. `tidal.el` can be obtained from the Tidal repository, at https://github.com/yaxu/Tidal.







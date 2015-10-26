# Installing Tidal on Windows

This is a rough set of instructions based on information from the Tidal 
mailing list.


## Cygwin
First, install Cygwin from https://www.cygwin.com. In Cygwin, make sure the
following packages are installed:

~~~~
git
gcc-core
gcc-g++
make
libsndfile
libsndfile-devel
libsamplerate
libsamplerate-devel
~~~~

## Portaudio

Download Portaudio from http://www.portaudio.com. In Cygwin, Unpack
the download with `tar -xvzf <filename>`. After unpacking, from Cygwin, go to the directory
where you unpacked Portaudio and then run:

~~~~
./configure && make && make install
~~~~

## Liblo

Download Liblo from http://liblo.sourceforge.net.
In Cygwin, unpack Liblo with `tar -xvzf <filename>`, then in Cygwin go to the directory where you
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

Next you will need to find or create the `.emacs` file located in your home directory. This is the Emacs config file. Your exact location may vary depending on how Emacs is installed/run. 

1. If you run `runemacs.exe` by double-clicking on it, then your `.emacs` file will probably be located at `C:\Users\<username>\AppData\Roaming\`. 
2. If you put the `runemacs.exe` folder on your path and run it from a command prompt, then your `.emacs` file will probably be located at `c:\users\<username>\`

Be aware of how you started `runemacs.exe`, and create the `.emacs` in the appropriate folder if it does not exist already. 

Alternately, you can try to have Emacs create the `.emacs` file for you automatically by changing a config setting from one of the Emacs menus and saving your configuration. 

### Haskell-Mode

Haskell-mode needs to be installed in Emacs. The easiest way to do this in Windows is add the Marmalade package manager in Emacs. There are other ways to install haskell-mode (detailed at https://github.com/haskell/haskell-mode), but Marmalade is probably easiest.

Enable Marmalade by adding this to your `.emacs` file:

~~~~
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
~~~~

Refresh the package index by `M-x package-refresh-contents`. Then install haskell-mode via `M-x package-install` [RET] `haskell-mode`.

*Note* the `M-x` key combination is `alt-x` in Windows.

### Enable Tidal

In `.emacs` add this to enable Tidal:

~~~~
(add-to-list 'load-path "c:/projects/tidal")
  (require 'haskell-mode)
  (require 'tidal)
~~~~

... but replace `c:/projects/tidal` with the path to the folder that contains `tidal.el`. `tidal.el` can be obtained from the Tidal repository, at https://github.com/yaxu/Tidal. The easiest way to use it is to clone the Tidal repository and modify the `.emacs` file to use the path where you cloned it:

~~~~
git clone https://github.com/yaxu/Tidal c:\tidal
~~~~

then:

~~~~
(add-to-list 'load-path "c:/tidal")
  (require 'haskell-mode)
  (require 'tidal)
~~~~






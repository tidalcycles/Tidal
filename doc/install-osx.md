Unless otherwise specified, the below commands should be typed or pasted into a terminal window.

Install homebrew:
```
ruby -e "$(curl -fsSL https://raw.github.com/mxcl/homebrew/go/install)"
```

Initialise homebrew:
```
brew doctor
```

Install emacs, and make it appear in your applications folder:
```
brew install emacs --cocoa
ln -s /usr/local/Cellar/emacs/24.3/Emacs.app /Applications
```

Install Dirt, a synth (well, more of a sampler) made to work with
Tidal. A homebrew 'recipe' for dirt does exist, but that doesn't come
with any sounds to play with, so for now it's probably easiest just
download it all from github and compile it as follows.

Install the liblo library, which the Dirt synth needs to compile:
```
brew install liblo
```

Install the "jack audio connection kit", Dirt needs it too:
```
brew install jack
```

---
* **Note:** If Homebrew's installation of Jack fails with a ``make`` error, you can use the [JackOSX installer](http://www.jackosx.com/download.html) instead. This will, however, add an additional step when installing Dirt (see below).

---

Get the sourcecode for the Dirt synth:
```
git clone https://github.com/yaxu/Dirt.git
```

Compile dirt:
```
cd Dirt
make clean; make
```

If you get errors for sndfile.h and samplerate.h, install libsndfile and libsamplerate via homebrew.

```
brew install libsndfile
```

then,

```
brew install libsamplerate
```

---
* **Note:** If Dirt fails to compile after using the JackOSX installer as above, you may need to add flags to the Makefile to specify the appropriate paths:
```
CFLAGS += -g -I/usr/local/include -Wall -O3 -std=gnu99 -DCHANNELS=2
LDFLAGS += -lm -L/usr/local/lib -llo -lsndfile -lsamplerate -ljack
```
---

Install Haskell from the binaries served at:

[https://www.haskell.org/platform/mac.html](https://www.haskell.org/platform/mac.html)

Or you might get it from homebrew (this takes a while)
```
brew install haskell-platform
```

Install Tidal (yeah!)
```
cabal update
cabal install tidal
```

Ok now time to configure emacs.. Do the following:
```
mkdir ~/tidal
cd ~/tidal
curl -L https://raw.githubusercontent.com/yaxu/Tidal/master/tidal.el > tidal.el
```

Then create a file in your home folder called .emacs (unless it exists already), then open the file in a text editor and insert the following lines:
```
(require 'package)
(add-to-list 'package-archives 
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(package-initialize)
(setq load-path (cons "~/tidal/" load-path))
(require 'tidal)
(setq tidal-interpreter "/usr/local/bin/ghci")
```

The above ensures that emacs has access to the extensions in the 'marmalade' repository (in particular, haskell-mode), that the tidal.el file you downloaded earlier is is loaded, and that tidal can find the haskell interpreter.

---
* **Note:** If you have already installed Haskell using the [Haskell Platform](http://www.haskell.org/platform/) installer, make the following change to the above:

```
(setq tidal-interpreter "/usr/bin/ghci")
```
---

Now start emacs (or if it's already loaded, restart it to make sure .emacs is read), it should be in your Applications folder (if you start it from the terminal it'll probably load an old version). Once emacs has started, press `alt-x` (i.e. hold down `alt` while pressing `x`) and type:
```
package-refresh-contents
```
Then do `alt-x` again and type:
```
package-install
```
and then:
```
haskell-mode
```

Ok everything should be pretty much ready to go. Now you just have to start jack audio, start jack and then get things going in emacs. So back in a terminal window, start jack:
```
jackd -d coreaudio &
```
Then start dirt:
```
./dirt &
```

Then back in emacs, open a file called something ending with .tidal, such as test.tidal

Then you start up tidal, by doing `ctrl-c` followed by `ctrl-s`

Then type something like
```
d1 $ sound "bd sn/2"
```

And finally do `ctrl-c` `ctrl-c`

Hopefully at this point you will hear a kick drum - snare! Make sure
your volume is up.

If all is well, join these instructions to continue exploring Tidal:

<https://github.com/yaxu/Tidal/blob/master/doc/tidal.md>

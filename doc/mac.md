Unless otherwise specified, the below commands should be typed or pasted into a terminal window.

Install homebrew:
```shell
ruby -e "$(curl -fsSL https://raw.github.com/mxcl/homebrew/go/install)"
```

Initialise homebrew:
```shell
brew doctor
```

# Install emacs, and make it appear in your applications folder:
brew install emacs --cocoa
ln -s /usr/local/Cellar/emacs/24.3/Emacs.app /Applications

# Install something that Dirt needs to compile:
brew install liblo

# Get the sourcecode for the Dirt synth:
git clone https://github.com/yaxu/Dirt.git

# Compile dirt:
cd Dirt
make clean; make

# Install the "jack audio connection kit", Dirt needs it:
brew install jack

# Install Haskell (this takes a while)
brew install haskell-platform

# Install Tidal (yeah!)
cabal update
cabal install tidal

# Ok now time to configure emacs.. Do the following:
mkdir ~/tidal
cd ~/tidal
curl https://raw.github.com/yaxu/Tidal/master/tidal.el > tidal.el

# Then create a file in your home folder called .emacs (unless it exists already)
# Open the file in a text editor and insert the following lines:
(require 'package)
(add-to-list 'package-archives 
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(package-initialize)
(setq load-path (cons "~/emacs-stuff/" load-path))
(require 'tidal)

# The above ensures that emacs has access to the extensions in the 'marmalade' repository (in particular, haskell-mode), and that the tidal.el file you downloaded earlier is is loaded.

# Now start emacs, it should be in your Applications folder (if you start it from the terminal it'll probably load an old version).
# Once emacs has started, hold down "alt" while pressing "x" and type:
package-refresh-contents
# Then do alt-x again and type:
package-install
# and then:
haskell-mode

# Ok now everything should be pretty much ready to go. Now you just have to start jack audio, start jack and then get things going in emacs.

# So back in a terminal window, start jack:
jackd -d coreaudio &
# Then start dirt:
./dirt &

# Then back in emacs, open a file called something ending with .tidal, such as test.tidal
# Then to start tidal, do ctrl-c followed by ctrl-s
# Then type something like
d1 $ sound "bd sn/2"
# And finally do ctrl-c ctrl-c
# Hopefully you will hear a kick drum - snare!

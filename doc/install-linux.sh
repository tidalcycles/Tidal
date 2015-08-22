#!/usr/bin/env bash
set -e

# This script installs tidal.
# It will get all its dependencies and put
# files into ~/tidal and a start script on the
# Desktop.
#
# This script has been tested with Ubuntu 13.10
# and Debian.

# prepare system
mkdir -p ~/tidal
cd ~/tidal
sudo apt-get -y install build-essential libsndfile1-dev libsamplerate0-dev \
    liblo-dev libjack-jackd2-dev qjackctl jackd git \
    ghc zlib1g-dev cabal-install \
    emacs24 haskell-mode

# install Dirt
if [ -d "Dirt" ]; then
	cd Dirt
	if [ ! -d ".git" ]; then
		>&2 echo "no git repository for 'Dirt' ... don't know what to do"
		exit 1
	fi
	git pull
else
	git clone https://github.com/yaxu/Dirt.git
	cd Dirt
fi
make clean; make

# actually install tidal
cabal update
cabal install cabal
cabal install tidal

# configure Emacs
mkdir -p ~/tidal/emacs
rm -f ~/tidal/emacs/tidal.el
wget -O ~/tidal/emacs/tidal.el https://raw.githubusercontent.com/yaxu/Tidal/master/tidal.el
touch ~/.emacs
if [ `grep "(add-to-list 'load-path \"~/tidal/emacs\")" ~/.emacs | wc -l` -ne 1 ]; then
	echo "(add-to-list 'load-path \"~/tidal/emacs\")" >> ~/.emacs
fi
if [ `grep "(require 'tidal)" ~/.emacs | wc -l` -ne 1 ]; then
	echo "(require 'tidal)" >> ~/.emacs
fi
sudo adduser $USER audio

# put starter on th desktop
cd ~/Desktop
rm -f start-tidal
wget http://yaxu.org/tmp/start-tidal
chmod u+x start-tidal

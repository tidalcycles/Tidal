#!/bin/bash

# Tested with Ubuntu 13.10

mkdir ~/tidal
cd ~/tidal
sudo apt-get -y install build-essential libsndfile1-dev libsamplerate0-dev \
    liblo-dev libjack-jackd2-dev qjackctl jackd git \
    ghc zlib1g-dev cabal-install \
    emacs24 haskell-mode

git clone https://github.com/yaxu/Dirt.git
cd Dirt
make clean; make

cabal update
cabal install tidal

mkdir ~/tidal/emacs
wget -O ~/tidal/emacs/tidal.el https://raw.githubusercontent.com/yaxu/Tidal/master/tidal.el
touch ~/.emacs
echo "(add-to-list 'load-path \"~/tidal/emacs\")" >> ~/.emacs
echo "(require 'tidal)" >> ~/.emacs
sudo adduser $USER audio

cd ~/Desktop
wget http://yaxu.org/tmp/start-tidal
chmod u+x start-tidal

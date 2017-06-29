#!/bin/bash

echo Welcome to the TidalCycles linux install script. This is known to
echo work on Ubuntu 17.04. It will probably only work with
echo linux distributions that have echo supercollider 1.3.7 or later.

echo Installing dependencies..
sudo apt-get update
sudo apt-get install supercollider sc3-plugins build-essential git qjackctl haskell-cabal zlib1g-dev libportmidi-dev libasound2-dev

echo Adding user to the 'audio' group
sudo adduser $USER audio

if [ -e /usr/bin/atom ]; then
    echo Atom already installed.
else
   echo Installing atom
   wget --output-document=/tmp/atom.deb http://atom.io/download/deb
   sudo dpkg -i /tmp/atom.deb
   sudo apt --fix-broken install -y
fi
   
echo Installing/updating atom tidalcycles package
apm install tidalcycles

echo Installing/updating the tidal pattern engine, and tidal-midi
cabal update
cabal install tidal
cabal install tidal-midi


mkdir -p ~/.local/share/SuperCollider/downloaded-quarks/
cd ~/.local/share/SuperCollider/downloaded-quarks/
if [ -d ./SuperDirt ]; then
    echo ** Updating SuperDirt
    cd SuperDirt
    git pull
    cd -
else
    echo ** Installing SuperDirt quark
    git clone https://github.com/musikinformatik/SuperDirt.git
fi

if [ -d ./Vowel ]; then
    echo ** Updating Vowel quark
    cd Vowel
    git pull
    cd -
else
    echo ** Installing Vowel quark
    git clone https://github.com/supercollider-quarks/Vowel.git
fi

if [ -d ./Dirt-Samples ]; then
    echo ** Updating Dirt-Samples quark
    cd Dirt-Samples
    git pull
    cd -
else
    echo ** Installing Dirt-Samples quark, this may take a little while
    git clone https://github.com/tidalcycles/Dirt-Samples.git
fi

echo Install process complete. It is a good idea to reboot now.

#!/bin/bash

echo -e "\n[ Welcome to the TidalCycles linux install script. This is known to\n  work on Ubuntu 17.04. It will probably only work with\n  linux distributions that have supercollider 1.3.7 or later. ]"

echo -e "\n[ Installing dependencies.. ]"
#sudo apt-get update
#sudo apt-get install build-essential git qjackctl cabal-install zlib1g-dev libportmidi-dev libasound2-dev

echo -e "\n[ Testing supercollider version ]"
if (apt-cache policy supercollider|grep Candidate|grep -q 3.6.6); then
    echo -e "\n[ Old supercollider version found.. Compiling ]"
else
    echo -e "\n[ Installing distro version of supercollider ]"
    sudo apt-get install supercollider sc3-plugins
fi
exit
   
echo -e "\n[ Adding user to the 'audio' group ]"
sudo adduser $USER audio

if [ -e /usr/bin/atom ]; then
   echo -e "\n[ Atom already installed ]"
else
   echo -e "\n[ Installing atom ]"
   wget --output-document=/tmp/atom.deb http://atom.io/download/deb
   sudo dpkg -i /tmp/atom.deb
   sudo apt --fix-broken install -y
fi

echo -e "\n[ Changing the default ghci path to stack ghci ]"

echo -e "\n[ Installing/updating atom tidalcycles package ]"
apm install tidalcycles

echo -e "\n[ Setting default ghci path to use stack ]"
perl -p -i.bak -e 's/default: "ghci"/default: "stack ghci"/' ~/.atom/packages/tidalcycles/lib/tidalcycles.js

echo -e "\n[ Installing/updating the tidal pattern engine ]"
stack setup
stack install tidal

mkdir -p ~/.local/share/SuperCollider/downloaded-quarks/
cd ~/.local/share/SuperCollider/downloaded-quarks/
if [ -d ./SuperDirt ]; then
    echo -e "\n[ Updating SuperDirt ]"
    cd SuperDirt
    git pull
    cd -
else
    echo -e "\n[ Installing SuperDirt quark ]"
    git clone https://github.com/musikinformatik/SuperDirt.git
fi

if [ -d ./Vowel ]; then
    echo -e "\n[ Updating Vowel quark ]"
    cd Vowel
    git pull
    cd -
else
    echo -e "\n[ Installing Vowel quark ]"
    git clone https://github.com/supercollider-quarks/Vowel.git
fi

if [ -d ./Dirt-Samples ]; then
    echo -e "\n[ Updating Dirt-Samples quark ]"
    cd Dirt-Samples
    git pull
    cd -
else
    echo -e "\n[ Installing Dirt-Samples quark, this may take a little while ]"
    git clone https://github.com/tidalcycles/Dirt-Samples.git
fi

echo -e "\n\nInstall process complete! It is a good idea to reboot now."

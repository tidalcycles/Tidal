---
category: multi_laptop
weight: -9
---
Dirt supports multiple Tidal connections. To do this, one laptop will run `dirt` and the others will evaluate `d1` (or `d2`, `d3`, `d4`, etc) to point to that IP address.

## Step 1: sync computer clocks

Ensure that the system clocks of all the computers are already in sync, 
via something like `ntpd`, or preferably `ptpd`.

## Step 2: sync to tempo clock

On the remote Tidal computer, sync the host's tempo clock by setting an environment variable, e.g.:

````
TIDAL_TEMPO_IP=<HOST IP ADDRESS> emacs &
````

Replace `<HOST IP ADDRESS>` with the host's IP address.

## Step 3: start Dirt on the host

Hopefully that was easy.

## Step 4: set up Dirt connection on another laptop
(optional, if the remote computer has its own dirt process running it can output sound to its own hardware, but it will stay synced with the host's tempo)

Start up a Tidal process in Emacs, then evaluate this code:

````{haskell}
d1 <- stream "<IP ADDRESS OF HOST>" 7771 dirt
````

But instead replace `<IP ADDRESS OF HOST>` with the host's IP address.

## Step 5: evaluate crazy sound on remote computer

````{haskell}
d1 $ slow 16 $ striate 128 $ sound "bd*12 sn*8 [hh sn bd]*4 sn*4 bd*4"
````

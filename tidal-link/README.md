# tidal-link

Ableton Link integration for Tidal

Tested to work on Windows, which should mean that it compiles C++
using the GHC packaged MinGW version of gcc/g++.

Tested to build and load on Linux, but issues with SuperCollider means it's not tested,

GHCI:
> ghci --version
> The Glorious Glasgow Haskell Compilation System, version 9.2.1

Cabal:
> cabal --version
> cabal-install version 3.6.2.0
> compiled using version 3.6.2.0 of the Cabal library

Built using
> cabal v1-build

or

> cabal v1-repl

Installs a shared library into cabal bin dir.
Cabal bin dir must therefore be on path or added as option to
ghci `-L<cabal bin dir>`.
The shared library must be loaded as well: `-ltidallink`.

Loading on Linux when using ghcup thus becomes `ghci -L~/.cabal/bin/ -ltidallink -XOverloadedStrings`

The shared library must be registered in Linux, so add the folder to /etc/ld.so.conf. In my case, the file now looks like
```
include /etc/ld.so.conf.d/*.conf
/home/pierre/.cabal/bin
```
Followed by `sudo ldconfig`


To run as executable, add to tidal-link.cabal:
```
executable link-tester
  ghc-options: -Wall
  hs-source-dirs:
                 src/hs

  default-language:    Haskell2010
  main-is: Main.hs
  other-modules:     Sound.Tidal.Link

  Build-depends:
      base >=4.8 && <5

  include-dirs:
    src/c
    link/include
    link/modules/asio-standalone/asio/include
  extra-libraries:
      stdc++
      iphlpapi
      winmm
      ws2_32
```
# tidal-link

Ableton Link integration for Tidal

Tested to work on Windows, which should mean that it compiles C++
using the GHC packaged MinGW version of gcc/g++.

Tested to build and load on Linux, but issues with SuperCollider on my machine means it's not tested,

GHCI:
> ghci --version
> The Glorious Glasgow Haskell Compilation System, version 9.2.1

Cabal:
> cabal --version
> cabal-install version 3.6.2.0
> compiled using version 3.6.2.0 of the Cabal library

On Windows, this installs a DLL into cabal bin dir.
The reason is due to problems reported in https://gitlab.haskell.org/ghc/ghc/-/issues/20918.
Please try to reproduce the bug on your Windows machine and report your findings!!
Cabal bin dir must therefore be on path or added as option to
ghci `-L<cabal bin dir>`.
The shared library must be loaded as well: `-ltidallink`.

Loading on Windows thus becomes something like this `ghci -LC:\\tools\\cabal\\bin -ltidallink -XOverloadedStrings`

On Linux, the mentioned bug does not appear. Link is thus statically linked and Tidal can be used as normal.

To run as executable on Windows, add to tidal-link.cabal:
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
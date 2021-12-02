# tidal-link

Ableton Link integration for Tidal

So far it is only a skeleton that builds with Ableton link included.

Only tested on Windows, which should mean that it compiles C++
using the GHC packaged MinGW version of gcc/g++.

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

Call hello function to see the code in action

Installs a shared library into cabal bin dir.
Cabal bin dir must therefore be on path or added as option to
ghci `-L<cabal bin dir>`.
The shared library must be loaded as well: `-ltidallink`.

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
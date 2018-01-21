# Community

The below might help, but to find people to ask questions about
getting started, join the tidal-innards channel on the TOPLAP slack:
  http://toplap.org/toplap-on-slack/

You can also ask on the mailing list:
  https://we.lurk.org/postorius/lists/tidal.we.lurk.org/

# Tidal

Tidal is written in the Haskell language, in particular using the ghc
compiler/interpreter. Some resources for learning Haskell:

* http://learnyouahaskell.com/
* http://haskellbook.com/

# Quick guide to contributing a change to Tidal

The main repository is maintained on github:
  https://github.com/tidalcycles/tidal

The SuperDirt repository is here:
  https://github.com/musikinformatik/SuperDirt

In both cases the 'master' branch contains the current release. Active
development takes place on a branch called VERSION-NUMBER-dev (at the
time of writing, 1.0-dev). To make a contribution, you could:

* Make a dev fork
* Make and test a change
* Keep your fork up to date with the master
* Make a pull request

Others may then review and comment on your pull request. Please do say
when you think it's ready to be accepted to make sure it's not being
overlooked.

# Workflow

You can accomplish a lot within a normal tidal buffer in e.g. atom,
emacs or vim, by defining and testing functions using `let`.

For making a change to Tidal itself, take a fork of the code and
switch to the active branch:

```
git clone https://github.com/tidalcycles/Tidal.git
cd Tidal
git checkout 1.0-dev
```

Then make a change and install it by running a bare `cabal install`
from within the Tidal folder. You'll need to restart the interpreter
within your editor to pick up the new version.

# A process for making a release

We haven't documented a clear process for this, but we'd like to
describe how to..

* Share with others for testing
* Tag a release
* Distribute via to hackage / stackage

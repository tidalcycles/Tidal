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

In both bases the 'master' branch contains the current release. Active
development takes place on a branch called VERSION-NUMBER-dev (at the
time of writing, 1.0-dev). To make a contribution, you could:

* Make a dev fork
* Make and test a change
* Keep your fork up to date with the master
* Make a pull request

# A process for making a release

We haven't documented a clear process for this, but we'd like to
describe how to..

* Share with others for testing
* Tag a release
* Distribute via to hackage / stackage

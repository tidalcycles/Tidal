The main website is here: http://tidalcycles.org/

# Community

The below might help, but to find people to ask questions about
getting started, visit the "innards" category in the tidalclub forum:
  https://club.tidalcycles.org/c/innards/11

and/or the tidal-innards channel on the TOPLAP slack:
  http://chat.toplap.org/

# Tidal

Tidal is written in the Haskell language, in particular using the ghc
compiler/interpreter. Some resources for learning Haskell can be found here: 
  https://tidalcycles.org/index.php/Haskell_resources

# Quick guide to contributing a change to Tidal

The main repository is maintained on github:
  https://github.com/tidalcycles/tidal

The SuperDirt repository is here:
  https://github.com/musikinformatik/SuperDirt

In both cases development takes place on the master branch. To make a
contribution, you could:

* Fork the repository
* Make and test a change locally
* Keep your fork up to date with the master
* Make a pull request

Others may then review and comment on your pull request. Please do say
when you think it's ready to be accepted to make sure it's not being
overlooked.

If any of this is unclear, or if you'd like more information about
development workflow, you are very welcome to join the
`#tidal-innards` channel on http://talk.lurk.org/ and ask questions
there.

# Testing

Use `cabal test` to run the test suite to look for regressions. Please
add tests for any new functionality. You can look for things that need
testing like this:

```
cabal install --only-dependencies
cabal configure --enable-coverage    # only need to do this the first time
cabal test --show-details=streaming
firefox dist/hpc/prof/html/tests/hpc_index.html
```

To run up your changes locally, install Tidal with `cabal install`. To remove them again and revert to the latest release, run `ghc-pkg unregister tidal-1.0.0` being sure to match up the version numbers. (note that ghc packaging is in a state of flux at the moment - this command might not actually work..)

# A process for making a release

We haven't documented a clear process for this, but we'd like to
describe how to..

* Share with others for testing
* Tag a release
* Distribute via hackage / stackage

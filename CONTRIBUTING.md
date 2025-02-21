The main website is here: http://tidalcycles.org/

# Community

You can join in with Tidal development and discussion around it, via the repository currently hosted on github:
  https://github.com/tidalcycles/tidal

The two other active Tidal forums are the Tidal and Strudel discord:
  https://discord.gg/CqWhZEfNbq

and the Tidal club discourse forum:
  https://club.tidalcycles.org/

Both have general areas for discussion, and 'innards' sections for discussing the technalities of developing tidal.


# Tidal

Tidal is written in the Haskell language, in particular using the ghc
compiler/interpreter. Some resources for learning Haskell can be found here: 
  https://tidalcycles.org/docs/advanced/understanding-innards/Haskell_resources/

# Quick guide to contributing a change to Tidal

As mentioned, the main repository is maintained on github:
  https://github.com/tidalcycles/tidal

The main Tidal development branch is called 'main'.

The SuperDirt repository is here:
  https://github.com/musikinformatik/SuperDirt

The main development branch there is called 'develop'.

To make a contribution, you could:

* Fork the repository
* Make and test a change locally
* Keep your fork up to date with the main branch
* Make a pull request

Others may then review and comment on your pull request. Please do say
when you think it's ready to be accepted to make sure it's not being
overlooked.

If any of this is unclear, or if you'd like more information about development workflow, you are very welcome to ask on one of the forums linked below. All questions are very welcome.

## Submodules

Tidal depends on git submodules. In particular, the tidal-link module depends on submodules `link` and its `asio-standalone` dependency. 

If cloning the respository for the first time, add `--recursive` to your `git` command, to download these dependencies.

If you already have cloned the tidal repository, those submodules can be loaded using `git submodule update --init --recursive` .

# Recommendations to handle forks and branches

In your forked repository: before doing anything, make sure that local files are up to date:

```
git checkout main
git fetch upstream
git pull upstream main
git push
```

For this to work, you will have had to have registered the upstream repository:
```
git remote add upstream git@github.com:tidalcycles/tidal.git
```

Then to work on something, create a fresh branch:
```
git checkout -b fix-some-issue
```
edit files, test, etc. Finally:
```
git commit -a
git push --set-upstream origin fix-some-issue
```

# Formatting

We use the [ormolu](https://hackage.haskell.org/package/ormolu) formatter to standardise source files. This is run automatically by a github action, but you can also run it yourself, or get your editor to run it for you.

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

# Making a Release

*Note: This may be incomplete&mdash;before making a release, it's a good idea to reach out to an existing project maintainer to double-check the process.*

First, you'll need to figure out the new version number. Tidal follows the [Haskell Package Versioning Policy](https://pvp.haskell.org/)&mdash;basically, for a given version (e.g. `v1.0.0`), only update the last number if you're releasing a minor, non-breaking change (so a bug fix release might be `v1.0.1`). A major release requires editing the first or second numbers (so a major release that substantially adds or changes functionality might be `v1.1.0`, and a release that rearchitects the fundamentals would be `v2.0.0`). Major releases include those that update dependencies to a new major release. It's also a good idea to do a major release for any bugfixes where performers have started using the "bug" for aesthetics.

## Get Permission
First, you need to do the following:

* Make sure that you have been given Owner permissions on the tidalcycles GitHub organization or the Tidal repository
* If you don't have one, [create a user account on Hackage](https://hackage.haskell.org/users/register-request). You'll then need to get upload permissions - the email you receive when you create your account will have details about this process, but at the time of writing you can do this by asking existing Tidal maintainers to approve you, by sharing a link with them. 
* Make sure that you've been added to the maintainers group for the tidal package on Hackage.

## Create a Draft Release in GitHub

* Draft a [new Tidal release](https://github.com/tidalcycles/Tidal/releases)
* The name of the release will be the human-readable nickname (some traditional form of pattern making or something else that strikes your fancy)
* For the tag, you can specify the next version in the form `v0.0.0` and GitHub will automatically tag the most recent commit whenever you publish the release
* The "Generate Release Notes" is an easy way to list all the relevant updates and new contributors. Feel free to edit this further as needed
* **Save this as a draft for now**

## Update the Repository

Push any final changes to the code, updating the following files:
* **[tidal.cabal](https://github.com/tidalcycles/Tidal/blob/1.10-dev/tidal.cabal)**: Change the version field
  * **Outdated Dependencies:** Run `cabal update` then `cabal outdated` to determine whether any of Tidal's dependencies are out of date, then update those as well
* **[CHANGELOG.md](https://github.com/tidalcycles/Tidal/blob/1.10-dev/CHANGELOG.md)**: Add your new version at the top (you can copy the release notes from your draft GitHub release)
* **[src/Sound/Tidal/Version.hs](https://github.com/tidalcycles/Tidal/blob/1.10-dev/src/Sound/Tidal/Version.hs)**: Update the version string here too. This is the version that's printed to the console when someone starts Tidal.
* **If any of the other packages (e.g. tidal-link) have changed**: Update the respective **.cabal** files for these packages, and then update dependency information in **tidal.cabal** as needed.

## Test and Package the Repository

* Run `cabal test` to make sure all the tests pass (see above for details).
* Run `cabal haddock` and watch for errors to test that Cabal can generate the documentation for the package.
* Run `cabal check` to check for any errors with the package metadata.
* Run `cabal sdist` to generate an archive for distribution.

## Upload and Test Releases

* [The Hackage upload page](https://hackage.haskell.org/upload) contains instructions and links for uploading a release archive. **Start by uploading a package candidate because a package release can't be changed!**
* To distribute a package candidate for testing, find the download link for the `.tar.gz` bundle on the Hackage page for the package candidate. This candidate version can be installed with the following command: `cabal v1-install [url]` (note that at this time, [the v1 install command is necessary for installing a library from a URL](https://github.com/haskell/cabal/issues/8335)).
* Once everyone is happy with the new version, go ahead and upload the archive as a package release and publish the release on GitHub!

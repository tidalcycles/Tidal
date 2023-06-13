To build an ad-hoc arch package:

* makepkg --nocheck -f --install

To update changes:

* upload release candidate to cabal (without --publish)
* update sha512sums and b2sums in PKGBUILD
* Do the above again

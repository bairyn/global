PLACEHOLDER
-----

0.2.1.0
-----
* Add instances to `UDEmpty`.

0.2.0.2
-----
* Add homepage and bug-reports to cabal file, linking to the github repository
  and its issue tracker, respectively.

0.2.0.1
-----
* Revise synopsis, because "Haskell2010 compatible" suggests that both the
  package and its dependencies don't require extensions, which is false.
  Apologies for the misinformation!

0.2.0.0
-----
* Support for `template-haskell-2.8.0.0`.
* Drop support for Control.Concurrent.SampleVar, which is first removed in
  `base-4.7.0.0`.  This entails a major API change, so one of the first two
  components is incremented.
* Replace `syntax-trees` dependency with fork called
  `syntax-trees-fork-bairyn` that is able to be built.
* Added `CHANGELOG.md`.

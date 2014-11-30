PLACEHOLDER
-----

0.2.0.0
-----
* Support for `template-haskell-2.8.0.0`.
* Drop support for Control.Concurrent.SampleVar, which is first removed in
  `base-4.7.0.0`.  This entails a major API change, so one of the first two
  components is incremented.
* Replace `syntax-trees` dependency with fork called
  `syntax-trees-fork-bairyn` that is able to be built.
* Added `CHANGELOG.md`.

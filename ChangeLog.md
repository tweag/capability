# Revision history for capability

## 0.5.0.0 -- 2021-07-21

* Fix compatibility with GHC 9.0.
  See [#96](https://github.com/tweag/capability/pull/96)

* Remove `Capability.Writer.Discouraged`.
  This module incurred a dependency on monad-unlift which is no longer
  available for GHC 9.0. Given that its use was discouraged, it was deemed best
  to remove it.
  See [#96](https://github.com/tweag/capability/pull/96)

* Added `censor` function to `Capability.Writer`.
  See [#94](https://github.com/tweag/capability/pull/94)

## 0.4.0.0 -- 2021-03-15

* Fix infinite loop in `writer`.
  See [#85](https://github.com/tweag/capability/pull/85)

* Introduce `Capability.Reflection` to define ad-hoc interpreters.
  See [#86](https://github.com/tweag/capability/pull/86)

* Fix compatibility with GHC 8.10.
  See [#87](https://github.com/tweag/capability/pull/87)

## 0.3.0.0 -- 2020-03-19

* Rename HasStream to HasSink, for symmetry.
  See [#75](https://github.com/tweag/capability/pull/75)

* Introduce HasSource, a superclass of HasReader.
  See [#75](https://github.com/tweag/capability/pull/75)

* Make HasSource and HasSink superclasses of HasState.
  See [#75](https://github.com/tweag/capability/pull/75)

* Introduce `derive` to run an action that requires additional capabilities.
  See [#74](https://github.com/tweag/capability/pull/74)
  and [#83](https://github.com/tweag/capability/pull/83)

* Handlers `zoom` and `magnify` can now carry capabilities over from the context.
  See [#73](https://github.com/tweag/capability/pull/73)

* Introduce functional capabilities and the `TypeOf` type family.
  See [#72](https://github.com/tweag/capability/pull/72)

## 0.2.0.0 -- 2019-03-22

* Make HasStream a superclass of HasWriter.
  See [#64](https://github.com/tweag/capability/pull/64)
* Bumped version bounds on generic-lens.
  See [#67](https://github.com/tweag/capability/pull/67)

## 0.1.0.0 -- 2018-10-04

* Initial release.

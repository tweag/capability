# capability: effects, extensionally

This package defines strategies to generate implementations of capabilities
using the `DerivingVia` language extension added in GHC 8.6.

This package is not available on Hackage, yet, as some of its dependencies
have not been updated to GHC 8.6, yet.

Haddocks can be found on the [CircleCI project][circleci]
on the artifacts tab of a successful build.

[circleci]: https://circleci.com/gh/tweag/capabilities-via/tree/master

## Nix Shell

Some of this package's dependencies require patches to build with GHC 8.6.
These patches are defined in
[`nix/haskell/default.nix`](nix/haskell/default.nix).
A development environment with all patched dependencies in scope is defined in
[`shell.nix`](shell.nix).

## Cachix Nix Cache

A Nix cache for this package's dependencies is provided via [cachix][cachix].
If you have [cachix][cachix] installed, then you can activate it by executing

```
$ cachix use capabilities-via
```

[cachix]: https://cachix.org/

## Build

The build instructions assume that you have [Nix][nix] installed.
Execute the following command to build the library.

```
$ nix-shell --pure --run "cabal configure"
$ nix-shell --pure --run "cabal build"
```

[nix]: https://nixos.org/nix/

## Examples

An example is provided in [`WordCount`](examples/WordCount.hs).
Execute the following commands to try it out:

```
$ nix-shell --pure --run "cabal configure --enable-tests"
$ nix-shell --pure --run "cabal repl examples"

ghci> :set -XOverloadedStrings
ghci> wordAndLetterCount "ab ba"
Letters
'a': 2
'b': 2
Words
"ab": 1
"ba": 1
```

To execute all examples and see if they produce the expected results run

```
$ nix-shell --pure --run "cabal test examples --show-details=streaming --test-option=--color"
```

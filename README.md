# capabilities-via

This package defines strategies to generate implementations of capabilities
using the `DerivingVia` language extension added in GHC 8.6.

## Examples

An example is provided in [`Example.CountLog`](src/Example/CountLog.hs).
Execute the following commands to try it out:

```
$ nix-shell --pure --run "cabal repl"

ghci> runLogM regularLogger $ logNum 4
num: 4
ghci> runLogM loudLogger $ logNum 4
NUM: 4
ghci> runCounterM doubleCount
(2,2)
ghci> runCounter'M doubleCount
2
ghci> runCountLogM mixed
num: 2
num: 4
ghci> runCountLogM $ logNum 4
num: 4
```

## Nix Shell

Many of this package's dependencies require patches to build with GHC 8.6.
These patches are defined [`nix/haskell/default.nix`](nix/haskell/default.nix).
A development environment with all patched dependencies in scope is defined in
[`shell.nix`](shell.nix). When additional dependencies are added to the Cabal
file, execute the following line to add these new dependencies to the Nix
Shell.

```
$ ./nix/scripts/update-haskell-deps
```

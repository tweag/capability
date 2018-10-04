# capability: effects, extensionally

A capability is a type class that says explicitly which effects
a function is allowed to use. The [`mtl`][mtl] works like this too.
But unlike the `mtl`, this library decouples effects from their
implementation. What this means in practice:

- You can implement large sets of capabilities using the
  efficient [`ReaderT` pattern][readert], rather than a slow monad
  transformer stack.
- Capabilities compose well: e.g. it's easy to have multiple reader
  effects.
- You can use a writer effect without implementing it as a writer
  monad (which is known to [leak space][writer-space-leak]).
- You can reason about effects. For instance, if a monad provides a
 reader effect at type `IORef A`, it also provides a state effect at type `A`

For more on these, you may want to read the announcement [blog
 post][blog].

This library is an alternative to the [`mtl`][mtl]. It defines a set
of standard, reusable capability type classes, such as the `HasReader`
and `HasState` type classes, which provide the standard reader and
state effects, respectively.

Where `mtl` instances only need to be defined once and for all,
capability-style programming has traditionally suffered from verbose
boilerplate: rote instance definitions for every new implementation of
the capability. Fortunately GHC 8.6 introduced
the [`DerivingVia`][deriving-via] language extension. We use it to
remove the boilerplate, turning capability-style programming into an
appealing alternative to `mtl`-style programming. The
[`generic-lens`][generic-lens] library is used to access fields of
structure in the style of the [`ReaderT` pattern][readert].

An additional benefit of separating capabilities from their
implementation is that they avoid a pitfall of the `mtl`. In the
`mtl`, two different `MonadState` are disambiguated by their types,
which means that it is difficult to have two `MonadState Int` in the
same monad stack. Capability type classes are parameterized by a name
(also known as a *tag*). This makes it possible to combine multiple
versions of the same capability. For example,

```haskell
twoStates :: (HasState "a" Int m, HasState "b" Int m) => m ()
```

Here, the tags `"a"` and `"b"` refer to different state spaces.

In summary, compared to the `mtl`:

- capabilities represent what effects a function can use, rather than
  how the monad is constructed;
- capabilities are named, rather than disambiguated by type;
- capabilites are discharged with deriving-via combinators
  and [`generic-lens`][generic-lens], rather than with instance
  resolution.

An example usage looks like this:

``` haskell
testParity :: (HasReader "foo" Int m, HasState "bar" Bool m) => m ()
testParity = do
  num <- ask @"foo"
  put @"bar" (even num)

data Ctx = Ctx { foo :: Int, bar :: IORef Bool }
  deriving Generic

newtype M a = M { runM :: Ctx -> IO a }
  deriving (Functor, Applicative, Monad) via ReaderT Ctx IO
  -- Use DerivingVia to derive a HasReader instance.
  deriving (HasReader "foo" Int) via
    -- Pick the field foo from the Ctx record in the ReaderT environment.
    Field "foo" "ctx" (MonadReader (ReaderT Ctx IO))
  -- Use DerivingVia to derive a HasState instance.
  deriving (HasState "bar" Bool) via
    -- Convert a reader of IORef to a state capability.
    ReaderIORef (Field "bar" "ctx" (MonadReader (ReaderT Ctx IO)))

example :: IO ()
example = do
    rEven <- newIORef False
    runM testParity (Ctx 2 rEven)
    readIORef rEven >>= print
    runM testParity (Ctx 3 rEven)
    readIORef rEven >>= print
```

For more complex examples, see the [Examples section](#examples) and
the [`examples` subtree](./examples).

This package is not available on Hackage yet, as some of its
dependencies have not been updated to GHC 8.6, yet.

API documentation can be found in the artifacts tab of any successful
build in the [CircleCI project][circleci].

[circleci]: https://circleci.com/gh/tweag/capabilities-via/tree/master
[mtl]: http://hackage.haskell.org/package/mtl
[blog]: https://www.tweag.io/posts/2018-10-04-capability.html
[deriving-via]: https://downloads.haskell.org/~ghc/8.6.1/docs/html/users_guide/glasgow_exts.html#deriving-via
[generic-lens]: https://hackage.haskell.org/package/generic-lens
[readert]: https://www.fpcomplete.com/blog/2017/06/readert-design-pattern
[writer-space-leak]: https://blog.infinitenegativeutility.com/2016/7/writer-monads-and-space-leaks

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

## Build instructions

### Nix Shell

Some of this package's dependencies require patches to build with GHC 8.6.
These patches are defined in
[`nix/haskell/default.nix`](nix/haskell/default.nix).
A development environment with all patched dependencies in scope is defined in
[`shell.nix`](shell.nix).

### Cachix Nix Cache

A Nix cache for this package's dependencies is provided via [cachix][cachix].
If you have [cachix][cachix] installed, then you can activate it by executing

```
$ cachix use tweag
```

[cachix]: https://cachix.org/

### Build

The build instructions assume that you have [Nix][nix] installed.
Execute the following command to build the library.

```
$ nix-shell --pure --run "cabal configure"
$ nix-shell --pure --run "cabal build"
```

[nix]: https://nixos.org/nix/

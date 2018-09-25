# 2018.09.21

## Capability type-class laws

The capabilities `HasReader`, `HasState`, and `HasWriter` provide the same
interface as the corresponding Mtl type classes, apart from the tags.
Unfortunately, neither the Mtl documentation, nor the transformers documentation
specify any laws for these type classes. A discussion about this can be
found on the [mtl issue tracker](https://github.com/haskell/mtl/issues/5).

This library does define laws for the capability classes based on the above
mentioned mtl issue. At this point these are not definitive.
A detailed discussion of the state laws, and formal proofs written in Agda
can be found can be found in
[this](http://gallium.inria.fr/blog/lawvere-theories-and-monads/)
blog post by Pierre Dagand.

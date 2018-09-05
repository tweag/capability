self: super:

{
  haskell = super.haskell // {
    packages = super.haskell.packages // {
      ghc843 = super.haskell.packages.ghc843.override {
        overrides = hsself: hssuper: {
          capabilities-via = hssuper.callPackage ./capabilities-via {};
        };
      };
      ghc861 = super.haskell.packages.ghc861.override {
        overrides = hsself: hssuper: {
          capabilities-via = hssuper.callPackage ./capabilities-via {};

          # blaze-markup's test-suite requires tasty.
          # tasty doesn't build with GHC 8.6.
          blaze-markup = super.haskell.lib.dontCheck hssuper.blaze-markup;
          generic-lens = super.haskell.lib.dontCheck hssuper.generic-lens;
          # temporary's test-suite requires tasty.
          # tasty doesn't build with GHC 8.6.
          temporary = super.haskell.lib.dontCheck hssuper.temporary;

          cabal-doctest = super.haskell.lib.doJailbreak hssuper.cabal-doctest;
          contravariant = super.haskell.lib.doJailbreak hssuper.contravariant;
          doctest = super.haskell.lib.doJailbreak hssuper.doctest;
          free = super.haskell.lib.doJailbreak hssuper.free;
          inspection-testing = super.haskell.lib.doJailbreak hssuper.inspection-testing;
          safe-exceptions = super.haskell.lib.doJailbreak hssuper.safe-exceptions;
          split = super.haskell.lib.doJailbreak hssuper.split;
          StateVar = super.haskell.lib.doJailbreak hssuper.StateVar;
          unliftio-core = super.haskell.lib.doJailbreak hssuper.unliftio-core;

          hspec-jenkins = super.haskell.lib.appendPatch hssuper.hspec-jenkins
            ./hspec-jenkins-hspec_2_5_1.patch
          ;

          adjunctions = super.haskell.lib.overrideCabal hssuper.adjunctions (attrs: {
            postPatch = ''
              ${attrs.postPatch or ""}
              sed -i '
                /import Data\.Functor\.Contravariant$/s/import/import "contravariant"/;
                1i{-# LANGUAGE PackageImports #-}' \
                src/Control/Monad/Trans/Contravariant/Adjoint.hs \
                src/Data/Functor/Contravariant/Adjunction.hs \
                src/Data/Functor/Contravariant/Rep.hs
            '';
          });
          foldl = super.haskell.lib.overrideCabal hssuper.foldl (attrs: {
            jailbreak = true;
            postPatch = ''
              ${attrs.postPatch or ""}
              sed -i '
                /import Data\.Functor\.Contravariant/s/import/import "contravariant"/;
                1i{-# LANGUAGE PackageImports #-}' \
                src/Control/Foldl.hs
            '';
          });
          invariant = super.haskell.lib.overrideCabal hssuper.invariant (attrs: {
            postPatch = ''
              ${attrs.postPatch or ""}
              sed -i '
                /import\s\+Data\.Functor\.Contravariant/s/import/import "contravariant"/;
                1i{-# LANGUAGE PackageImports #-}' \
                src/Data/Functor/Invariant.hs
            '';
          });
          kan-extensions = super.haskell.lib.overrideCabal hssuper.kan-extensions (attrs: {
            postPatch = ''
              ${attrs.postPatch or ""}
              sed -i '
                /import Data\.Functor\.Contravariant$/s/import/import "contravariant"/;
                1i{-# LANGUAGE PackageImports #-}' \
                src/Data/Functor/Contravariant/Coyoneda.hs \
                src/Data/Functor/Contravariant/Day.hs \
                src/Data/Functor/Contravariant/Yoneda.hs
            '';
          });
          lens = super.haskell.lib.overrideCabal hssuper.lens (attrs: {
            jailbreak = true;
            postPatch = ''
              ${attrs.postPatch or ""}
              sed -i '
                /import\s\+Data\.Functor\.Contravariant/s/import/import "contravariant"/;
                1i{-# LANGUAGE PackageImports #-}' \
                $(grep -Rl "import\s\+Data\.Functor\.Contravariant$")
            '';
          });
          profunctors = super.haskell.lib.overrideCabal hssuper.profunctors (attrs: {
            jailbreak = true;
            postPatch = ''
              ${attrs.postPatch or ""}
              sed -i '
                /import Data\.Functor\.Contravariant/s/import/import "contravariant"/;
                1i{-# LANGUAGE PackageImports #-}' \
                src/Data/Profunctor/Unsafe.hs \
                src/Data/Profunctor/Strong.hs
            '';
          });
          semigroupoids = super.haskell.lib.overrideCabal hssuper.semigroupoids (attrs: {
            jailbreak = true;
            postPatch = ''
              ${attrs.postPatch or ""}
              # The containers version constraint is hidden behind a cabal flag,
              # which seems to confuse jailbreak-cabal.
              sed -i '
                /containers >=/s/containers.*$/containers/
                ' \
                semigroupoids.cabal
              sed -i '
                /import Data\.Functor\.Contravariant/s/import/import "contravariant"/;
                1i{-# LANGUAGE PackageImports #-}' \
                src/Data/Semigroupoid.hs
            '';
          });
          unordered-containers = hssuper.unordered-containers.overrideAttrs (attrs: {
            patchPhase = ''
              ${attrs.patchPhase or ""}
              sed -i '230s/M\.fold/M.foldr/' tests/HashMapProperties.hs
            '';
          });
          vector-algorithms = hssuper.vector-algorithms.overrideAttrs (attrs: {
            postPatch = ''
              ${attrs.postPatch or ""}
              # See https://ghc.haskell.org/trac/ghc/wiki/Migration/8.6#DPHisgone
              sed -i 's/-Odph/-O2 -fmax-simplifier-iterations=20/' vector-algorithms.cabal
            '';
          });
        };
      };
    };
  };
}

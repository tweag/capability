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

          # generic-lens's inspection-testing test-suite fails.
          generic-lens = super.haskell.lib.dontCheck hssuper.generic-lens;

          contravariant = super.haskell.lib.doJailbreak hssuper.contravariant;
          doctest = super.haskell.lib.doJailbreak hssuper.doctest;
          free = super.haskell.lib.doJailbreak hssuper.free;
          safe-exceptions = super.haskell.lib.doJailbreak hssuper.safe-exceptions;
          unliftio-core = super.haskell.lib.doJailbreak hssuper.unliftio-core;

          hspec-jenkins = super.haskell.lib.appendPatch hssuper.hspec-jenkins
            (super.fetchpatch {
              name = "hspec-jenkins-pr-5.patch";
              url = "https://github.com/worksap-ate/hspec-jenkins/pull/5.patch";
              sha256 = "0vzldzfmbs99rssi6yns2fnv0p6jcyc9zsp6a64b4ffm6swqhq7w";
            })
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
          haskell-src-exts = super.haskell.lib.overrideCabal hssuper.haskell-src-exts (attrs: {
            jailbreak = true;
            postPatch = ''
              ${attrs.postPatch or ""}
              sed -i '
                1i{-# LANGUAGE NoMonadFailDesugaring #-}' \
                src/Language/Haskell/Exts/InternalLexer.hs \
                src/Language/Haskell/Exts/ParseUtils.hs
            '';
          });
          hspec-core = super.haskell.lib.overrideCabal hssuper.hspec-core (attrs: {
            postPatch = ''
              ${attrs.postPatch or ""}
              sed -i '
                1i{-# LANGUAGE NoMonadFailDesugaring #-}' \
                test/Test/Hspec/Core/Example/LocationSpec.hs
            '';
          });
          inspection-testing = super.haskell.lib.overrideCabal hssuper.inspection-testing (attrs: {
            jailbreak = true;
            postPatch = ''
              ${attrs.postPatch or ""}
              sed -i '
                1i{-# LANGUAGE NoMonadFailDesugaring #-}' \
                Test/Inspection/Plugin.hs
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
          polyparse = super.haskell.lib.overrideCabal hssuper.polyparse (attrs: {
            jailbreak = true;
            postPatch = ''
              ${attrs.postPatch or ""}
              sed -i '
                1i{-# LANGUAGE NoMonadFailDesugaring #-}' \
                src/Text/Parse.hs \
                src/Text/Parse/ByteString.hs
            '';
          });
          profunctors = super.haskell.lib.overrideCabal hssuper.profunctors (attrs: {
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
          unliftio = hssuper.unliftio.overrideAttrs (attrs: {
            patchPhase = ''
              ${attrs.patchPhase or ""}
              sed -i '
                168s/Int/Natural/;
                /import/iimport GHC.Natural (Natural)
                ' src/UnliftIO/STM.hs
            '';
          });
          unordered-containers = hssuper.unordered-containers.overrideAttrs (attrs: {
            patchPhase = ''
              ${attrs.patchPhase or ""}
              sed -i '230s/M\.fold/M.foldr/' tests/HashMapProperties.hs
            '';
          });
        };
      };
    };
  };
}

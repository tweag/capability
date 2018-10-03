self: super:

{
  haskell = super.haskell // {
    packages = super.haskell.packages // {
      ghc843 = super.haskell.packages.ghc843.override {
        overrides = hsself: hssuper: {
          capability = hssuper.callCabal2nix "capability" ../.. {};
        };
      };
      ghc861 = super.haskell.packages.ghc861.override {
        overrides = hsself: hssuper: {
          capability = hssuper.callCabal2nix "capability" ../.. {};

          # generic-lens's inspection-testing test-suite fails.
          generic-lens = super.haskell.lib.dontCheck hssuper.generic-lens;

          free = super.haskell.lib.doJailbreak hssuper.free;
          lens = super.haskell.lib.doJailbreak hssuper.lens;

          hspec-jenkins = super.haskell.lib.appendPatch hssuper.hspec-jenkins
            (super.fetchpatch {
              name = "hspec-jenkins-pr-5.patch";
              url = "https://github.com/worksap-ate/hspec-jenkins/pull/5.patch";
              sha256 = "0vzldzfmbs99rssi6yns2fnv0p6jcyc9zsp6a64b4ffm6swqhq7w";
            })
          ;

          inspection-testing = super.haskell.lib.overrideCabal hssuper.inspection-testing (attrs: {
            jailbreak = true;
            postPatch = ''
              ${attrs.postPatch or ""}
              sed -i '
                1i{-# LANGUAGE NoMonadFailDesugaring #-}' \
                Test/Inspection/Plugin.hs
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
              '';
          });
          unliftio = hssuper.unliftio.overrideAttrs (attrs: {
            jailbreak = true;
            patchPhase = ''
              ${attrs.patchPhase or ""}
              sed -i '
                168s/Int/Natural/;
                /import/iimport GHC.Natural (Natural)
                ' src/UnliftIO/STM.hs
            '';
          });
        };
      };
    };
  };
}

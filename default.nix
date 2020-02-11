{ pkgs ? (import ./nix/nixpkgs) }:

let
  compiler = "ghc865";
  source = pkgs.lib.sourceByRegex ./. [
    "^.*\.md$"
    "^capability\.cabal$"
    "^examples.*$"
    "^src.*$"
  ];
  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = capabilityOverlay;
  };
  capabilityOverlay = self: super: {
    "capability" = super.callCabal2nix "capability" source { };
  };
in {
  capability = haskellPackages.capability;
  shell = haskellPackages.shellFor {
    packages = ps: [
      ps.capability
    ];
    buildInputs = [
      haskellPackages.cabal-install
      haskellPackages.ghcid
    ];
  };
}

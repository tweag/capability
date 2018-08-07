# https://github.com/NixOS/nixpkgs/pull/30399#issuecomment-340420000
#
# Pure fetchTarball with sha256 that works in nix 1.11. This will go away
# once using nix 1.12+.
let
  spec = builtins.fromJSON (builtins.readFile ./src.json);
  src = import <nix/fetchurl.nix> {
    url = "https://github.com/${spec.owner}/${spec.repo}/archive/${spec.rev}.tar.gz";
    inherit (spec) sha256;
  };
  nixcfg = import <nix/config.nix>;
in builtins.derivation {
  system = builtins.currentSystem;
  name = "${src.name}-unpacked";
  builder = nixcfg.shell;
  inherit src;
  args = [
    (builtins.toFile "builder" ''
      $coreutils/mkdir $out
      cd $out
      $gzip -d < $src | $tar -x --strip-components=1
    '')
  ];
  coreutils = builtins.storePath nixcfg.coreutils;
  tar = builtins.storePath nixcfg.tar;
  gzip = builtins.storePath nixcfg.gzip;
}

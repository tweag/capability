let
  rev = "f5f6dc053b1a0eca03c853dad710f3de070df24e";
  sha256 = "0kxp8fmcdw2nla5kficm7ffbllczwh7b7nqagwr4djs417rf4wma";
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };
  pkgs = import nixpkgs { config.allowUnfree = true; };
in pkgs

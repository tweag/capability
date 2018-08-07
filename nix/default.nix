{ nixpkgsSrc ? ./nixpkgs }:
import (import nixpkgsSrc) {
  config = { };
  overlays = [ (import ./haskell) ];
}

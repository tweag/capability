{ mkDerivation, base, containers, deriving-compat, dlist
, exceptions, generic-lens, hspec, lens, monad-control
, monad-unlift, mtl, mutable-containers, primitive, roles, silently
, stdenv, streaming, transformers
}:
mkDerivation {
  pname = "capabilities-via";
  version = "0.1.0.0";
  src = ../../..;
  libraryHaskellDepends = [
    base containers deriving-compat dlist exceptions generic-lens lens
    monad-control monad-unlift mtl mutable-containers primitive roles
    streaming transformers
  ];
  testHaskellDepends = [ base hspec mtl silently ];
  homepage = "https://github.com/tweag/capabilities-via";
  license = stdenv.lib.licenses.bsd3;
}

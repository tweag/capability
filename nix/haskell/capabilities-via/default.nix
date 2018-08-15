{ mkDerivation, base, containers, dlist, exceptions, generic-lens
, hspec, lens, monad-control, monad-unlift, mtl, mutable-containers
, primitive, silently, stdenv, streaming, transformers
}:
mkDerivation {
  pname = "capabilities-via";
  version = "0.1.0.0";
  src = ../../..;
  libraryHaskellDepends = [
    base dlist exceptions generic-lens lens monad-control monad-unlift
    mtl mutable-containers primitive streaming transformers
  ];
  testHaskellDepends = [
    base containers hspec mtl silently streaming
  ];
  homepage = "https://github.com/tweag/capabilities-via";
  license = stdenv.lib.licenses.bsd3;
}

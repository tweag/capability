{ mkDerivation, base, deriving-compat, generic-lens, lens
, monad-control, mtl, roles, stdenv, streaming, transformers
}:
mkDerivation {
  pname = "capabilities-via";
  version = "0.1.0.0";
  src = ../../..;
  libraryHaskellDepends = [
    base deriving-compat generic-lens lens monad-control mtl roles
    streaming transformers
  ];
  homepage = "https://github.com/tweag/capabilities-via";
  license = stdenv.lib.licenses.bsd3;
}

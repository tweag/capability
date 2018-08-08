{ mkDerivation, base, deriving-compat, generic-lens, lens, mtl
, stdenv, transformers
}:
mkDerivation {
  pname = "capabilities-via";
  version = "0.1.0.0";
  src = ../../..;
  libraryHaskellDepends = [
    base deriving-compat generic-lens lens mtl transformers
  ];
  doHaddock = false;
  homepage = "https://github.com/tweag/capabilities-via";
  license = stdenv.lib.licenses.bsd3;
}

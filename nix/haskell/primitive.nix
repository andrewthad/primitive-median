{ mkDerivation, base, fetchgit, ghc-prim, stdenv, transformers }:
mkDerivation {
  pname = "primitive";
  version = "0.6.3.0";
  src = fetchgit {
    url = "https://github.com/andrewthad/primitive";
    sha256 = "0v571dg5w50mby1vx50w5dawgpynw3rgsxrsifzjh42p4lz8a7kk";
    rev = "3c013ce26e5bc488c75ea1e19e4f072ec3cd14f2";
  };
  libraryHaskellDepends = [ base ghc-prim transformers ];
  homepage = "https://github.com/haskell/primitive";
  description = "Primitive memory-related operations";
  license = stdenv.lib.licenses.bsd3;
}

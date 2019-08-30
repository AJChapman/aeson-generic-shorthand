{ mkDerivation, aeson, base, casing, stdenv }:
mkDerivation {
  pname = "aeson-generic-shorthand";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ aeson base casing ];
  description = "A convenient shorthand for defining ToJSON and FromJSON instances";
  license = stdenv.lib.licenses.bsd3;
}

{ mkDerivation, base, bytestring, machines, stdenv }:
mkDerivation {
  pname = "machines-bytestring";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base bytestring machines ];
  description = "ByteString support for machines";
  license = stdenv.lib.licenses.bsd3;
}

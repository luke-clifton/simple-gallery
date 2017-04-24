{ mkDerivation, base, bytestring, directory, filelock, filepath
, imagemagick, lucid, mtl, optparse-applicative, stdenv, text
}:
mkDerivation {
  pname = "simple-gallery";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring directory filelock filepath imagemagick lucid mtl
    optparse-applicative text
  ];
  description = "Simple CGI image gallery";
  license = stdenv.lib.licenses.bsd2;
}

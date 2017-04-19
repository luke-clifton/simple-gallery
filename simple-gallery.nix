{ mkDerivation, base, bytestring, directory, filelock, filepath
, imagemagick, lucid, mtl, stdenv, text
}:
mkDerivation {
  pname = "simple-gallery";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring directory filelock filepath imagemagick lucid mtl
    text
  ];
  description = "Simple CGI image gallery";
  license = stdenv.lib.licenses.bsd2;
}

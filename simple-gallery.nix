{ mkDerivation, base, cgi, JuicyPixels, JuicyPixels-extra, lucid
, stdenv
}:
mkDerivation {
  pname = "simple-gallery";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base cgi JuicyPixels JuicyPixels-extra lucid
  ];
  description = "Simple CGI image gallery";
  license = stdenv.lib.licenses.bsd2;
}

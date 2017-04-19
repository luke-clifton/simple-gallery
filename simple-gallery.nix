{ mkDerivation, async, base, cgi, directory, filelock, filepath
, JuicyPixels, JuicyPixels-extra, lucid, stdenv, text
}:
mkDerivation {
  pname = "simple-gallery";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    async base cgi directory filelock filepath JuicyPixels
    JuicyPixels-extra lucid text
  ];
  description = "Simple CGI image gallery";
  license = stdenv.lib.licenses.bsd2;
}

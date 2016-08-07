{ mkDerivation, aeson, attoparsec, base, bytestring, containers
, directory, fgl, gloss, http-client, http-media, linear, random
, servant, servant-client, stdenv, text, time, transformers
}:
mkDerivation {
  pname = "origami";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson attoparsec base bytestring fgl http-client http-media linear
    random servant servant-client text time
  ];
  executableHaskellDepends = [
    aeson attoparsec base bytestring containers directory gloss
    http-client linear random servant-client text transformers
  ];
  homepage = "https://github.com/githubuser/origami#readme";
  description = "Simple project template from stack";
  license = stdenv.lib.licenses.bsd3;
}

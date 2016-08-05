{ mkDerivation, attoparsec, base, stdenv }:
mkDerivation {
  pname = "origami";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ attoparsec base ];
  homepage = "https://github.com/githubuser/origami#readme";
  description = "Simple project template from stack";
  license = stdenv.lib.licenses.bsd3;
}

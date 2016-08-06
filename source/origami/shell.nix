{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, attoparsec, base, gloss, linear, servant
      , servant-client, stdenv, text
      }:
      mkDerivation {
        pname = "origami";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          attoparsec base linear servant servant-client
        ];
        executableHaskellDepends = [ attoparsec base gloss text ];
        homepage = "https://github.com/githubuser/origami#readme";
        description = "Simple project template from stack";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv

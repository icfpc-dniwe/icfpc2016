{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs stdenv lib;
  haskellLib = pkgs.haskell.lib;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  haskellPackages_ = haskellPackages.override {
    overrides = self: super: {
    };
  };

  drv = haskellPackages_.callPackage ./default.nix {};

  drv_ = haskellLib.overrideCabal drv (drv: {
    libraryPkgconfigDepends = [ pkgs.freeglut ] ++ lib.optional stdenv.isDarwin pkgs.darwin.apple_sdk.frameworks.OpenGL;
  });

in

  if lib.inNixShell then drv_.env else drv_

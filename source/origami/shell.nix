{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;
  inherit (pkgs.haskell) lib;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  haskellPackages_ = haskellPackages.override {
    overrides = self: super: {
    };
  };

  drv = haskellPackages_.callPackage ./default.nix {};

  drv_ = lib.overrideCabal drv (drv: {
    libraryPkgconfigDepends = [ pkgs.freeglut ];
  });

in

  if pkgs.lib.inNixShell then drv_.env else drv_

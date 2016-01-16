{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, async, base, contravariant, stdenv, text
      , time, transformers
      }:
      mkDerivation {
        pname = "logging-effect";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          async base contravariant text time transformers
        ];
        homepage = "https://github.com/ocharles/logging-effect.hs";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv

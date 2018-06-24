{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, async, base, bytestring, criterion
      , exceptions, fast-logger, free, lifted-async, monad-control
      , monad-logger, mtl, prettyprinter, semigroups, stdenv, stm
      , stm-delay, text, time, transformers, transformers-base
      , unliftio-core
      }:
      mkDerivation {
        pname = "logging-effect";
        version = "1.3.0";
        src = ./.;
        libraryHaskellDepends = [
          async base exceptions free monad-control mtl prettyprinter
          semigroups stm stm-delay text time transformers transformers-base
          unliftio-core
        ];
        benchmarkHaskellDepends = [
          base bytestring criterion fast-logger lifted-async monad-logger
          prettyprinter text time
        ];
        homepage = "https://github.com/ocharles/logging-effect";
        description = "A mtl-style monad transformer for general purpose & compositional logging";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv

{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, async, base, bytestring, criterion
      , exceptions, fast-logger, free, lifted-async, monad-control
      , monad-logger, mtl, semigroups, stdenv, stm, stm-delay, text, time
      , transformers, transformers-base, wl-pprint-text
      }:
      mkDerivation {
        pname = "logging-effect";
        version = "1.2.5";
        src = ./.;
        libraryHaskellDepends = [
          async base exceptions free monad-control mtl semigroups stm
          stm-delay text time transformers transformers-base wl-pprint-text
        ];
        benchmarkHaskellDepends = [
          base bytestring criterion fast-logger lifted-async monad-logger
          text time wl-pprint-text
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

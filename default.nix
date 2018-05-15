{ mkDerivation, async, base, bytestring, criterion, exceptions
, fast-logger, free, lifted-async, monad-control, monad-logger, mtl
, prettyprinter, semigroups, stdenv, stm, stm-delay, text, time
, transformers, transformers-base, wl-pprint-text
}:
mkDerivation {
  pname = "logging-effect";
  version = "1.3.0";
  src = ./.;
  libraryHaskellDepends = [
    async base exceptions free monad-control mtl prettyprinter
    semigroups stm stm-delay text time transformers transformers-base
  ];
  benchmarkHaskellDepends = [
    base bytestring criterion fast-logger lifted-async monad-logger
    text time wl-pprint-text
  ];
  homepage = "https://github.com/ocharles/logging-effect";
  description = "A mtl-style monad transformer for general purpose & compositional logging";
  license = stdenv.lib.licenses.bsd3;
}

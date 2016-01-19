{ mkDerivation, async, base, exceptions, free, mtl, stdenv, stm
, stm-delay, text, time, transformers, wl-pprint-text
, criterion, monad-logger, lifted-async
}:
mkDerivation {
  pname = "logging-effect";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    async base exceptions free mtl stm stm-delay text time transformers
    wl-pprint-text criterion monad-logger lifted-async
  ];
  homepage = "https://github.com/ocharles/logging-effect.hs";
  license = stdenv.lib.licenses.bsd3;
}

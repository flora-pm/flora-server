{ mkDerivation, atomic-primops, base, bytestring, clock, containers
, criterion, data-sketches, deepseq, doctest, exceptions, fetchgit
, hspec, lib, mtl, primitive, QuickCheck, random, random-shuffle
, stm, text, transformers, transformers-compat, utf8-string
}:
mkDerivation {
  pname = "prometheus-client";
  version = "1.1.0";
  src = fetchgit {
    url = "https://github.com/fimad/prometheus-haskell";
    sha256 = "1xg3jyhy60xxhcwcl8sc55r7yzya0nqjl8bchms6cvfnzldrcih5";
    rev = "43f19dae23f1e374c6e99eed6840ce185cca66c1";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/prometheus-client; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    atomic-primops base bytestring clock containers data-sketches
    deepseq exceptions mtl primitive stm text transformers
    transformers-compat utf8-string
  ];
  testHaskellDepends = [
    atomic-primops base bytestring clock containers data-sketches
    deepseq doctest exceptions hspec mtl primitive QuickCheck
    random-shuffle stm text transformers transformers-compat
    utf8-string
  ];
  benchmarkHaskellDepends = [
    base bytestring criterion random text utf8-string
  ];
  homepage = "https://github.com/fimad/prometheus-haskell";
  description = "Haskell client library for http://prometheus.io.";
  license = lib.licenses.asl20;
}

{ mkDerivation, base, doctest, fetchgit, lib, prometheus-client
, text, utf8-string
}:
mkDerivation {
  pname = "prometheus-metrics-ghc";
  version = "1.0.1.2";
  src = fetchgit {
    url = "https://github.com/fimad/prometheus-haskell";
    sha256 = "1xg3jyhy60xxhcwcl8sc55r7yzya0nqjl8bchms6cvfnzldrcih5";
    rev = "43f19dae23f1e374c6e99eed6840ce185cca66c1";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/prometheus-metrics-ghc; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base prometheus-client text utf8-string
  ];
  testHaskellDepends = [ base doctest prometheus-client ];
  homepage = "https://github.com/fimad/prometheus-haskell";
  description = "Metrics exposing GHC runtime information for use with prometheus-client";
  license = lib.licenses.asl20;
}

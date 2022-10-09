{ mkDerivation, base, effectful-core, fetchgit, lib, tasty
, tasty-hunit, time
}:
mkDerivation {
  pname = "time-effectful";
  version = "0.0.1.0";
  src = fetchgit {
    url = "https://github.com/haskell-effectful/time-effectful";
    sha256 = "12sir7ln4nfx9w5xz77g23jlfvhnwvv4gzw20czj6vbpak8zz3i1";
    rev = "e212239b685e1ecf7ee95dd1e944cc563351907f";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ base effectful-core time ];
  testHaskellDepends = [
    base effectful-core tasty tasty-hunit time
  ];
  homepage = "https://github.com/haskell-effectful/time-effectful#readme";
  license = lib.licenses.mit;
}

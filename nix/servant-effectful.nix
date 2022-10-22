{ mkDerivation, base, effectful-core, fetchgit, hashable, lib, mtl
, servant, servant-server, tasty, tasty-hunit, wai, warp
}:
mkDerivation {
  pname = "servant-effectful";
  version = "0.0.1.0";
  src = fetchgit {
    url = "https://github.com/kleidukos/servant-effectful";
    sha256 = "1vrp4883jsnq4rgdh89qhka6zs2q96bfxi3m1iaqvc7984g1pl64";
    rev = "65e3041c6cfbc315b20ad22ca18f61dda104eec8";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base effectful-core hashable mtl servant servant-server wai warp
  ];
  testHaskellDepends = [
    base effectful-core hashable servant servant-server tasty
    tasty-hunit
  ];
  homepage = "https://github.com/haskell-effectful/servant-effectful/tree/main/servant-effectful#readme";
  license = lib.licenses.mit;
}

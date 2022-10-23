{ mkDerivation, aeson, base, bytestring, effectful, effectful-core
, fetchgit, lib, log-base, tasty, tasty-hunit, text, time
, time-effectful
}:
mkDerivation {
  pname = "log-effectful";
  version = "1.0.0.0";
  src = fetchgit {
    url = "https://github.com/haskell-effectful/log-effectful";
    sha256 = "WveRv+rouGEBHb5R8yiNgeN1xeXtJMUst/Ea1XydH9k=";
    rev = "v1.0.0.0";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    aeson base bytestring effectful-core log-base text time
    time-effectful
  ];
  testHaskellDepends = [
    aeson base effectful effectful-core log-base tasty tasty-hunit text
    time-effectful
  ];
  homepage = "https://github.com/haskell-effectful/log-effectful#readme";
  license = lib.licenses.bsd3;
}

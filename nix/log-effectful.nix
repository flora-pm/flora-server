{ mkDerivation, aeson, base, bytestring, effectful, effectful-core
, fetchgit, lib, log-base, tasty, tasty-hunit, text, time
, time-effectful
}:
mkDerivation {
  pname = "log-effectful";
  version = "0.0.1.0";
  src = fetchgit {
    url = "https://github.com/haskell-effectful/log-effectful";
    sha256 = "0nwq1i9bm29d6nh5j8sjc7m3rbs3fjf56hwph7yrgc478x645vhi";
    rev = "aaeb7eef5717e9ed26dfbf85016f277134883520";
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

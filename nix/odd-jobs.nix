{ mkDerivation, aeson, base, bytestring, containers, daemons
, directory, either, fast-logger, fetchgit, filepath, foreign-store
, friendly-time, generic-deriving, hedgehog, hostname, hpack, lib
, lifted-async, lifted-base, lucid, mmorph, monad-control
, monad-logger, mtl, optparse-applicative, postgresql-simple
, random, resource-pool, safe, servant, servant-lucid
, servant-server, servant-static-th, string-conv, tasty
, tasty-discover, tasty-hedgehog, tasty-hunit, text
, text-conversions, time, timing-convenience, unix, unliftio
, unliftio-core, unordered-containers, wai, warp
}:
mkDerivation {
  pname = "odd-jobs";
  version = "0.2.2";
  src = fetchgit {
    url = "https://github.com/jappeace/odd-jobs";
    sha256 = "0lxajjhvxi4sv61qhdrjiy4vfp6fc3fk3ysq7b0vqwxkz5sslwk2";
    rev = "a75515791f2c743614ec05d54493ef12b143002e";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring daemons directory either fast-logger filepath
    friendly-time generic-deriving hostname lucid monad-control
    monad-logger mtl optparse-applicative postgresql-simple
    resource-pool safe servant servant-lucid servant-server
    servant-static-th string-conv text text-conversions time
    timing-convenience unix unliftio unliftio-core unordered-containers
    wai warp
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base bytestring daemons directory either fast-logger filepath
    foreign-store friendly-time generic-deriving hostname lucid
    monad-control monad-logger mtl optparse-applicative
    postgresql-simple resource-pool safe servant servant-lucid
    servant-server servant-static-th string-conv text text-conversions
    time timing-convenience unix unliftio unliftio-core
    unordered-containers wai warp
  ];
  testHaskellDepends = [
    aeson base bytestring containers daemons directory either
    fast-logger filepath friendly-time generic-deriving hedgehog
    hostname lifted-async lifted-base lucid mmorph monad-control
    monad-logger mtl optparse-applicative postgresql-simple random
    resource-pool safe servant servant-lucid servant-server
    servant-static-th string-conv tasty tasty-discover tasty-hedgehog
    tasty-hunit text text-conversions time timing-convenience unix
    unliftio unliftio-core unordered-containers wai warp
  ];
  testToolDepends = [ tasty-discover ];
  prePatch = "hpack";
  homepage = "https://www.haskelltutorials.com/odd-jobs";
  description = "A full-featured PostgreSQL-backed job queue (with an admin UI)";
  license = lib.licenses.bsd3;
}

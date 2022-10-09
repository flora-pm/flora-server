{ mkDerivation, aeson, base, bytestring, fetchgit, hspec
, http-conduit, lib, mtl, network, random, resourcet, text, time
, unordered-containers, uuid-types
}:
mkDerivation {
  pname = "raven-haskell";
  version = "0.1.4.1";
  src = fetchgit {
    url = "https://gitlab.com/dpwiz/raven-haskell";
    sha256 = "11bk7qj159glbx252a92lqdwhp2xpl9zfhqx3zhk1zbm6lrskzwk";
    rev = "9dacea2bec9c6f5d9f7d46a2a1d9094cf6147fbf";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/./raven-haskell; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    aeson base bytestring http-conduit mtl network random resourcet
    text time unordered-containers uuid-types
  ];
  testHaskellDepends = [
    aeson base bytestring hspec time unordered-containers
  ];
  homepage = "https://bitbucket.org/dpwiz/raven-haskell";
  description = "Haskell client for Sentry logging service";
  license = lib.licenses.mit;
}

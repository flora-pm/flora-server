{ mkDerivation, array, async, base, base64-bytestring
, blaze-builder, bytestring, case-insensitive, containers, cookie
, deepseq, directory, exceptions, fetchgit, filepath, ghc-prim
, hspec, http-types, iproute, lib, mime-types, monad-control
, network, network-uri, random, stm, streaming-commons, text, time
, transformers, zlib
}:
mkDerivation {
  pname = "http-client";
  version = "0.7.10";
  src = fetchgit {
    url = "https://github.com/snoyberg/http-client";
    sha256 = "050g09q0svlvyf480ars3lh1gwkz7cr0h86x439f5awlj3pwzxln";
    rev = "105be4031597c5a5ab92963c87fc435a3bf1de25";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/http-client; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    array base base64-bytestring blaze-builder bytestring
    case-insensitive containers cookie deepseq exceptions filepath
    ghc-prim http-types iproute mime-types network network-uri random
    stm streaming-commons text time transformers
  ];
  testHaskellDepends = [
    async base blaze-builder bytestring case-insensitive containers
    cookie deepseq directory hspec http-types monad-control network
    network-uri streaming-commons text time transformers zlib
  ];
  doCheck = false;
  homepage = "https://github.com/snoyberg/http-client";
  description = "An HTTP client engine";
  license = lib.licenses.mit;
}

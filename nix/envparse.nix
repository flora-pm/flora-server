{ mkDerivation, base, containers, fetchgit, hspec, lib, text }:
mkDerivation {
  pname = "envparse";
  version = "0.5.0";
  src = fetchgit {
    url = "https://github.com/supki/envparse";
    sha256 = "04zxc38gk7ns9i6ycrl626804v521qxfi100iiayjhjm5hw7hjkx";
    rev = "503a699b7ec4e67e01a9216d7947b366f8025d0b";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ base containers ];
  testHaskellDepends = [ base containers hspec text ];
  homepage = "https://supki.github.io/envparse";
  description = "Parse environment variables";
  license = lib.licenses.bsd3;
}

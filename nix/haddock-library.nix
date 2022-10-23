{ mkDerivation, base, base-compat, containers, deepseq, directory
, fetchgit, filepath, hspec, hspec-discover, lib, optparse-applicative
, parsec, QuickCheck, text, tree-diff
}:
let haddockSrc = fetchgit {
    url = "https://github.com/haskell/haddock";
    sha256 = "ccYSQh8A/RhllAaRkvKc8QwtxVlSe7D1n0wRDUwHpoo=";
    rev = "7f2892b571c7b072c86edbf21b7c7469e21f6303";
  };
in mkDerivation {
  pname = "haddock-library";
  version = "1.11.0";
  src = "${haddockSrc}/haddock-library";
  libraryHaskellDepends = [ base containers parsec text ];
  testHaskellDepends = [
    base base-compat containers deepseq directory filepath hspec
    optparse-applicative parsec QuickCheck text tree-diff
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "http://www.haskell.org/haddock/";
  description = "Library exposing some functionality of Haddock";
  license = lib.licenses.bsd2;
}


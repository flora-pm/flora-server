{ mkDerivation, base, fetchgit, http-types, lib, wai }:
mkDerivation {
  pname = "wai-middleware-heartbeat";
  version = "0.0.1.0";
  src = fetchgit {
    url = "https://github.com/flora-pm/wai-middleware-heartbeat";
    sha256 = "1s2flv2jhfnd4vdfg6rmvq7s852w1pypasdg0l6ih6raaqyqzybn";
    rev = "bd7dbbe83d25c00fcd2cf5c77736af904910c596";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ base http-types wai ];
  description = "Heartbeat middleware for the WAI ecosystem";
  license = lib.licenses.mit;
}

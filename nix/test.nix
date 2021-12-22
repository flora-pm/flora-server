{
  # https://github.com/NixOS/nixpkgs/blob/master/pkgs/top-level/haskell-packages.nix
  pkgs ? import ./pkgs.nix
}:

pkgs.runCommand "run-tests" ({ POSTGRES=pkgs.postgresql; }) ''
set -xe
echo $POSTGRES;

export PATH=$PATH:$POSTGRES/bin
ln -s ${../migrations} ./migrations
${pkgs.haskellPackages.flora-server}/bin/flora-test

''

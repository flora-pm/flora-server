{
  # https://github.com/NixOS/nixpkgs/blob/master/pkgs/top-level/haskell-packages.nix
  pkgs ? import ./pkgs.nix
}:

pkgs.runCommand "run-tests" ({ POSTGRES=pkgs.postgresql; }) ''
set -xe
echo $POSTGRES;

export PATH=$PATH:$POSTGRES/bin
export LC_ALL=C.UTF-8
ln -s ${../migrations} ./migrations
${pkgs.haskellPackages.flora-server}/bin/flora-test

touch $out
''

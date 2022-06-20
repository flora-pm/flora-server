{
# https://github.com/NixOS/nixpkgs/blob/master/pkgs/top-level/haskell-packages.nix
pkgs ? import ./nix/pin.nix { } }:
pkgs.mkShell rec {
  nativeBuildInputs = [
    pkgs.bash
    pkgs.cabal-install
    pkgs.cacert
    pkgs.concurrently
    pkgs.esbuild
    pkgs.ghcid
    pkgs.git
    pkgs.haskellPackages.apply-refact
    pkgs.haskellPackages.cabal-fmt
    (pkgs.haskell.lib.markUnbroken pkgs.haskellPackages.postgresql-migration)
    pkgs.hlint
    pkgs.ncurses6
    pkgs.postgresql_14
    pkgs.tmux
    pkgs.yarn
    pkgs.libffi
    pkgs.zlib
    (pkgs.haskell.lib.dontCheck
      (pkgs.haskell.packages."ghc921".callHackageDirect {
        pkg = "fourmolu";
        ver = "0.7.0.1";
        sha256 = "0wrcmd7v0sfyagiwqxnh117xqikid3hfz2vkxzihywx0ld7jp780";
      } { }))
    (import ./nix/pin2.nix { }).souffle
  ];
  exactDeps = true;
  NIX_PATH = "nixpkgs=${pkgs.path}:.";
  shellHook = ''
    export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath nativeBuildInputs}"
    source environment.sh
    export LOCALE_ARCHIVE="/nix/store/m53mq2077pfxhqf37gdbj7fkkdc1c8hc-glibc-locales-2.27/lib/locale/locale-archive"
    export LC_ALL=C.UTF-8
    cat scripts/shell-welcome.txt
  '';
}

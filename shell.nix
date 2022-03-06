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
    pkgs.hlint
    pkgs.iputils
    pkgs.ncurses6
    pkgs.nixfmt
    pkgs.postgresql_14
    pkgs.stylish-haskell
    pkgs.tmux
    pkgs.yarn
    pkgs.libffi
    pkgs.zlib
    pkgs.souffle
  ];
  exactDeps = true;
  NIX_PATH = "nixpkgs=${pkgs.path}:.";
  shellHook = ''
    export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath nativeBuildInputs}"
    source environment.sh
    export LOCALE_ARCHIVE="/nix/store/m53mq2077pfxhqf37gdbj7fkkdc1c8hc-glibc-locales-2.27/lib/locale/locale-archive"
    export LC_ALL=C.UTF-8
    cat scripts/nix-welcome.txt
  '';
}

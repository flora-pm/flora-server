{
  # https://github.com/NixOS/nixpkgs/blob/master/pkgs/top-level/haskell-packages.nix
  pkgs ? import ./nix/pkgs.nix
}:
pkgs.haskellPackages.shellFor {
  packages = ps : [ ps.flora-server ];
    buildInputs = [
      pkgs.ghcid
      pkgs.cabal-install
      pkgs.hlint
      pkgs.cacert
      pkgs.haskellPackages.apply-refact
      pkgs.stylish-haskell
      pkgs.git
      pkgs.haskellPackages.cabal-fmt
      pkgs.postgresql
    ];
    exactDeps = true;
    NIX_PATH="nixpkgs=${pkgs.path}:.";
    shellHook = ''
      source environment.sh
      export LOCALE_ARCHIVE="/nix/store/m53mq2077pfxhqf37gdbj7fkkdc1c8hc-glibc-locales-2.27/lib/locale/locale-archive"
      export LC_ALL=C.UTF-8
    '';
}

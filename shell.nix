let pkgs = import (builtins.fetchTarball {
      # master on 2021-11-07
      url = "https://github.com/NixOS/nixpkgs/archive/a7517f61ceb8bb2d100950ed3b9071b46141926f.tar.gz";
    }) { };
in with pkgs;
  mkShell rec {
    shellHook = ''
      source environment.sh
      export LOCALE_ARCHIVE="/nix/store/m53mq2077pfxhqf37gdbj7fkkdc1c8hc-glibc-locales-2.27/lib/locale/locale-archive"
      export LC_ALL=C.UTF-8
      export LD_LIBRARY_PATH="${lib.makeLibraryPath buildInputs}";
      echo $LD_LIBRARY_PATH
    '';
    buildInputs = [
      # Haskell Deps
      haskell.compiler.ghc8107
      cabal-install
      ghcid
      hlint
      haskellPackages.apply-refact
      stylish-haskell
      git
      haskellPackages.cabal-fmt
      libffi

      # DB Deps
      postgresql_12
      gmp
      zlib
      glibcLocales
      dbmate
      openldap

      # Extra
      direnv
      yarn
      nodejs
    ];
  }

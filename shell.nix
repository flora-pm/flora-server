let pkgs = import (builtins.fetchTarball {
      # master on 2021-11-07
      url = "https://github.com/NixOS/nixpkgs/archive/2606cb0fc24e65f489b7d9fdcbf219756e45db35.tar.gz";
    }) { };
in with pkgs;
  mkShell {
    shellHook = ''
      source environment.sh
      export LOCALE_ARCHIVE="/nix/store/m53mq2077pfxhqf37gdbj7fkkdc1c8hc-glibc-locales-2.27/lib/locale/locale-archive"
      export LC_ALL=C.UTF-8
      export LIBRARY_PATH="${pkgs.zlib}/lib";
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

      # DB Deps
      postgresql_12
      gmp
      zlib
      glibcLocales
      haskellPackages.postgresql-simple-migration

      # Extra
      direnv
    ];
  }

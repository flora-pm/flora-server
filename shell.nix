let pkgs = import (builtins.fetchTarball {
      # master on 2021-11-07
      url = "https://github.com/NixOS/nixpkgs/archive/2c2a09678ce2ce4125591ac4fe2f7dfaec7a609c.tar.gz";
    }) { };
in with pkgs;
  mkShell rec {
    shellHook = ''
      source environment.sh
      export LOCALE_ARCHIVE="/nix/store/m53mq2077pfxhqf37gdbj7fkkdc1c8hc-glibc-locales-2.27/lib/locale/locale-archive"
      export LC_ALL=C.UTF-8
      export LD_LIBRARY_PATH="${lib.makeLibraryPath buildInputs}";
    '';
    buildInputs = [
      # Haskell Deps
      haskell.compiler.ghc8107
      cabal-install
      ghcid
      hlint
      cacert
      haskellPackages.apply-refact
      stylish-haskell
      git
      haskellPackages.cabal-fmt

      # DB Deps
      postgresql_14
      gmp
      zlib
      glibcLocales
      haskellPackages.postgresql-simple-migration

      # Extra
      direnv
      yarn
      nodejs
    ];
  }

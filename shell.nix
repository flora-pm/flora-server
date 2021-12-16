let pkgs = import (builtins.fetchTarball {
      # master on 2021-11-07
      url = "https://github.com/NixOS/nixpkgs/archive/2c2a09678ce2ce4125591ac4fe2f7dfaec7a609c.tar.gz";
    }) { overlays = [(
      self: super: rec {
        # - Don't forget to update the sha256 accordingly.
        all-cabal-hashes = super.fetchurl {
          url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/d464d4b.tar.gz";
          sha256 = "1f0j5z60c63j4biwh0n92brxrfwlhxzdlnvfvnrsq1p6iyfijyma";
        };
      })];
    };
in with pkgs; {
  oldShell = 
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
        haskellPackages.postgresql-libpq
        stylish-haskell
        git
        haskellPackages.cabal-fmt
        ncurses6

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
    };

    newShell = (haskellPackages.override {
      overrides = self: super: with haskell.lib; rec {
        PyF = super.callHackage "PyF" "0.10.2.0" {};
        lucid = super.callHackage "lucid" "2.11.0" {};
        optics-core = super.callHackage "optics-core" "0.4" {};
        pcre2 = super.callHackage "pcre2" "2.0.3" {};
        Cabal = super.callHackage "Cabal" "3.6.2.0" {};
        pg-transact = dontCheck (unmarkBroken super.pg-transact);
        hspec-pg-transact = dontCheck (super.hspec-pg-transact);
        postgresql-migration = unmarkBroken super.postgresql-migration;
        text-display = unmarkBroken super.text-display;
        wai-middleware-heartbeat = super.callCabal2nix "wai-middleware-heartbeat" (fetchTarball {
          url = "https://github.com/flora-pm/wai-middleware-heartbeat/archive/bd7dbbe.tar.gz";
          sha256 = "1s2flv2jhfnd4vdfg6rmvq7s852w1pypasdg0l6ih6raaqyqzybn";
        }) {};
        envparse = super.callCabal2nix "envparse" (fetchTarball { 
          url = "https://github.com/supki/envparse/archive/de5944f.tar.gz";
          sha256 = "0piljyzplj3bjylnxqfl4zpc3vc88i9fjhsj06bk7xj48dv3jg3b";
        }) {};
        pg-entity = dontCheck (super.callCabal2nix "pg-entity" (fetchTarball {
          url = "https://github.com/tchoutri/pg-entity/archive/e5fc4cf.tar.gz";
          sha256 = "06fbjim83mbpv9ixacq40ir3cfzdy4dbkqx5pawc8z0n8ncwb9zq";
        }) {});
        sourcePrometheus = fetchTarball {
          url = "https://github.com/fimad/prometheus-haskell/archive/43f19da.tar.gz";
          sha256 = "1xg3jyhy60xxhcwcl8sc55r7yzya0nqjl8bchms6cvfnzldrcih5";
        };
        prometheus-metrics-ghc = super.callCabal2nix "prometheus-metrics-ghc" ("${sourcePrometheus}/prometheus-metrics-ghc") {};
        prometheus-client = super.callCabal2nix "prometheus-client" ("${sourcePrometheus}/prometheus-client") {};
        prometheus-wai-middleware-prometheus = super.callCabal2nix "prometheus-wai-middleware-prometheus" ("${sourcePrometheus}/prometheus-wai-middleware-prometheus") {};
      };
    }).callCabal2nix "flora" ./. {

    };
}

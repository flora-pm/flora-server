{ inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    utils.url = github:numtide/flake-utils;
  };

  outputs = { nixpkgs, utils, ... }:
    utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        compiler = "ghc92";

        config = { allowBroken = true; allowUnsupportedSystem = true; };

        overlay = pkgsNew: pkgsOld: {
          flora =
            pkgsNew.haskell.lib.justStaticExecutables
              (pkgsNew.overrideCabal
                pkgsNew.haskellPackages.flora
                (old: {
                  nativeBuildInputs = (old.nativeBuildInputs or []) ++ [
                    pkgsNew.makeWrapper
                  ];

                  postInstall = (old.postInstall or "") ++
                    ''
                    wrapProgram $out/bin/flora-cli --prefix PATH : ${pkgsNew.souffle}/bin
                    '';
                })
              );

          haskell =
            pkgsOld.haskell // {
              packages = pkgsOld.haskell.packages // {
                "${compiler}" =
                  pkgsOld.haskell.packages."${compiler}".override (old: {
                    overrides =
                      pkgsNew.lib.fold
                        pkgsNew.lib.composeExtensions
                        (old.overrides or (_: _: { }))
                        [ (pkgsNew.haskell.lib.packageSourceOverrides {
                            flora = ./.;

                            text-display = "0.0.2.0";
                          })
                          (pkgsNew.haskell.lib.packagesFromDirectory {
                            directory = ./nix;
                          })
                          (haskellPackagesNew: haskellPackagesOld: {
                            Cabal-syntax = haskellPackagesNew.Cabal_3_8_1_0;

                            lens-aeson = haskellPackagesNew.lens-aeson_1_2_2;

                            log-effectful =
                              pkgsNew.haskell.lib.doJailbreak
                                haskellPackagesOld.log-effectful;

                            monad-time = haskellPackagesNew.monad-time_0_4_0_0;

                            odd-jobs =
                              pkgsNew.haskell.lib.overrideCabal
                                haskellPackagesOld.odd-jobs
                                (old: {
                                  doCheck = false;

                                  prePatch = "";

                                  libraryToolDepends = [];
                                });

                            PyF = haskellPackagesNew.PyF_0_11_1_0;

                            pcre2 =
                              pkgsNew.haskell.lib.dontCheck
                                haskellPackagesOld.pcre2;

                            pg-entity =
                              pkgsNew.haskell.lib.doJailbreak
                                (pkgsNew.haskell.lib.dontCheck
                                  haskellPackagesOld.pg-entity
                                );

                            postgresql-simple-migration =
                              pkgsNew.haskell.lib.doJailbreak
                                haskellPackagesOld.postgresql-simple-migration;

                            raven-haskell =
                              pkgsNew.haskell.lib.dontCheck
                                haskellPackagesOld.raven-haskell;

                            resource-pool =
                              haskellPackagesNew.resource-pool_0_3_1_0;

                            servant-static-th =
                              pkgsNew.haskell.lib.dontCheck
                                haskellPackagesOld.servant-static-th;

                            slugify =
                              pkgsNew.haskell.lib.dontCheck
                                haskellPackagesOld.slugify;

                            souffle-haskell =
                              pkgsNew.haskell.lib.dontCheck
                                haskellPackagesOld.souffle-haskell;

                            pg-transact =
                              pkgsNew.haskell.lib.dontCheck
                                haskellPackagesOld.pg-transact;

                            pg-transact-effectful =
                              pkgsNew.haskell.lib.doJailbreak
                                haskellPackagesOld.pg-transact-effectful;

                            prometheus-proc =
                              pkgsNew.haskell.lib.doJailbreak
                                haskellPackagesOld.prometheus-proc;

                            text-metrics =
                              pkgsNew.haskell.lib.doJailbreak
                                haskellPackagesOld.text-metrics;

                            type-errors-pretty =
                              pkgsNew.haskell.lib.doJailbreak
                                (pkgsNew.haskell.lib.dontCheck
                                  haskellPackagesOld.type-errors-pretty
                                );

                            wai-middleware-heartbeat =
                              pkgsNew.haskell.lib.doJailbreak
                                haskellPackagesOld.wai-middleware-heartbeat;

                            vector =
                              pkgsNew.haskell.lib.overrideCabal
                                haskellPackagesNew.vector_0_13_0_0
                                (old: {
                                  testHaskellDepends = (old.testHaskellDepends or []) ++ [
                                    haskellPackagesNew.doctest
                                  ];
                                });

                            vector-algorithms =
                              haskellPackagesNew.vector-algorithms_0_9_0_1;
                          })
                        ];
                  });
              };
            };
        };

        pkgs =
          import nixpkgs { inherit config system; overlays = [ overlay ]; };

      in
        rec {
          packages.default = pkgs.haskell.packages."${compiler}".flora;

          apps = rec {
            default = server;

            server = {
              type = "app";

              program = "${pkgs.flora}/bin/flora-server";
            };

            cli = {
              type = "app";

              program = "${pkgs.flora}/bin/flora-cli";
            };
          };

          devShells.default = pkgs.haskell.packages."${compiler}".flora.env;
        }
    );
}


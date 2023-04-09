{
  description = "flora";
  nixConfig = {
    extra-substituters = "https://horizon.cachix.org";
    extra-trusted-public-keys = "horizon.cachix.org-1:MeEEDRhRZTgv/FFGCv3479/dmJDfJ82G6kfUDxMSAw0=";
  };

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    horizon-platform.url =
      "git+https://gitlab.horizon-haskell.net/package-sets/horizon-platform";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    # non-nix dependencies
    poolboy.url = "github:blackheaven/poolboy/v0.2.1.0";
    poolboy.flake = false;
  };
  outputs = inputs@{ flake-utils, horizon-platform, nixpkgs, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hsPkgs = horizon-platform.legacyPackages.${system}.override {
          overrides = import ./nix/hspkgs.nix { inherit pkgs inputs; src = ./.; };
        };
      in
      {
        apps = rec {
          default = server;
          server = flake-utils.lib.mkApp {
            drv = hsPkgs.flora;
            name = "flora-server";
          };
          cli = flake-utils.lib.mkApp {
            drv = hsPkgs.flora;
            name = "flora-cli";
          };
        };
        formatter = pkgs.nixpkgs-fmt;
        devShells.default = hsPkgs.shellFor {
          packages = p: [ p.flora ];
          nativeBuildInputs = [
            hsPkgs.apply-refact
            hsPkgs.fourmolu
            hsPkgs.haskell-language-server
            hsPkgs.postgresql-migration
            pkgs.cabal-install
            pkgs.ghcid
            pkgs.hlint
            pkgs.postgresql_14
            pkgs.souffle
          ];

          shellHook = ''
            source ${./environment.sh}
            cat ${./scripts/shell-welcome.txt}
          '';
        };
        packages.default = hsPkgs.flora;
      });
}

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
    streamly.url = "github:composewell/streamly/81bfdec";
    streamly.flake = false;
    poolboy.url = "github:blackheaven/poolboy/v0.2.1.0";
    poolboy.flake = false;
  };
  outputs = inputs@{ flake-utils, horizon-platform, nixpkgs, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hsPkgs = with pkgs.haskell.lib;
          horizon-platform.legacyPackages.${system}.override {
            overrides = hfinal: hprev: {
              flora = dontCheck (overrideCabal (hfinal.callCabal2nix "flora" ./. { }) (drv: {
                preConfigure = ''
                  ${pkgs.lib.getExe pkgs.souffle} -g cbits/categorise.{cpp,dl}
                '';
              }));
              streamly-core =
                hfinal.callCabal2nix "streamly-core" "${inputs.streamly}/core/" { };
              streamly = hfinal.callCabal2nix "streamly" inputs.streamly { };
              poolboy = dontCheck (hfinal.callCabal2nix "poolboy" inputs.poolboy { });
              resource-pool = hfinal.callHackage "resource-pool" "0.3.1.0" { };
            };
          };
      in
      {
        apps = rec {
          default = server;
          server = flake-utils.lib.mkApp { drv = "${hsPkgs.flora}/bin/flora-server"; };
          cli = flake-utils.lib.mkApp { drv = "${hsPkgs.flora}/bin/flora-cli"; };
        };
        formatter = pkgs.nixpkgs-fmt;
        devShells.default = hsPkgs.shellFor {
          packages = p: [ p.flora ];
          nativeBuildInputs = [
            hsPkgs.apply-refact
            hsPkgs.fourmolu
            hsPkgs.postgresql-migration
            pkgs.cabal-install
            pkgs.ghcid
            pkgs.hlint
            pkgs.postgresql_14
            pkgs.souffle
          ];

          shellHook = ''
            source ./environment.sh
            cat ./scripts/shell-welcome.txt
          '';
        };
        packages.default = hsPkgs.flora;
      });
}

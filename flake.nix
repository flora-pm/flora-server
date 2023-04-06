{
  description = "flora";
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    horizon-platform.url =
      "git+https://gitlab.horizon-haskell.net/package-sets/horizon-platform";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    # nixos-22.05 provides souffle 2.2; that version does not easily
    # build on the current pin, so use a separate input for this
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-22.05";
    streamly = {
      url = "github:composewell/streamly";
      flake = false;
    };
    poolboy.url = "github:blackheaven/poolboy";
  };
  outputs = inputs@{ self, flake-utils, horizon-platform, nixpkgs, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = import nixpkgs { inherit system; };
        souffle_2_2 = inputs.nixpkgs-stable.legacyPackages.${system}.souffle;
        hsPkgs = with pkgs.haskell.lib;
          horizon-platform.legacyPackages.${system}.override {
            overrides = hfinal: hprev: {
              flora = overrideCabal (dontHaddock (dontCheck
                (doJailbreak (hfinal.callCabal2nix "flora" ./. { })))) (drv: {
                  preConfigure = ''
                    cd cbits
                    ${pkgs.lib.getExe souffle_2_2} -g categorise.{cpp,dl}
                    cd ..
                  '';
                });
              streamly-core =
                hfinal.callCabal2nix "streamly-core" "${inputs.streamly}/core/"
                { };
              streamly = hfinal.callCabal2nix "streamly" inputs.streamly { };
              poolboy = hfinal.callCabal2nix "poolboy" inputs.poolboy { };
            };
          };
      in {
        apps = rec {
          default = server;

          server = {
            type = "app";
            program = "${hsPkgs.flora}/bin/flora-server";
          };

          cli = {
            type = "app";
            program = "${hsPkgs.flora}/bin/flora-cli";
          };
        };
        devShells.default = hsPkgs.shellFor {
          packages = p: [ p.flora ];
          nativeBuildInputs = with pkgs; [
            cabal-install
            ghcid
            hlint
            hsPkgs.apply-refact
            hsPkgs.fourmolu
            hsPkgs.postgresql-migration
            postgresql_14
            souffle_2_2
          ];
          shellHook = ''
            source ./environment.sh
            cat ./scripts/shell-welcome.txt
          '';
        };
        packages.default = hsPkgs.flora;
      });
}

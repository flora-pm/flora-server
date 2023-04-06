{
  description = "flora";
  inputs = {
    flake-utils = { url = "github:numtide/flake-utils"; };
    horizon-platform = {
      url =
        "git+https://gitlab.horizon-haskell.net/package-sets/horizon-platform";
    };
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    streamly.url = "git+https://github.com/composewell/streamly";
    streamly.flake = false;
    poolboy.url = "git+https://github.com/blackheaven/poolboy";
  };
  outputs = inputs@{ self, flake-utils, horizon-platform, nixpkgs, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = import nixpkgs { inherit system; };
        hsPkgs = with pkgs.haskell.lib;
          horizon-platform.legacyPackages.${system}.override {
            overrides = hfinal: hprev: rec {
              flora = overrideCabal (dontHaddock (dontCheck
                (doJailbreak (hfinal.callCabal2nix "flora" ./. { })))) (drv: {
                  preConfigure = ''
                    cd cbits; ${pkgs.souffle}/bin/souffle -g categorise.{cpp,dl}
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
        devShells.default = hsPkgs.flora.env;
        packages.default = hsPkgs.flora;
      });
}

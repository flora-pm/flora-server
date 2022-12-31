{
  description = "flora";
  inputs = {
    flake-utils = { url = "github:numtide/flake-utils"; };
    horizon-platform = { url = "git+https://gitlab.homotopic.tech/horizon/horizon-platform"; };
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };
  outputs = inputs@{ self, flake-utils, horizon-platform, nixpkgs, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = import nixpkgs { inherit system; };
        hsPkgs = with pkgs.haskell.lib;
          horizon-platform.legacyPackages.${system}.override {
            overrides = hfinal: hprev: {
                flora = overrideCabal (dontHaddock (dontCheck
                    (doJailbreak (hfinal.callCabal2nix "flora" ./. { }))))
                  (drv: {
                    preConfigure = ''
                      cd cbits; ${pkgs.souffle}/bin/souffle -g categorise.{cpp,dl}
                      cd ..
                    '';
                  });
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

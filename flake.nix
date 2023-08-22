{
  description = "flora";
  nixConfig = {
    extra-substituters = [
      "https://horizon.cachix.org"
      "https://flora-pm.cachix.org"
    ];
    extra-trusted-public-keys = [
      "horizon.cachix.org-1:MeEEDRhRZTgv/FFGCv3479/dmJDfJ82G6kfUDxMSAw0="
      "flora-pm.cachix.org-1:/6CcPGZqC3kzHk9MKE/soIEKP1BO24L7Y2vx7p1orLM="
    ];
    allow-import-from-derivation = true;
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    # we need souffle 2.3, only change this if you're sure, that flora should compile with
    # souffle > 2.3
    nixpkgs-souffle.url = "github:nixos/nixpkgs/a74a4a2f324fb54637a9e2597ef1fdca6ad869c8";
    flake-utils.url = "github:numtide/flake-utils";
    horizon-platform.url =
      "git+https://gitlab.horizon-haskell.net/package-sets/horizon-platform";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    pre-commit-hooks.inputs.flake-utils.follows = "flake-utils";
    pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";

    # non-nix dependencies
    poolboy.url = "github:blackheaven/poolboy/v0.2.1.0";
    poolboy.flake = false;
  };
  outputs = inputs@{ self, flake-utils, horizon-platform, nixpkgs, pre-commit-hooks, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system}.appendOverlays [
          (_self: _super: { inherit (inputs.nixpkgs-souffle.legacyPackages.${system}) souffle; })
        ];
        src = ./.;
        pre-commit-check = pre-commit-hooks.lib.${system}.run
          (import ./nix/pre-commit-config.nix { inherit src; });
        hsPkgs = horizon-platform.legacyPackages.${system}.extend (
          import ./nix/hspkgs.nix { inherit src pkgs inputs; }
        );
        floraShell = import ./nix/shell-config.nix { inherit src pkgs hsPkgs pre-commit-check; };
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
        devShells = rec {
          onlyPreCommit = pkgs.mkShell {
            name = "pre-commit-shell";
            inherit (pre-commit-check) shellHook;
          };
          flora = floraShell;
          default = flora;
        };
        packages = rec {
          inherit (hsPkgs) flora;
          default = flora;
        };
        checks = {
          flora-tests = self.packages.${system}.flora;
          flora-shell = self.devShells.${system}.flora;
          flora-style = pre-commit-check;
        };
      });
}

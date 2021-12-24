let pkgs = import ./pkgs.nix;
in {
  default = pkgs.haskellPackages.flora-server;
  shell = (import ../shell.nix { inherit pkgs; });
  test = import ./test.nix { };
}

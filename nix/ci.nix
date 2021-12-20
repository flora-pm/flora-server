{
  default = import ../default.nix {};
  # https://github.com/NixOS/nixpkgs/blob/master/pkgs/top-level/haskell-packages.nix#L47
  shell = (import ../shell.nix {});


}

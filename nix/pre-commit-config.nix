{ src }: {
  inherit src;
  hooks = {
    # nix hooks
    nixpkgs-fmt.enable = true;
    deadnix.enable = true;
    statix.enable = true;

    # haskell hooks
    cabal-fmt = {
      enable = true;
      excludes = [ "^test/fixtures" ];
    };
    fourmolu.enable = true;
    hlint.enable = true;
  };
}


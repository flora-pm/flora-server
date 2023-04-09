{ src, pkgs, hsPkgs, pre-commit-check }: hsPkgs.shellFor {
  packages = p: [ p.flora ];
  nativeBuildInputs = [
    hsPkgs.apply-refact
    hsPkgs.fourmolu
    hsPkgs.haskell-language-server
    hsPkgs.hlint
    hsPkgs.postgresql-migration
    pkgs.cabal-install
    pkgs.ghcid
    pkgs.postgresql_14
    pkgs.souffle
  ];

  shellHook = ''
    ${pre-commit-check.shellHook}
    source ${src}/environment.sh
    cat ${src}/scripts/shell-welcome.txt
  '';
}


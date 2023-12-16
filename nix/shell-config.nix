{ src, pkgs, hsPkgs, hsDev, pre-commit-check }: hsPkgs.shellFor {
  packages = p: [ p.flora ];
  nativeBuildInputs = [
    # hsDev.haskell-language-server
    hsPkgs.cabal-install
    hsPkgs.postgresql-migration
    pkgs.ghcid
    pkgs.postgresql_14
    pkgs.souffle
    pkgs.yarn
    pkgs.esbuild
  ];

  shellHook = ''
    ${pre-commit-check.shellHook}
    rm -f ./cbits/categorise.cpp
    ${pkgs.lib.getExe pkgs.souffle} -g ./cbits/categorise.{cpp,dl}
    source ${src}/environment.sh
    cat ${src}/scripts/shell-welcome.txt
  '';
}


{ pkgs, src, inputs }: (hfinal: _hprev: with pkgs.haskell.lib; {
  flora = overrideCabal (hfinal.callCabal2nix "flora" src { }) (drv: {
    doCheck = true;

    testSystemDepends = drv.testSystemDepends or [ ] ++ [
      pkgs.souffle
      pkgs.which
    ];

    testToolDepends = drv.testToolDepends or [ ] ++ [
      pkgs.postgresql_14
      pkgs.postgresqlTestHook
    ];

    checkPhase = ''
      runHook preCheck
      source ${src}/environment.sh
      source ${src}/environment.ci.sh
      createdb \
        --echo \
        -p $FLORA_DB_PORT \
        -U $FLORA_DB_USER \
        -w $FLORA_DB_DATABASE
      # we need a different connstring due to not being
      # able ot use TCP/IP with nix, see
      # https://nixos.org/manual/nixpkgs/unstable/#sec-postgresqlTestHook
      export FLORA_DB_CONNSTRING="\
        dbname=$FLORA_DB_DATABASE \
        user=$FLORA_DB_USER \
        password=$FLORA_DB_PASSWORD"
      migrate init "$FLORA_DB_CONNSTRING"
      migrate migrate "$FLORA_DB_CONNSTRING" migrations
      dist/build/flora-cli/flora-cli -- create-user \
        --username "hackage-user" \
        --email "tech@flora.pm" \
        --password "foobar2000"
      dist/build/flora-test/flora-test
      ${drv.checkPhase or ""}
      runHook postCheck
      set +x
    '';

    preConfigure = ''
      ${pkgs.lib.getExe pkgs.souffle} -g cbits/categorise.{cpp,dl}
    '';
  });

  poolboy = dontCheck (hfinal.callCabal2nix "poolboy" inputs.poolboy { });
  tracing = hfinal.callCabal2nix "tracing" inputs.tracing { };
  tracing-effectful = hfinal.callCabal2nix "tracing-effectful" "${inputs.tracing}/tracing-effectful" { };
  dani-servant-lucid2 = hfinal.callHackage "dani-servant-lucid2" "0.1.0.0" {};
  lucid2 = doJailbreak (hfinal.callHackage "lucid2" "0.0.20240424" {});
  htmx = hfinal.callHackageDirect { pkg = "htmx"; ver = "0.1.0.2"; sha256 = "sha256-eCdYwjwtduzct5JFV7AifZdzYZZGPIMs6k/GBPvSivw="; } {};
  htmx-lucid = hfinal.callHackageDirect {pkg = "htmx-lucid"; ver = "0.2.0.1"; sha256 = "sha256-an5Ju+PrOuSZT0dQuqV+LrsjR+AA5I3RyPQ4U5jel5Y=";} {};
  resource-pool = hfinal.callHackage "resource-pool" "0.4.0.0" { };
})

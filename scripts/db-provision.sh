#!/usr/bin/env bash

set -euo pipefail

if [ $# -lt 1 ]; then
  echo "$0 should be called with at least one argument, the path to the config file"
  exit 1
fi

CONFIG_PATH="$1"

# Defaults to `cabal run` for make/dev. Docker sets FLORA_CLI=flora-cli.
# TODO: replace all this with flora-cli command
FLORA_CLI="${FLORA_CLI:-cabal run -- flora-cli}"

echo "Starting db provisioning with $CONFIG_PATH..."

$FLORA_CLI -c "$CONFIG_PATH" create-user --username "hackage-user" --email "tech@flora.pm" --password "foobar2000"
$FLORA_CLI -c "$CONFIG_PATH" provision categories
$FLORA_CLI -c "$CONFIG_PATH" provision-repository --name "hackage" \
    --url https://hackage.haskell.org \
    --description "Central package repository"
$FLORA_CLI -c "$CONFIG_PATH" provision-repository --name "cardano" \
    --url https://chap.intersectmbo.org \
    --description "Packages of the Cardano project"
$FLORA_CLI -c "$CONFIG_PATH" provision-repository --name "horizon" \
    --url https://packages.horizon-haskell.net \
    --description "Packages of the Horizon project"
$FLORA_CLI -c "$CONFIG_PATH" provision-repository --name "mlabs" \
    --url https://plutonomicon.github.io/plutarch-plutus \
    --description "Packages of the MLabs Cardano ecosystem"
$FLORA_CLI -c "$CONFIG_PATH" index-dependency --name "cardano"\
    --depends-on "hackage" \
    --priority 1
$FLORA_CLI -c "$CONFIG_PATH" index-dependency --name "horizon"\
    --depends-on "hackage" \
    --priority 1
$FLORA_CLI -c "$CONFIG_PATH" index-dependency --name "mlabs" \
    --depends-on "cardano" \
    --priority 1
$FLORA_CLI -c "$CONFIG_PATH" index-dependency --name "mlabs" \
    --depends-on "hackage" \
    --priority 2

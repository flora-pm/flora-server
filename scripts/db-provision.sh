#!/usr/bin/env bash

set -euo pipefail

if [ $# -lt 1 ]; then
  echo "$0 should be called with at least one argument, the path to the config file"
  exit 1
fi

CONFIG_PATH="$1"

echo "Starting db provisioning with $CONFIG_PATH..."

cabal run -- flora-cli -c "$CONFIG_PATH" create-user --username "hackage-user" --email "tech@flora.pm" --password "foobar2000"
cabal run -- flora-cli -c "$CONFIG_PATH" provision categories
cabal run -- flora-cli -c "$CONFIG_PATH" provision-repository --name "hackage" \
    --url https://hackage.haskell.org \
    --description "Central package repository"
cabal run -- flora-cli -c "$CONFIG_PATH" provision-repository --name "cardano" \
    --url https://chap.intersectmbo.org \
    --description "Packages of the Cardano project"
cabal run -- flora-cli -c "$CONFIG_PATH" provision-repository --name "horizon" \
    --url https://packages.horizon-haskell.net \
    --description "Packages of the Horizon project"
cabal run -- flora-cli -c "$CONFIG_PATH" provision-repository --name "mlabs" \
    --url https://plutonomicon.github.io/plutarch-plutus \
    --description "Packages of the MLabs Cardano ecosystem"
cabal run -- flora-cli -c "$CONFIG_PATH" index-dependency --name "cardano"\
    --depends-on "hackage" \
    --priority 1
cabal run -- flora-cli -c "$CONFIG_PATH" index-dependency --name "horizon"\
    --depends-on "hackage" \
    --priority 1
cabal run -- flora-cli -c "$CONFIG_PATH" index-dependency --name "mlabs" \
    --depends-on "cardano" \
    --priority 1
cabal run -- flora-cli -c "$CONFIG_PATH" index-dependency --name "mlabs" \
    --depends-on "hackage" \
    --priority 2

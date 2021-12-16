#!/usr/bin/env bash

set -eaxo pipefail

trap 'kill 0' EXIT

source ./environment.test.sh

if [ -z "$1" ] ;
then
  cabal run -- flora-test
else
  ghcid --command='cabal v2-repl flora-test' --test 'Main.main'
fi

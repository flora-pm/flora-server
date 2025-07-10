#!/usr/bin/env bash

set -eao pipefail

[ -f ./environment.test.local.sh ] && source ./environment.test.local.sh || source ./environment.test.sh

if [ -z "$1" ] ;
then
  cabal test
else
  ghcid --command='cabal v2-repl flora-test' --test 'Main.main'
fi

#!/usr/bin/env bash

set -eao pipefail

[ -f ./environment.prod.sh ] && source ./environment.prod.sh || source ./environment.local.sh

gmake build-assets

gmake db-migrate

cabal run --no-semaphore exe:flora-server

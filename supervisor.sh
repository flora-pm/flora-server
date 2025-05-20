#!/usr/bin/env bash

set -eao pipefail

[ -f ./environment.prod.sh ] && source ./environment.prod.sh || source ./environment.local.sh

gmake db-migrate

gmake start

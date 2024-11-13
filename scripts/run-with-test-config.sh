#!/usr/bin/env bash

set -eao pipefail

[ -f ./environment.test.local.sh ] && source ./environment.test.local.sh || source ./environment.test.sh

export DATALOG_DIR="cbits/"

if [ -z "$@" ] ;
then
  print "Call this script with an argument!"
  exit 1
else
  make "$@"
fi


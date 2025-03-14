#!/usr/bin/env bash

set -e

export PATH="${HOME}/.ghcup/bin:${PATH}"

mise trust
mise install
mise run install-haskell-toolchain
cabal install postgresql-migration-0.2.1.8
mise run build-js
make build-release

#!/usr/bin/env bash

set -e

export PATH="${HOME}/.ghcup/bin:${PATH}"

mise trust
mise install
mise run install-haskell-toolchain
mise run build-js
make build-release

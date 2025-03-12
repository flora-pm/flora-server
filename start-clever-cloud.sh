#!/usr/bin/env bash

set -e

export PATH="${HOME}/.ghcup/bin:${PATH}"

cabal install postgresql-migration-0.2.1.8
make db-init
make db-migrate
make db-provision
make start-release

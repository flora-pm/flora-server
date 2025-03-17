#!/usr/bin/env bash

set -eu

export PATH="${HOME}/.ghcup/bin:${PATH}"
export FLORA_DB_CONNSTRING="host=${FLORA_DB_HOST} dbname=${FLORA_DB_DATABASE} \
  user=${FLORA_DB_USER} password=${FLORA_DB_PASSWORD} port=${FLORA_DB_PORT} \
  sslmode=${FLORA_DB_SSLMODE:-allow}"

mise trust
mise install
mise run install-haskell-toolchain
cabal install postgresql-migration-0.2.1.8
mise run build-js
make build-release
cabal run --project-file cabal.project.release flora-cli -- create-user --username "admin" --email ${FLORA_DEV_ADMIN_EMAIL} --password ${FLORA_DEV_ADMIN_PASSWORD}

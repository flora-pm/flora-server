#!/usr/bin/env bash

set -e

export PATH="${HOME}/.ghcup/bin:${PATH}"
export FLORA_DB_CONNSTRING="host=${FLORA_DB_HOST} dbname=${FLORA_DB_DATABASE} port=${FLORA_DB_PORT} \
  user=${FLORA_DB_USER} password=${FLORA_DB_PASSWORD} \
  sslmode=${FLORA_DB_SSLMODE:-allow}"

make db-init
make db-migrate
make db-provision
make start-release

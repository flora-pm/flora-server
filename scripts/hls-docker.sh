#!/bin/sh
set -eu

cd "$(dirname "$0")/.."

exec docker compose exec -T -w "$PWD" devel haskell-language-server-wrapper "$@"

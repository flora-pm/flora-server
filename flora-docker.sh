#!/usr/bin/env bash

set -euo pipefail

# Simple bash script to do the docker things;
# After building do: migrate, provision, bootstrap
#
# Once those are done, run both jobs and server in order.
#
IMAGE="${IMAGE:-ghcr.io/flora-pm/flora-server:v1.0.30}"
NETWORK="${NETWORK:-flora-server_default}"
PLATFORM="${PLATFORM:-linux/amd64}"
CONFIG_HOST="${CONFIG_HOST:-$(pwd)/environment.prod.kdl}"
CONFIG_IN="/flora.kdl"

run() {
  if [ ! -f "$CONFIG_HOST" ]; then
    echo "halt: config file not found: $CONFIG_HOST" >&2
    exit 1
  fi
  docker run --rm --platform "$PLATFORM" --network "$NETWORK" \
    -e LANG=C.UTF-8 -e LC_ALL=C.UTF-8 \
    -v "$CONFIG_HOST:$CONFIG_IN:ro" "$@"
}

cmd="${1:-}"
case "$cmd" in
  build)
    docker build --platform "$PLATFORM" -f Dockerfile.staged -t "$IMAGE" .
    ;;
  migrate)
    run --entrypoint flora-migrate "$IMAGE" -c "$CONFIG_IN"
    ;;
  provision)
    run -e FLORA_CLI=flora-cli --entrypoint /scripts/db-provision.sh "$IMAGE" "$CONFIG_IN"
    ;;
  bootstrap)
    run --entrypoint flora-migrate "$IMAGE" -c "$CONFIG_IN"
    run -e FLORA_CLI=flora-cli --entrypoint /scripts/db-provision.sh "$IMAGE" "$CONFIG_IN"
    ;;
  server)
    run -p 8084:8084 --entrypoint flora-server "$IMAGE" -c "$CONFIG_IN"
    ;;
  jobs)
    run -p 8085:8085 --entrypoint flora-jobs-runner "$IMAGE" -c "$CONFIG_IN"
    ;;
  *)
    echo "usage: $0 {build|migrate|provision|bootstrap|server|jobs}" >&2
    exit 1
    ;;
esac

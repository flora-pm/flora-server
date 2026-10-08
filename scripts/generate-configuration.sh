#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
TEMPLATES_DIR="$ROOT_DIR/config/templates"
FILES=(flora.kdl flora_test.kdl jobs_runner.kdl jobs_runner_test.kdl)

usage() {
  cat <<USAGE
Usage: $(basename "$0") (-d | -l | -c) [-f]

Select a configuration setup and copy its templates to the repository root.

Setups (exactly one is required):
  -d, --docker   Executables run inside the docker-compose "devel" container
  -l, --local    Executables run on the host machine
  -c, --ci       Continuous integration

Options:
  -f, --force    Overwrite configuration files that already exist
  -h, --help     Show this help and exit

Generated files: ${FILES[*]}
USAGE
}

GETOPT_STATUS=0
getopt --test > /dev/null || GETOPT_STATUS=$?
if [ "$GETOPT_STATUS" -ne 4 ]; then
  echo "$(basename "$0"): enhanced getopt (util-linux) is required" >&2
  exit 1
fi

PARSED=$(getopt --options dlcfh --longoptions docker,local,ci,force,help --name "$(basename "$0")" -- "$@") || {
  usage >&2
  exit 2
}
eval set -- "$PARSED"

SETUP=""
FORCE=0

set_setup() {
  if [ -n "$SETUP" ] && [ "$SETUP" != "$1" ]; then
    echo "$(basename "$0"): only one setup can be selected (got --$SETUP and --$1)" >&2
    usage >&2
    exit 2
  fi
  SETUP="$1"
}

while true; do
  case "$1" in
    -d|--docker) set_setup docker; shift ;;
    -l|--local)  set_setup local;  shift ;;
    -c|--ci)     set_setup ci;     shift ;;
    -f|--force)  FORCE=1;          shift ;;
    -h|--help)   usage; exit 0 ;;
    --) shift; break ;;
    *) echo "$(basename "$0"): unexpected argument: $1" >&2; exit 2 ;;
  esac
done

if [ $# -ne 0 ]; then
  echo "$(basename "$0"): unexpected positional arguments: $*" >&2
  usage >&2
  exit 2
fi

if [ -z "$SETUP" ]; then
  echo "$(basename "$0"): a setup is required" >&2
  usage >&2
  exit 2
fi

SOURCE_DIR="$TEMPLATES_DIR/$SETUP"

for file in "${FILES[@]}"; do
  if [ ! -f "$SOURCE_DIR/$file" ]; then
    echo "$(basename "$0"): missing template $SOURCE_DIR/$file" >&2
    exit 1
  fi
done

if [ "$FORCE" -eq 0 ]; then
  for file in "${FILES[@]}"; do
    if [ -e "$ROOT_DIR/$file" ]; then
      echo "$(basename "$0"): $ROOT_DIR/$file already exists; use --force to overwrite" >&2
      exit 1
    fi
  done
fi

for file in "${FILES[@]}"; do
  cp "$SOURCE_DIR/$file" "$ROOT_DIR/$file"
  echo "Wrote $file ($SETUP)"
done

#!/usr/bin/env bash
#   incremental-index/  two packages revised years apart, so a package index
#                       timestamp between them makes the import incremental.
#   revised-index/      the same cabal path twice, in chronological order, so
#                       the import has to pick the newest revision.
#
# Everything else about these packages is deliberately boring.
#
# Run after changing a fixture. Output is reproducible in the only way the
# tests care about (entry paths, mtimes and contents) but not byte-for-byte, since that also depends on the local gzip.
set -euo pipefail

cd "$(dirname "$0")"

if ! tar --version 2>/dev/null | head -1 | grep -q 'GNU tar'; then
  # echo "error: GNU tar is required (found: $(tar --version 2>/dev/null | head -1))" >&2
  exit 1
fi

tar_at() { # tar_at <mtime> <archive> <create|append> <path>
  local mtime=$1 archive=$2 mode=$3 path=$4
  local flag=-cf
  [[ $mode == append ]] && flag=-rf
  tar --format=ustar --mtime="$mtime" --owner=0 --group=0 --numeric-owner \
    "$flag" "$archive" "$path"
}

cabal_file() { # cabal_file <name> <synopsis> <extra-dependency>
  local name=$1 synopsis=$2 dep=${3:-}
  {
    echo "cabal-version: 3.0"
    echo "name: $name"
    echo "version: 1.0.0"
    echo "synopsis: $synopsis"
    echo "build-type: Simple"
    echo
    echo "library"
    echo "  exposed-modules: Lib"
    echo "  build-depends: base${dep:+, $dep}"
    echo "  default-language: Haskell2010"
  }
}

build_incremental() {
  local dir=incremental-index
  local out=$dir/01-index.tar.gz
  mkdir -p "$dir"
  local work
  work=$(mktemp -d)
  trap 'rm -rf "$work"' RETURN

  mkdir -p "$work/stale-pkg/1.0.0" "$work/recent-pkg/1.0.0"
  cabal_file stale-pkg "not revised in a long time" \
    >"$work/stale-pkg/1.0.0/stale-pkg.cabal"
  cabal_file recent-pkg "revised recently" stale-pkg \
    >"$work/recent-pkg/1.0.0/recent-pkg.cabal"

  ( cd "$work"
    tar_at '2010-01-01 00:00:00Z' index.tar create stale-pkg/1.0.0/stale-pkg.cabal
    tar_at '2024-01-01 00:00:00Z' index.tar append recent-pkg/1.0.0/recent-pkg.cabal
  )
  # Into place only once it is complete, so a failure here leaves the committed
  # fixture untouched rather than deleted.
  gzip -n -c "$work/index.tar" >"$work/out.tar.gz"
  mv "$work/out.tar.gz" "$out"
}

build_revised() {
  local dir=revised-index
  local out=$dir/01-index.tar.gz
  mkdir -p "$dir"
  local work
  work=$(mktemp -d)
  trap 'rm -rf "$work"' RETURN

  local path=revised-pkg/1.0.0/revised-pkg.cabal
  mkdir -p "$work/revised-pkg/1.0.0"

  ( cd "$work"
    cabal_file revised-pkg "revision 0" >"$path"
    tar_at '2020-01-01 00:00:00Z' index.tar create "$path"
    # Same path, later mtime: a cabal revision, exactly as Hackage records one.
    cabal_file revised-pkg "revision 1" >"$path"
    tar_at '2021-01-01 00:00:00Z' index.tar append "$path"
  )
  gzip -n -c "$work/index.tar" >"$work/out.tar.gz"
  mv "$work/out.tar.gz" "$out"
}

build_incremental
build_revised
tar -tvzf incremental-index/01-index.tar.gz
tar -tvzf revised-index/01-index.tar.gz

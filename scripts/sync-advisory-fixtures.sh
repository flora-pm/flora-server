#!/usr/bin/env bash

# Usage: scripts/sync-advisory-fixtures.sh [SECURITY_ADVISORIES_CHECKOUT]

set -euo pipefail

HACKAGE_URL="https://hackage.haskell.org"
INDEX_ARCHIVE="$(cd "$(dirname "$0")/.." && pwd)/test/fixtures/Cabal/hackage/01-index.tar.gz"

work_dir="$(mktemp -d "${TMPDIR:-/tmp}/flora-advisory-fixtures.XXXXXX")"
trap 'rm -rf "$work_dir"' EXIT

advisories_dir="${1:-$work_dir/security-advisories}"
if [ $# -eq 0 ]; then
  git clone --quiet --depth 1 https://github.com/haskell/security-advisories "$advisories_dir"
fi

latest_version() {
  curl -fsSL -H 'Accept: application/json' "$HACKAGE_URL/package/$1" \
    | tr -d '{} \n"' | tr ',' '\n' \
    | LC_ALL=C sort -t. -k1,1n -k2,2n -k3,3n -k4,4n -k5,5n -k6,6n \
    | awk -F: 'NF == 2 { any = $1 } $2 == "normal" { normal = $1 } END { print (normal != "" ? normal : any) }'
}

# 2024-04-13T16:48:19Z -> 202404131648.19, for `touch -t`.
touch_stamp() {
  sed -E 's/^([0-9]{4})-([0-9]{2})-([0-9]{2})T([0-9]{2}):([0-9]{2}):([0-9]{2}).*$/\1\2\3\4\5.\6/'
}

# Advisories name a package with `package = "<name>"`; a GHC component
# (`ghc-component = "..."`) is imported as the ghc package.
grep -rhE '^[[:space:]]*(package|ghc-component)[[:space:]]*=' "$advisories_dir/advisories/published" \
  | sed -E "s/^[[:space:]]*ghc-component.*/ghc/; s/^[^=]*=[[:space:]]*[\"']([^\"']+).*/\1/" \
  | LC_ALL=C sort -u > "$work_dir/wanted.txt"

tar -tzf "$INDEX_ARCHIVE" | cut -d/ -f1 | LC_ALL=C sort -u > "$work_dir/present.txt"
LC_ALL=C comm -13 "$work_dir/present.txt" "$work_dir/wanted.txt" > "$work_dir/missing.txt"

echo "$(($(wc -l < "$work_dir/wanted.txt"))) affected packages, $(($(wc -l < "$work_dir/missing.txt"))) missing from $INDEX_ARCHIVE"

if [ ! -s "$work_dir/missing.txt" ]; then
  exit 0
fi

index_dir="$work_dir/index"

while read -r package; do
  version="$(latest_version "$package")"
  if [ -z "$version" ]; then
    echo "Could not find any version of $package on Hackage" >&2
    exit 1
  fi
  echo "Adding $package-$version"
  mkdir -p "$index_dir/$package/$version"
  cabal_file="$index_dir/$package/$version/$package.cabal"
  curl -fsSL \
    -o "$cabal_file" "$HACKAGE_URL/package/$package-$version/$package.cabal" \
    -o "$work_dir/upload-time" "$HACKAGE_URL/package/$package-$version/upload-time"
  stamp="$(touch_stamp < "$work_dir/upload-time")"
  TZ=UTC touch -t "$stamp" "$cabal_file" "$index_dir/$package/$version" "$index_dir/$package"
done < "$work_dir/missing.txt"

gzip -dc "$INDEX_ARCHIVE" > "$work_dir/01-index.tar"
(cd "$index_dir" && find -- * | LC_ALL=C sort \
  | COPYFILE_DISABLE=1 tar -rf "$work_dir/01-index.tar" --numeric-owner --no-recursion -T -)
gzip -n -9 "$work_dir/01-index.tar"
mv "$work_dir/01-index.tar.gz" "$INDEX_ARCHIVE"

echo "Updated $INDEX_ARCHIVE"

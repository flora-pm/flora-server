#!/usr/bin/env bash

set -eux

git add .

fourmolu --mode check $(git ls-files '*.hs')

git status

set +e
git diff --exit-code
diff_code=$?

if [ $diff_code -ne 0 ]
then
  echo "Test formatting failed"
  exit 1
fi

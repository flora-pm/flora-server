#!/usr/bin/env bash

set -eux

git add .

find app src test -name "*.hs" | xargs -P $(nproc) -I {} hlint --refactor-options="-i" --refactor {}

git status

set +e

git diff --exit-code
diff_code=$?

if [ $diff_code -ne 0 ]
then
  echo "Test Hlint failed"
  exit 1
fi

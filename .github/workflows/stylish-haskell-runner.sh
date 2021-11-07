#!/usr/bin/env bash

set -eux

git add .

stylish-haskell -c .stylish-haskell.yaml -r src test app -i

git status

set +e
git diff --exit-code
diff_code=$?

if [ $diff_code -ne 0 ]
then
  echo "Test formatting failed"
  exit 1
fi

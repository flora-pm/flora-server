#!/usr/bin/env bash

set -euo pipefail

output="$(psql "$FLORA_DB_CONNSTRING" -f scripts/missing-fk-indexes.sql 2>&1 > /dev/null)"

if [[ "$output" == *"Missing FK indexes"* ]]
then
    echo "Missing FK index! Run \`psql \"\$FLORA_DB_CONNSTRING\" -f scripts/missing-fk-indexes.sql\` and apply them"
    echo $output
    exit 1
else
    exit 0
fi

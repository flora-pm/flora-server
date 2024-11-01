#!/usr/bin/env bash

set -euo pipefail

output="$(psql "$FLORA_DB_CONNSTRING" -f scripts/duplicate-indexes.sql)"

if [[ "$output" == *"(0 rows)"* ]]
then
    exit 0
else
    echo "Duplicate indexes! Run \`psql \"\$FLORA_DB_CONNSTRING\" -f scripts/duplicate-indexes.sql\` and remove them"
    echo $output
    exit 1
fi

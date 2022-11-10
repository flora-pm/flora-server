#!/bin/bash

COMMIT_MESSAGE_PATTERN="\[((FLORA)-[0-9]+)|NO-ISSUE\]"

case "$PULL_REQUEST_AUTHOR"  ] in
  "dependabot[bot]" | "dependabot" )
    echo "Pull request by dependabot - not checking"
    exit 0
    ;;
      
  *)
    echo "$PULL_REQUEST_TITLE" | grep -E "${COMMIT_MESSAGE_PATTERN}"
    if [ $? -ne 0 ]; then
      echo "Pull request title doesn't start with '[FLORA-ISSUE]' or '[NO-ISSUE]'" > /dev/stderr
      exit 1
    fi
    ;;

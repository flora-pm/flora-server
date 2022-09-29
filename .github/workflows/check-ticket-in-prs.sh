#!/bin/bash

COMMIT_MESSAGE_PATTERN="\[((FLORA)-[0-9]+)|NO-ISSUE\]"
COMMIT_MESSAGES_FILE=$(mktemp /tmp/commit-messages-XXXX.tmp)

trap "rm ${COMMIT_MESSAGES_FILE}" EXIT

curl \
  -H "Accept: application/vnd.github.v3+json" \
  -H "Authorization: Bearer ${GITHUB_TOKEN}" \
  $PULL_REQUEST_COMMITS_URL \
  | jq ".[] | .commit.message" -r > $COMMIT_MESSAGES_FILE

if [ "$PULL_REQUEST_AUTHOR" == "dependabot" ]; then
  echo "Pull request by dependabot - not checking"
  exit 0
else
  echo "$PULL_REQUEST_TITLE" | grep -E "${COMMIT_MESSAGE_PATTERN}"
  if [ $? -ne 0 ]; then
    echo "Pull request title doesn't start with '[FLORA-ISSUE]' or '[NO-ISSUE]'" > /dev/stderr
    exit 1
  fi
fi

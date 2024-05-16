#!/usr/bin/env bash

# Get the latest tag
previous_version=$(git tag --sort=committerdate | tail -n1)

git log --pretty=oneline --no-color $previous_version..HEAD > prlog
changelog-d --prlog prlog

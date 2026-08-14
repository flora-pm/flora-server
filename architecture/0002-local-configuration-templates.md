# 2. Local configuration templates

Date: 2026-08-11

## Status

Proposed

## Context

With various ways of running the various executables (local, docker-compose),
it's important to adjust the configuration files to have the adequate values

## Decision

We will have a script (`scripts/generate-configuration.sh`) that will receive
the desired setup will select the appropriate configuration template and put it
in a well-known place.

So far there are three configurations:

* docker (`-d|--docker`)
* local (`-l|--local`)
* ci (`-c|-ci`)

As such, `environment.*.kdl` files will be re-designed. Executables will
hardcode which files they need for normal operations and running tests.

* `flora.kdl`
* `flora_test.kdl`

* `jobs_runner.kdl`
* `jobs_runner_test.kdl`

## Consequences

Since the expected files will be picked according to the developer's platform,
they will not be tracked by git. Therefore, the executables will have to guide
the user to running the generation script if they don't find the files on disk.

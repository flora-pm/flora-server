# Contributing to Flora

Thank you for your contribution to Flora! We need you to read and understand this document when you open your PR or your ticket.

Before you proceed, we need you to read and acknowledge our [Code of Conduct][CoC] document.

## How to contribute

### Questions

Open a thread in the [Questions][Questions board] discussion board. You'll get help from everyone in the community.

### Issues & Bugs

Open a [Ticket][Ticket] and tell us what you can about your problem.

### Feature requests

Open a thread in the [Feature Request][Feature Request board] discussion board.
Be certain to search if it has already been suggested!

### Pull Requests

You need to

* Read this document
* Have a ticket that you can relate the PR to, so that we can have some context for your change
* Provide screenshots of before/after if you change the UI.
* Insert a changelog entry in the `changelog.d` directory, based on this template:

```cabal
synopsis: Add feature xyz
prs: #102
issues: #100 #101
significance: significant -- Only if this is important enough to be put at the top of the changelog.

description: {

- Detail number 1
- Detail number 2

}
```

You can include (in moderation) some shitposting in your contributions, be it funny memes in your PR descriptions,
humour in git commits that do not prevent their understanding. They will be accepted at the core team's discretion.

Example:

```
Sponsored-By: 2 bottles of Club Mate & a heat wave in Western Europe
```

## Development environment

The compiler version used is described in the `cabal.project` file.
`cabal-install` version 3.16 or higher is needed.

Three environments are supported: Docker (recommended), your host machine, and Nix.

### Docker (recommended)

A docker-based workflow brings all dependencies with it, and communicates with another container
for the PostgreSQL database.

```bash
## Build the development container
$ make docker-build
## Start the containers for the database and the server
$ make docker-up
## Once the containers are running, you can enter the development environment and start hacking
$ make docker-enter
## You'll be in the docker container, ready to start Flora
(docker)$ make start-tmux
## You'll be in a tmux session, everything should be launched
## Visit localhost:8084 from your web browser to see if it all works.
```

The committed `.env` file sets `COMPOSE_PROFILES=local`, which is why plain `docker compose up`
(and `make docker-up`) starts the application services (`devel`, `flora-database`) along with
`prometheus` and `jaeger`. Passing an explicit `--profile` flag on the command line overrides
this default, so combine profiles explicitly when you need more than one
(see [Live Eventlogs](#live-eventlogs)).

### Host setup

The following Haskell command-line tools have to be installed:

* `fourmolu` 0.20.0.0: To style the code base
* `hlint` 3.10 & `apply-refact`: To enforce certain patterns in the code base ("lint")
* `cabal-gild`: To style cabal files
* `ghcid`: To automatically reload the Haskell code base upon source changes
* `ghc-tags`: To generate ctags or etags for the project
* `eventlog2html`: To render eventlog profiles (see [Profiling](#profiling))
* [`changelog-d`](https://codeberg.org/fgaz/changelog-d/releases/tag/v1.0) 1.0: To generate the changelog. Not on Hackage; install it from the Codeberg release.

```
cabal install --ignore-project fourmolu-0.20.0.0 hlint-3.10 apply-refact cabal-gild ghcid ghc-tags eventlog2html --semaphore -j
```

(Some of the above packages could have incompatible dependencies, so consider installing them separately with `cabal install`)

The following system dependencies are needed:

* [`gperftools`](https://pkgs.org/download/gperftools): This gives us `tcmalloc`, an alternative malloc implementation that helps against memory fragmentation in long-running servers
* `libsodium-1.0.18`: The system library that powers most of the cryptography happening in flora
* `yarn`: The tool that handles the JavaScript code bases
* `esbuild`: The tool that handles asset bundling
* PostgreSQL: see [PostgreSQL: first installation](#postgresql-first-installation) if this is your first time

#### Notes on macOS

`cabal.project.freeze` is not portable, so make sure to delete it before running `make build`, if not using the Docker-based setup.

If using `brew`, install both libsodium and pkg-config:
```
$ brew install libsodium pkg-config
```

And add the stanza matching your architecture (pick one) to your `cabal.project.local`:
```
-- ARM
package *
  extra-include-dirs: /opt/homebrew/include
  extra-lib-dirs: /opt/homebrew/lib
```
```
-- Intel
package *
  extra-include-dirs: /usr/local/include
  extra-lib-dirs: /usr/local/lib
```

#### PostgreSQL: first installation

If this is your first time with PostgreSQL, here is what you should do:

1. Locate the `pg_hba.conf` file. If a search engine cannot help you, you can find it easily with
   `sudo find / -type f -name pg_hba.conf` on UNIX systems.
2. Go to the bottom of the file and perform the following changes:

```diff
local   all             all                                     peer
## IPv4 local connections:
- host    all             all             127.0.0.1/32            md5
+ host    all             all             127.0.0.1/32            scram-sha-256
## IPv6 local connections:
- host    all             all             ::1/128                 md5
+ host    all             all             ::1/128                 scram-sha-256
```
3. Restart the database engine (using `systemctl` on Linux, or `brew services restart postgresql@17`
    if you have installed PostgreSQL with `brew`)

4. Connect (via sudo) to the `root` user

```bash
user $ sudo -s
[sudo] password: [type your user's password]
```
Then as root, connect to the postgres account, and open a `psql` shell.

```bash
root # su -l postgres
postgres $ psql
psql (17.9 (OS version here))
Type "help" for help.
```

Now, set the password for user `postgres` to the character string `'postgres'`

```
postgres=# alter role postgres with password 'postgres';
```

And you are good to go.

### Nix

Nix is an alternative way to interact with the Flora codebase.

`Flora` provides a [nix](https://github.com/flora-pm/flora.nix) setup to make provisioning a development environment.

#### Formatting and Linting with nix and `pre-commit-hooks`

After starting up a `devShell` with `nix develop` or `direnv`, a `shellHook` will run that installs two things
- a script, called `pre-commit`
- a pre-commit hook for git

The former can be invoked to run the formatters and linters on the entire project, to check everything, run

```bash
pre-commit run -a
```

to check a single check run

```bash
pre-commit run <check>
```

> **Note**
> The available hooks can be found in `./nix/pre-commit-config.nix`

The latter is invoked when calling `git commit`, it will abort the commit, if the linting and formatting does not succeed.

If you want to commit although they do not succeed, pass `--no-verify` to the `git commit` command.

> **Warning**
> Be careful that this does not mean you get around linting and formatting, as they're checked in `CI`

## Working on Flora

### Configuration

The configuration is handled through KDL files. The following environments are provided:

* `environment.kdl`: local development
* `environment.docker.kdl`: development inside the Docker container
* `environment.ci.kdl`: continuous integration
* `environment.test.kdl` & `environment.test.local.kdl`: test suite

When interfacing via `make` this is handled for you (the Makefile uses `environment.kdl`).
If you're interfacing with the `flora-cli` directly, pass an environment with `--config` or `-c`.

Example:
```bash
cabal run -- flora-cli -c environment.kdl provision categories
```

Use `flora-cli --help` to see what commands are available.

A very useful command to run is

```bash
## Starts a tmux session with code reloading for frontend and backend
$ make start-tmux
```

To explore the other possible `Make` rules, type:

```bash
$ make help
```

### Database

The `make` targets below use `environment.kdl`, which reaches the database on
`localhost:5432`. This works on the host — including against the compose database, whose
port is published. Inside the `devel` container the database lives on the `flora-database`
host instead, so override the config there:

```bash
(docker)$ make db-setup CONFIG=environment.docker.kdl
```

To create the database and apply the migrations, type:

```bash
$ make db-setup
```

You can also use `db-create` and `db-drop` to create and delete the database, and `db-reset`
to drop, re-create and re-provision it.

Then populate the development database:

```bash
$ make db-provision
$ cabal run -- flora-cli -c environment.kdl create-user --admin --can-login --username "admin" \
    --email "admin@localhost" --password "password123"
$ make db-provision-packages
```

#### Importing a package index

The previous paragraph shows how to import test packages, but you may want to import a whole package index, for shits and giggles.

You can do so with:

```bash
$ cabal run flora-cli -- -c environment.kdl import-index ~/.cabal/packages/hackage.haskell.org/01-index.tar.gz \
  --repository hackage.haskell.org
```

Similarly if you have the [cardano packages index](https://input-output-hk.github.io/cardano-haskell-packages/) configured, run:

```bash
$ cabal run flora-cli -- -c environment.kdl import-index ~/.cabal/packages/cardano/01-index.tar.gz \
  --repository "cardano"
```

#### Connecting to the local database

If you need to connect to the database directly:

```bash
## flora_dev is the host-provisioned dev database; use flora_dev_1 for a database
## provisioned with environment.docker.kdl, or flora_test for the test database
psql -h localhost -p 5432 -U postgres -d flora_dev
```

### Profiling

#### Offline profiling with eventlog2html

If you are about to run `flora-cli` or `flora-server` with profiling, please first read
https://well-typed.com/blog/2021/01/first-look-at-hi-profiling-mode/.

Here are the steps:

1. `$ cabal --project-file=cabal.profiling.project build flora-server` (or `flora-cli`)
2. `$ cabal --project-file=cabal.profiling.project run -- flora-server -c environment.kdl +RTS -l -hi -i0.5 -RTS`
3. `$ eventlog2html flora-server.eventlog`

Also consider [capturing live eventlogs](#live-eventlogs) during development.

#### Live Eventlogs

Flora can stream its GHC eventlog live via
[eventlog-live](https://github.com/well-typed/eventlog-live): heap and GC
metrics (to Prometheus). The `live-eventlog` compose profile runs the
forwarders, an OpenTelemetry collector, Prometheus, and Grafana.

1. Ensure `eventlogSocketDirectory` is set in your config file (the committed
   environment files set it to `/tmp/flora-eventlog`).
   Each executable creates its own `<progName>.sock` inside it.
2. Run the server with eventlog RTS flags. These commands use
   `environment.docker.kdl` and are meant to be run **inside the `devel`
   container** (see [On the host machine](#on-the-host-machine) for host runs):

```
$ cabal --project-file=cabal.profiling.project run flora-server -- -c environment.docker.kdl +RTS -l -hi -i5 --eventlog-flush-interval=1 -RTS
```

flora-jobs-runner can be profiled the same way, with the same RTS flags:

```
$ cabal --project-file=cabal.profiling.project run flora-jobs-runner -- -c environment.docker.kdl +RTS -l -hi -i5 --eventlog-flush-interval=1 -RTS
```

Note: `--eventlog-flush-interval=1` has a measurable runtime cost; use it
for profiling sessions only.

`-hi` produces heap/GC metrics (to Prometheus). `-i5` sets the heap-census
interval to 5s (the RTS default of 0.1s floods the collector).

##### Docker-compose

The `live-eventlog` profile starts the observability stack (forwarders, collector, Prometheus, Grafana).

In order to start both the application services and the observability stack, type:

```bash
$ docker compose --profile local --profile live-eventlog up --build
```

Then open `http://localhost:3000` and use the "Flora Eventlog Heap" dashboard
for heap/GC metrics, or "Eventlog Profiles" for cost-centre flamegraphs (only
populated when the process runs with `-p`, see above). If host port 3000 is
taken, set `FLORA_GRAFANA_PORT` to remap Grafana's host port.

##### On the host machine

For a host-run executable, use a configuration whose database points at
`localhost` (e.g. `environment.kdl`), create the socket
directory (`mkdir -p /tmp/flora-eventlog`) and set
`FLORA_EVENTLOG_DIR=/tmp/flora-eventlog` when starting the stack; a set
`FLORA_EVENTLOG_DIR` makes the forwarders bind-mount that host directory
instead of the named volume:

```bash
$ FLORA_EVENTLOG_DIR=/tmp/flora-eventlog docker compose --profile live-eventlog up --build
```

## Project Architecture

### Layers

#### Database model layer

The files of our data model follows a predictible layout.

`Flora.Model` contains directories where the database entities live. They have standardised files within them:

```
.
└─ Flora.Model.*
   ├── Guard.hs  ← monadic guards for easy acces to resources
   ├── Query.hs  ← read-only database queries
   ├── Types.hs  ← type definitions, class instances, smart constructors
   └── Update.hs ← read-write database statements
```

#### Business logic layer

Business logic lives under `Flora.Domain`:

```
.
└─ Flora.Domain
   ├── Flora.Domain.Package ← package/release resolution
   ├── Flora.Domain.Release ← pure release rules, e.g. `latestViableRelease`
   ├── Flora.Domain.Search  ← search over the model (own `flora-search` library)
   └── Flora.Domain.Import  ← the package import pipeline
```

These modules use the database layer to expose re-usable functions and logic, which can be shared
between the Pages and API controllers

#### Web layer

Route definitions and implementation are located in the `FloraWeb` namespace, with in particular:

```
.
└─ FloraWeb
   ├── FloraWeb.API   ← JSON API
   ├── FloraWeb.Atom  ← Atom Feed
   └── FloraWeb.Pages ← HTML pages
```

### Wrapped entity IDs

We never use raw UUIDs for primary keys, but we wrap those UUIDs in smart constructors that will either generate one randomly,
or in certain cases generate the ID deterministically, based on immutable characteristics of the entity in question.

### Effects

Since Flora is a piece of software with lots of interactions with the outside world, we model those through an effect system,
which allows us to tag functions' type signatures. Effects are provided by most of the third-party libraries that we use
(logging, filesystem, etc.) and we have defined our own for database interactions (See the [Database](#database-1) section for more information).

### Database

#### Schema migrations

Application schema migrations are kept as raw SQL in the `migrations/` directory. They are applied through the `flora-migrate` executable.
Arbiter – the job queue – generates its own schema migrations, which are applied in `flora-migrate` too.

#### Read-Only and Read-Write operations

We differentiate read-only and read-write operations in the codebase through the usage of two different effects in type signatures: `ReadDB` and `WriteDB`.

Interpreting the `ReadDB` effect gives us access to read-only transactions, that are enforced in PostgreSQL by setting the transaction mode to "READ ONLY".
See [SET TRANSACTION][SET TRANSACTION] for more details.

## Making a release (maintainers)

Here is the procedure to follow when making a release:

1. Create a PR to prepare the release of the next version targeting `development`. It must include:
    * Bump the version in the flora.cabal file
    * Generate the changelog with ./scripts/generate-changelog.sh
    * Write down the date in the CHANGELOG
    * Remove the changelog.d entries

2. Once the PR is merged into `development`, merge `development` into `main`
3. Create a [new release](https://github.com/flora-pm/flora-server/releases/new).

[CoC]: https://github.com/flora-pm/flora-server/blob/development/CODE_OF_CONDUCT.md
[Feature Request board]: https://github.com/flora-pm/flora-server/discussions/new?category=feature-requests
[Questions board]: https://github.com/flora-pm/flora-server/discussions/categories/questions
[Ticket]: https://github.com/flora-pm/flora-server/issues/new
[SET TRANSACTION]: https://www.postgresql.org/docs/current/sql-set-transaction.html

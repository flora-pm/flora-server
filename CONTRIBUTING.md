Thank you for your contribution to Flora! We need you to read and understand this document when you open your PR or your ticket:

## Project Setup

We need you to read and acknowledge our [Code of Conduct][CoC] document.

The compiler version used is described in the `cabal.project` file.
`cabal-install` version 3.8 or higher is needed.

The following Haskell command-line tools will have to be installed:

* `postgresql-migration`: To perform schema migrations
* `fourmolu`: To style the code base. Version is 0.14.1.0
* `hlint` & `apply-refact`: To enforce certain patterns in the code base ("lint")
* `cabal-fmt` and `nixfmt`: To style the cabal and nix files
* `ghcid`: To automatically reload the Haskell code base upon source changes
* `ghc-tags`: To generate ctags or etags for the project

(Some of the above packages have incompatible dependencies, so don't try to install them all at once with `cabal install`)

* `libsodium-1.0.18`: The system library that powers most of the cryptography happening in flora
* `yarn`: The tool that handles the JavaScript code bases
* `esbuild`: The tool that handles asset bundling
* `changelog-d` v1.0: https://codeberg.org/fgaz/changelog-d/releases/tag/v1.0

### Questions

Open a thread in the [Questions][Questions board] discussion board. You'll get help from everyone in the community.

### Issues & Bugs

Open a [Ticket][Ticket] and tell us what you can about your problem.

### Pull Requests

You need to

* Read this document
* Have a ticket that you can relate the PR to, so that we can have some context for your change
* Provide screenshots of before/after if you change the UI.
* Put `[FLORA-XXXX]` where XXXX is the ticket this PR is related to, or [NO-ISSUE] if no tickets are related, in the
PR title and commit message:

```
[NO-ISSUE] Update dependencies for Storybook.js
```

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

### Feature request

Open a thread in the [Feature Request][Feature Request board] discussion board.
Be certain to search if it has already been suggested!

### Making a release

Here is the procedure to follow when making a release:

1. Create a PR to prepare the release of the next version targeting `development`. It must include:
    * Bump the version in the flora.cabal file
    * Generate the changelog with ./scripts/generate-changelog.sh
    * Write down the date in the CHANGELOG
    * Remove the changelog.d entries

2. Once the PR is merged into `development`, merge `development` into `main`
3. Create a [new release](https://github.com/flora-pm/flora-server/releases/new).

### Profiling

If you are about to run `flora-cli` or `flora-server` with profiling, please first read
https://well-typed.com/blog/2021/01/first-look-at-hi-profiling-mode/.

Here are the steps:

1. `$ cabal build flora-server -f prof` (or `flora-cli`)
2. `$ cabal run -- flora-server +RTS -l -hT -i0.5 -RTS`
3. `$ eventlog2html flora-server.eventlog`

## Installation and Configuration

Step 1. Read The above "Project Setup" section.
Step 2. Keep reading from here.

### Flora server

The configuration is handled through environment variables. They are all prefixed by `FLORA_` to avoid conflict, and the
server will tell you which ones are missing.

To start in the best of conditions, create a file called `environment.local.sh` with the following content:

```bash
source environment.sh
```

This will get all the variables from `environment.sh` and allow you to override them locally.

If you use `direnv`, you are advised to create a symbolic link from `environment.local.sh` to `.envrc`.

You can then build the server with `make build`. Do **not** simply run `cabal build`.

A very useful command to run is

```bash
$ make start-tmux
```
To start a tmux session with code reloading for frontend and backend:


To explore the other possible `Make` rules, type:

```bash
$ make help
```

### Database

The Flora server uses PostgreSQL 14. Please install it.

#### Side-Quest: First installation

If this is your first time with PostgreSQL, here is what you should do:

1. Locate the `pg_hba.conf` file. If a search engine cannot help you, you can find it easily with
   `sudo find / -type f -name pg_hba.conf` on UNIX systems.
2. Go to the bottom of the file and perform the following changes:

```diff
local   all             all                                     peer
# IPv4 local connections:
- host    all             all             127.0.0.1/32            md5
+ host    all             all             127.0.0.1/32            scram-sha-256
# IPv6 local connections:
- host    all             all             ::1/128                 md5
+ host    all             all             ::1/128                 scram-sha-256
```
3. Restart the database engine (using `systemctl` on Linux, or `brew services restart postgresql@14`
    if you have installed PostgreSQL with `brew`)

3. Connect (via sudo) to the `root` user

```bash
user $ sudo -s
[sudo] password: [type your user's password]
```
Then as root, connect to the postgres account, and open a `psql` shell.

```bash
root # su -l postgres
postgres $ psql
psql (14.7 (Ubuntu 14.7-1.pgdg18.04+1))
Type "help" for help.
```

Now, set the password for user `postgres` to the character string `'postgres'`

```
postgres=# alter role postgres with password 'postgres';
```

And you are good to go.

#### Setup project

To create the database and apply the migrations, type:

```bash
$ make db-setup
```

you can also use `db-create` and `db-drop` to create and delete the database in the PostgreSQL instance.

### Docker Workflow

A docker-based workflow is provided. The idea is to develop from within a container that brings with it all dependencies,
and communicates with another container for the PostgreSQL database.

```bash
# Start the containers for the database and the server
$ make docker-up
# Once the containers are running, you can enter the development environment and start hacking
$ make docker-enter
# You'll be in the docker container. Environment variables are automatically set
# so you should be able to start Flora
(docker)$ make start-tmux
# You'll be in a tmux session, everything should be launched
# Visit localhost:8084 from your web browser to see if it all works.
```

If you need to rebuild the container, run the following command:

```bash
$ make docker-build
```

### Provisioning the database

After everything is set up, (locally or via Docker), you can start populating the database:

```bash
$ make db-setup
$ make db-provision
$ cabal run -- flora-cli create-user --admin --can-login --username "admin" \
    --email "admin@localhost" --password "password123"
$ make db-provision-test-packages
```

### Importing a package index

The previous paragraph shows how to import test packages, but you may want to import a whole package index, for shit and giggles.

You can do so with:

```bash
$ cabal run flora-cli -- import-index ~/.cabal/packages/hackage.haskell.org/01-index.tar.gz \
  --repository hackage.haskell.org
```

Similarly if you have the [cardano packages index](https://input-output-hk.github.io/cardano-haskell-packages/) configured, run:

```bash
$ cabal run flora-cli -- import-index ~/.cabal/packages/cardano/01-index.tar.gz \
  --repository "cardano"
```

### Nix

Nix is an alternative way to interact with the Flora codebase.

`Flora` provides a `nix` setup to make provisioning a development environment and creating reproducible and simple
builds. To show all available flake attributes, run

```bash
nix flake show -Lv --allow-import-from-derivation --fallback
```

#### Using `nix` as provider for a development environment

Obtaining a `devShell` which contains all tools for develop `flora`, including correct compiler and haskell tooling
is as simple as running

```bash
nix -Lv develop
```

We recommend using our proposed `nix` config settings, including the extra binary cache, but ultimately it's up to you
whether you trust those settings by reacting with `y` on the prompt after running a `nix` command.

> **Warning**
> accepting binary caches from a flake requires elevated rights for `nix`, only allow it, if you know what you're doing

#### Using `nix` with `direnv`

Direnv can drastically reduce development cycles by reducing the amount of times `nix` evaluates the expressions for
this repository, which is a drastic improvement, especially with `IFD` (which this repo uses due to `callCabal2nix`).

Devshell startup times will be instant if you didn't change anything in the configuration and as long as usual if you
need to re-evaluate the `nix`-expressions (i.e. on cabal config changes or `nix` changes).

Find out how to install `direnv` on your machine by visiting [their github](https://github.com/direnv/direnv/).o
After installing, add a `.envrc` file to the root of the project containing:

```bash
use flake -Lv --fallback
```

and run

```bash
direnv allow
```

To reload the `direnv` environment, run

```bash
direnv reload
```

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

#### Using `nix` to build and run flora

To verify, that the haskell code builds, the tests pass and the formatting and linting are correct, as well as the nix code
working, run

```bash
nix flake check -Lv --allow-import-from-derivation
```

To build `flora`, invoke

```bash
nix build -Lv
```

To run the `cli`, run

```bash
nix run .#cli
```

To run the server, run

```bash
nix run .#server
```

#### Contributing to our `nix` infrastructure

Contributions to our `nix` infrastructures are always appreciated, however, there are a couple of guidelines
- don't forget to run formatting and linting (see above for `pre-commit-hooks`)
- prefer cached derivations, that means:
  - prefer upstream haskell packages over custom versions-
  - prefer frameworks that have reliable and trusted binary caches
- prefer versions with less IFD:
  - prefer realized `nix` derivations over `callHackage` over `callCabal2nix`
  - don't use custom packages if not absolutely necessary
- locking happens in the `nix` flake
  - `nix` provides a native locking mechanism with flakes, we only use that mechanism
  - if we need a source of a package, we add it as a flake input with `flake = false;`
  - we don't use any fetcher, if not absolutely needed (e.g. if you need a tarball which is
    not unpacked, it might sometimes be necessary)

[CoC]: https://github.com/flora-pm/flora-server/blob/development/CODE_OF_CONDUCT.md
[Feature Request board]: https://github.com/flora-pm/flora-server/discussions/new?category=feature-requests
[Questions board]: https://github.com/flora-pm/flora-server/discussions/categories/questions
[Ticket]: https://github.com/flora-pm/flora-server/issues/new
[nix-flakes]: https://nixos.wiki/wiki/Flakes

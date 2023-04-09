Thank you for your contribution to Flora! There is no Contributor License Agreement (CLA) to sign,
but we need you to read this document when you open your PR or your issue:

## Contributing

We need you to read and acknowledge our [Code of Conduct][CoC] document.

The compiler version used is described in the `cabal.project` file.
`cabal-install` version 3.8 or higher is needed.

The following Haskell command-line tools will have to be installed:

* `postgresql-migration`: To perform schema migrations
* `fourmolu`: To style the code base. Minimum version is 0.8.2.0
* `hlint` & `apply-refact`: To enforce certain patterns in the code base ("lint")
* `cabal-fmt` and `nixfmt`: To style the cabal and nix files
* `ghcid`: To automatically reload the Haskell code base upon source changes
* `ghc-tags`: To generate ctags or etags for the project

(Some of those packages have incompatible dependencies, so don't try to install them all at once with cabal)

* `yarn`: The tool that handles the JavaScript code bases
* `esbuild`: The tool that handles asset bundling

### Nix

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

> Warning
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

to check a single target run

```bash
pre-commit run <target>
```

> Note
> The available hooks can be found in `./nix/pre-commit-config.nix`

The latter is invoked when calling `git commit`, it will abort the commit, if the linting and formatting does not succeed.

If you want to commit although they do not succeed, pass `--no-verify` to the `git commit` command.

> Warning
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
nix run .#server
```

To run the server, run

```bash
nix run .#cli
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

Open tasks at the moment of writing include (ordered by priority):
- support and test on different architectures than `x86_64-linux`
- build containers for `flora` in `nix`
- offer easy deployments of `flora` with `nix`
- switch to [flake-parts](https://flake.parts/)

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

### Feature request

Open a thread in the [Feature Request][Feature Request board] discussion board.
Be certain to search if it has already been suggested!

### Making a release

Here is the procedure to follow when making a release:

1. Create a PR to prepare the release of the next version targeting `development`. It must include:
    * Bump the version in the flora.cabal file
    * Write down the date in the CHANGELOG
2. Once the PR is merged into `development`, merge `development` into `main`
3. Create a [new release](https://github.com/flora-pm/flora-server/releases/new).

### Questions 

Open a thread in the [Questions][Questions board] discussion board. You'll get help from everyone in the community.

### Issues & Bugs

Open a [Ticket][Ticket] and tell us what you can about your problem.

### Profiling

If you are about to run `flora-cli` or `flora-server` with profiling, please first read
https://well-typed.com/blog/2021/01/first-look-at-hi-profiling-mode/.

Here are the steps:

1. `$ cabal build flora-server -f prof` (or `flora-cli`)
2. `$ cabal run -- flora-server +RTS -l -hT -i0.5 -RTS`
3. `$ eventlog2html flora-server.eventlog`

[CoC]: https://github.com/flora-pm/flora-server/blob/development/CODE_OF_CONDUCT.md
[Feature Request board]: https://github.com/flora-pm/flora-server/discussions/new?category=feature-requests
[Questions board]: https://github.com/flora-pm/flora-server/discussions/categories/questions
[Ticket]: https://github.com/flora-pm/flora-server/issues/new
[nix-flakes]: https://nixos.wiki/wiki/Flakes

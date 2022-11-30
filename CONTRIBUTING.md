Thank you for your contribution to Flora! There is no Contributor License Agreement (CLA) to sign,
but we need you to read this document when you open your PR or your issue:

## Contributing

We need you to read and acknowledge our [Code of Conduct][CoC] document.

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

### Nix Setup

Nix Flakes are used to build the application. It is recommended your version of the `nix` tool
is at least 2.11. Please do not forget to add the appropriate configuration line in order to use flakes, as
described in the [NixOS Wiki][nix-flakes] page.

After that, it should be straightforward to run:

```
$ make nix-shell
```

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
2. Once the PR is merged into `development`, merge `development` into `master`
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

[CoC]: https://github.com/flora-pm/flora-server/blob/master/CODE_OF_CONDUCT.md
[Feature Request board]: https://github.com/flora-pm/flora-server/discussions/new?category=feature-requests
[Questions board]: https://github.com/flora-pm/flora-server/discussions/categories/questions
[Ticket]: https://github.com/flora-pm/flora-server/issues/new
[nix-flakes]: https://nixos.wiki/wiki/Flakes

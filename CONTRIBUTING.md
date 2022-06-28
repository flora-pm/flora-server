Thank you for your contribution to Flora! There is no Contributor License Agreement (CLA) to sign,
but we need you to read this document when you open your PR or your issue:

## Contributing

We need you to read and acknowledge our [Code of Conduct][CoC] document.

The following Haskell command-line tools will have to be installed:

* `postgresql-migration`: To perform schema migrations
* `fourmolu`: To style the code base
* `hlint` & `apply-refact`: To enforce certain patterns in the code base ("lint")
* `cabal-fmt` and `nixfmt`: To style the cabal and nix files
* `ghcid`: To automatically reload the Haskell code base upon source changes
* `ghc-tags`: To generate ctags or etags for the project

```bash
$ cabal install -j postgresql-migration fourmolu hlint apply-refact cabal-fmt nixfmt ghcid ghc-tags
```

* `yarn`: The tool that handles the JavaScript code bases
* `esbuild`: The tool that handles asset bundling

### Pull Requests

You need to

* Read this document
* Have a ticket that you can relate the PR to, so that we can have some context for your change
* Provide screenshots of before/after if you change the UI.

### Feature request

Open a thread in the [Feature Request][Feature Request board] discussion board.
Be certain to search if it has already been suggested!

### Questions 

Open a thread in the [Questions][Questions board] discussion board. You'll get help from everyone in the community.

### Issues & Bugs

Open a [Ticket][Ticket] and tell us what you can about your problem.

[CoC]: https://github.com/flora-pm/flora-server/blob/master/CODE_OF_CONDUCT.md
[Feature Request board]: https://github.com/flora-pm/flora-server/discussions/new?category=feature-requests
[Questions board]: https://github.com/flora-pm/flora-server/discussions/categories/questions
[Ticket]: https://github.com/flora-pm/flora-server/issues/new

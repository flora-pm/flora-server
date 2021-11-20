<h1 align="center"> Flora </h1>

<p align="center">
<a href="https://github.com/flora-pm/flora-server/actions">
  <img src="https://img.shields.io/github/workflow/status/flora-pm/flora-server/CI?style=flat-square" alt="CI badge" />
</a>
<a href="https://haskell.org">
  <img src="https://img.shields.io/badge/Made%20in-Haskell-%235e5086?logo=haskell&style=flat-square" alt="made with Haskell"/>
</a>
</p>

## Screenshot gallery

<img src="https://github.com/flora-pm/flora-server/raw/development/images/flora-package-view.png" alt="package view">

## Development

Source the `environment.sh` directly or through a `environment.local.sh` file (that is not tracked) if you want
to override some default values.

```bash
$ make help
start                          Start flora-server
build                          Build the server without optimisations
assets-deps                    Install the dependencies of the frontend
assets-build                   Build the web assets
assets-watch                   Continuously rebuild the web assets
assets-clean                   Remove JS artifacts
db-init                        Initialize the dev database
db-start                       Start the dev database
db-create                      Create the database
db-drop                        Drop the database
db-setup                       Setup the dev database
db-reset                       Reset the dev database
repl                           Start a REPL
test                           Run the test suite
ghcid-test                     Load the tests in ghcid and reload them on file change
ghcid-server                   Start flora-server in ghcid
lint                           Run the code linter (HLint)
style                          Run the code styler (stylish-haskell)
```

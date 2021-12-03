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

Create a `environment.local.sh` file (not tracked by git) and insert this line at the top:

```bash
source environment.sh
```

If you use [direnv](https://direnv.net/), you can link `environment.local.sh` to `.envrc` in order to reload the configuration
variables automatically.

### Requirements

All of these tools are provided by the Nix shell.

* PostgreSQL server*
* NodeJS & Yarn
* dbmate[¹][dbmate]

[¹]: https://github.com/amacneil/dbmate

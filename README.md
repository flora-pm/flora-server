<h1 align="center"> Flora <br> <small>A package index for the Haskell ecosystem</small> </h1>

<p align="center">
  <img src="https://raw.githubusercontent.com/flora-pm/flora-server/development/assets/favicon.svg" height=250 width=250 alt="Logo" />
</p>
<p align="center">

<a href="https://github.com/flora-pm/flora-server/actions">
  <img src="https://img.shields.io/github/workflow/status/flora-pm/flora-server/CI/development?style=flat-square" alt="CI badge" />
</a>
<a href="https://haskell.org">
  <img src="https://img.shields.io/badge/Made%20in-Haskell-%235e5086?logo=haskell&style=flat-square" alt="made with Haskell"/>
</a>
</p>

<dl>
  <p>
  <dt>Flora.pm</dt>
  <dd>A read-only mirror of hackage.haskell.org with an improved and lightweight interface.</dd>
 </p>

  <p>
  <dt>Flora Server</dt>
  <dd>An alternative package index server for the Haskell ecosystem with mirroring capabilities.</dd>
  </p>
</dl>


**Read More**

* [Code of Conduct](./CODE_OF_CONDUCT.md)
* [Contribution Guide](./CONTRIBUTING.md)
* [Development Wiki](https://github.com/flora-pm/flora-server/wiki)

## Installation and Configuration

For ease of development, a `shell.nix` file is provided. It brings with it system dependency and tooling.

To jump into the development environment, use `make nix-shell`. It is impure by default, so your editor and development
tools will still be accessible.

### System dependencies

* libpq
* Soufflé Datalog

### Flora server

Configuration is handled through environment variables. They are all prefixed by `FLORA_` to avoid conflict, and the
server will tell you which ones are missing.

To start in the best of conditions, create a file called `environment.local.sh` with the following content:

```bash
source environment.sh

# export FLORA_SENTRY_DSN="" # Don't forget to add your Sentry DSN if you use it!
# export FLORA_PROMETHEUS_ENABLED="true"
```

This will get all the variables from `environment.sh` and allow you to override them.

You can then build the server with 

```bash
# To build the binaries
$ make build
# To load the main library in a REPL
$ make repl
```

### Database

The Flora server uses PostgreSQL 14.1

To create the database and apply the migrations, type:

```bash
$ make db-setup # Implies db-create
```

you can also use `db-create` and `db-drop` to create and delete the database in the PostgreSQL instance.

### Docker Workflow

A docker-based workflow is provided. The idea is to develop from within a container that brings with it all dependencies,
and communicates with another container for the Postgres database.

```bash
# You need to build the container first. It's gonna take around 13 minutes the first time you build
$ make docker-build
# Start the container.
$ make docker-start
# Once the containers are running, you can enter the development environment and start hacking
$ make docker-enter
# You'll be in the docker container. Environment variables are automatically set 
# so you should be able to start Flora
(docker)$ make nix-tmux
# You'll be in a tmux session, everything should be launched
# Visit localhost:8084 from your web browser to see if it all works.

# To provision the development database, type:
$ make docker-enter
(docker)$ source environment.docker.sh
(docker)$ make db-drop  # password is 'postgres' by default
(docker)$ make db-setup # password is 'postgres' by default
(docker)$ make nix-provision
# And you should be good!
```
---

You can explore the Makefile rules by typing `make` in your shell.

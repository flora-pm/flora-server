<h1 align="center"> Flora </h1>

<p align="center">
<a href="https://github.com/flora-pm/flora-server/actions">
  <img src="https://img.shields.io/github/workflow/status/flora-pm/flora-server/CI/development?style=flat-square" alt="CI badge" />
</a>
<a href="https://haskell.org">
  <img src="https://img.shields.io/badge/Made%20in-Haskell-%235e5086?logo=haskell&style=flat-square" alt="made with Haskell"/>
</a>
</p>

<img src="https://github.com/flora-pm/flora-server/raw/development/images/flora-about.png" alt="about page">

## Installation and Configuration

For ease of development, a `shell.nix` file is provided. It brings with it system dependency and tooling.

To jump into the development environment, use `make nix-shell`. It is impure by default, so your editor and development
tools will still be accessible.

## Installing using Docker instead

If you are using an environment that is not friendly to nix, like MacOS, you may want to use Docker instead.

To build the image, run `docker build -t flora-server .`. The build will take a while to download and set up dependencies the first time it is run, but subsequent builds should be significantly faster.

The image is set up to run the nix-shell on boot, so once the build is complete simply run `docker run -it -p 8083:8083 --rm flora-server`. Then you should be able to build the server itself by `nix-build`, or use any of the `make` commands as described below. 

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

the Flora server uses PostgreSQL 14.1

To create the database and apply the migrations, type:

```bash
$ make db-setup # Implies db-create
```

you can also use `db-create` and `db-drop` to create and delete the database in the PostgreSQL instance.

---

You can explore the Makefile rules by typing `make` in your shell.

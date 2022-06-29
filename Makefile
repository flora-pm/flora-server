start: ## Start flora-server
	@cabal run exe:flora-server

build: soufflé ## Build the backend
	@cabal build -j -O1

clean: ## Remove the cabal build artifacts
	@rm cbits/*.cpp
	@cabal clean

assets-deps: ## Install the dependencies of the frontend
	@cd assets/ && yarn

build-assets: assets-deps ## Build the web assets
	@cd assets/ && yarn build

watch-assets: ## Continuously rebuild the web assets
	@cd assets/ && yarn watch

clean-assets: ## Remove JS artifacts
	@cd assets/ && rm -R node_modules

db-create: ## Create the database
	@createdb -h $(FLORA_DB_HOST) -p $(FLORA_DB_PORT) -U $(FLORA_DB_USER) $(FLORA_DB_DATABASE)

db-drop: ## Drop the database
	@dropdb -f --if-exists -h $(FLORA_DB_HOST) -p $(FLORA_DB_PORT) -U $(FLORA_DB_USER) $(FLORA_DB_DATABASE)

db-setup: db-create ## Setup the dev database
	@migrate init "$(FLORA_PG_CONNSTRING)" 
	@migrate migrate "$(FLORA_PG_CONNSTRING)" migrations

db-reset: db-drop db-setup db-provision ## Reset the dev database (uses Cabal)

db-provision: build ## Load the development data in the database
	@cabal run -- flora-cli create-user --username "hackage-user" --email "tech@flora.pm" --password "foobar2000"
	@cabal run -- flora-cli provision categories
	@cabal run -- flora-cli provision test-packages

import-from-hackage: ## Imports every cabal file from the ./index-01 directory
	@cabal run -- flora-cli import-packages ./01-index

repl: soufflé ## Start a cabal REPL
	@cabal repl lib:flora

ghci: repl ## Start a cabal REPL (alias for `make repl`)

watch: soufflé ## Load the main library and reload on file change
	@ghcid --target flora-server -l

test: build soufflé ## Run the test suite
	./scripts/run-tests.sh

watch-test: soufflé ## Load the tests in ghcid and reload them on file change
	./scripts/run-tests.sh --watch

watch-server: soufflé ## Start flora-server in ghcid
	@ghcid --target=flora-server --restart="src" --test 'FloraWeb.Server.runFlora'

lint: ## Run the code linter (HLint)
	@find app test src -name "*.hs" | xargs -P $(PROCS) -I {} hlint --refactor-options="-i" --refactor {}

style: ## Run the code formatters (stylish-haskell, cabal-fmt, nixfmt)
	@find app test src -name '*.hs' -exec fourmolu -i {} +
	@cabal-fmt -i flora.cabal
	@nixfmt *.nix

nix-shell: ## Enter the Nix shell
	@nix-shell

docker-build: ## Build and start the container cluster
	@docker-compose up -d --build

docker-start: ## Start the container cluster
	@docker-compose up -d

docker-enter: ## Enter the docker environment
	docker-compose exec flora-server "zsh"

start-tmux: ## Start a Tmux session with hot code reloading
	./scripts/start-tmux.sh

soufflé: ## Generate C++ files from the Soufflé Datalog definitions
	cd cbits ; souffle -g categorise.{cpp,dl}

tags: ## Generate ctags for the project with `ghc-tags`
	@ghc-tags -c

design-system: ## Generate the HTML components used by the design system
	@cabal run -- flora-cli gen-design-system

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.* ?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

UNAME := $(shell uname)

SHELL := /usr/bin/env bash

ifeq ($(UNAME), Darwin)
	PROCS := $(shell sysctl -n hw.logicalcpu)
else
	PROCS := $(shell nproc)
endif

.PHONY: all $(MAKECMDGOALS)

.DEFAULT_GOAL := help

.PHONY: migration
migration: ## Generate timestamped database migration boilerplate files
	@if test -z "$$name"; then \
	  echo "Usage: make migration name=some-name"; \
	else \
	  migName="`date -u '+%Y%m%d%H%M%S'`_$$name"; \
	  fname="migrations/$$migName.sql"; \
	  touch "$$fname"; \
	  echo "Touched $$fname";\
	fi

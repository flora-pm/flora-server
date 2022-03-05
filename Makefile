start: ## Start flora-server
	@cabal run exe:flora-server

build: ## Build the backend
	@cabal build -j -O0

clean: ## Remove the cabal build artifacts
	@cabal clean

assets-deps: ## Install the dependencies of the frontend
	@cd assets/ && yarn

assets-build: assets-deps ## Build the web assets
	@cd assets/ && yarn build

assets-watch: ## Continuously rebuild the web assets
	@cd assets/ && yarn watch

assets-clean: ## Remove JS artifacts
	@cd assets/ && rm -R node_modules

db-create: ## Create the database
	@createdb -h $(FLORA_DB_HOST) -p $(FLORA_DB_PORT) -U $(FLORA_DB_USER) $(FLORA_DB_DATABASE)

db-drop: ## Drop the database
	@dropdb --if-exists -h $(FLORA_DB_HOST) -p $(FLORA_DB_PORT) -U $(FLORA_DB_USER) $(FLORA_DB_DATABASE)

db-setup: db-create ## Setup the dev database
	@migrate init "$(FLORA_PG_CONNSTRING)" 
	@migrate migrate "$(FLORA_PG_CONNSTRING)" migrations

db-reset: db-drop db-setup db-provision ## Reset the dev database (uses Cabal)

db-provision: ## Load the development data in the database
	@cabal run -- flora-cli provision-fixtures

repl: soufflé ## Start a cabal REPL
	@cabal repl lib:flora

ghci: repl ## Start a cabal REPL (alias for `make repl`)

watch: soufflé ## Load the main library and reload on file change
	@ghcid --target flora-server -l

test: ## Run the test suite
	./scripts/run-tests.sh

watch-test: ## Load the tests in ghcid and reload them on file change
	./scripts/run-tests.sh --watch

watch-server: ## Start flora-server in ghcid
	@ghcid --target=flora-server --restart="src" --test 'FloraWeb.Server.runFlora'

lint: ## Run the code linter (HLint)
	@find app test src -name "*.hs" | xargs -P $(PROCS) -I {} hlint --refactor-options="-i" --refactor {}

style: ## Run the code formatters (stylish-haskell, cabal-fmt, nixfmt)
	@stylish-haskell -i -r src app test
	@cabal-fmt -i flora.cabal
	@nixfmt *.nix

nix-shell: ## Enter the Nix shell
	@nix-shell

docker-build: ## Build the docker image
	@docker-compose build

docker-start: ## Start the container cluster
	@docker-compose up -d

docker-enter: ## Enter the docker environment
	docker-compose exec flora-server "nix-shell"

soufflé: ## Generate C++ files from the Soufflé Datalog definitions
	cd cbits ; souffle -g categorise.{cpp,dl}

tags:
	@ghc-tags -c

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.* ?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

UNAME := $(shell uname)

SHELL := /bin/bash

ifeq ($(UNAME), Darwin)
	PROCS := $(shell sysctl -n hw.logicalcpu)
else
	PROCS := $(shell nproc)
endif

.PHONY: all $(MAKECMDGOALS)

.DEFAULT_GOAL := help

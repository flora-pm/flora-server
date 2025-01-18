init: ## Set up git hooks properly - needs calling once when cloning the repo
	@git config core.hooksPath .githooks

start: ## Start flora-server
	@cabal run exe:flora-server

build: soufflé ## Build the server
	@cabal build

build-release: soufflé ## Build the server for production
	@cabal freeze --project-file cabal.project.release
	@cabal build --project-file cabal.project.release

clean: ## Remove the cabal build artifacts
	@rm cbits/*.cpp
	@cabal clean

assets-deps: ## Install the dependencies of the frontend
	@cd assets/ && yarn install --immutable --immutable-cache --check-cache
	@cd docs/ && yarn install --immutable --immutable-cache --check-cache

build-assets: assets-deps ## Build the web assets
	@cd assets/ && yarn build
	@cd docs/ && yarn build

watch-assets: ## Continuously rebuild the web assets
	@cd assets/ && yarn watch

clean-assets: ## Remove JS artifacts
	@cd assets/ && rm -R node_modules
	@cd docs/ && rm -R node_modules

db-setup: db-create db-init db-migrate ## Setup the dev database

db-create: ## Create the database
	@createdb -h $(FLORA_DB_HOST) -p $(FLORA_DB_PORT) -U $(FLORA_DB_USER) $(FLORA_DB_DATABASE)

db-drop: ## Drop the database
	@dropdb -f --if-exists -h $(FLORA_DB_HOST) -p $(FLORA_DB_PORT) -U $(FLORA_DB_USER) $(FLORA_DB_DATABASE)

db-init: ## Create the database schema
	@migrate init "$(FLORA_DB_CONNSTRING)"

db-migrate: ## Apply database migrations
	@migrate migrate "$(FLORA_DB_CONNSTRING)" migrations

db-reset: db-drop db-setup db-provision ## Reset the dev database

db-provision: ## Create categories and repositories
	@cabal run -- flora-cli create-user --username "hackage-user" --email "tech@flora.pm" --password "foobar2000"
	@cabal run -- flora-cli provision categories
	@cabal run -- flora-cli provision-repository --name "hackage" --url https://hackage.haskell.org \
			--description "Central package repository"
	@cabal run -- flora-cli provision-repository --name "cardano" --url https://chap.intersectmbo.org \
			--description "Packages of the Cardano project"
	@cabal run -- flora-cli provision-repository --name "horizon" --url https://packages.horizon-haskell.net \
			--description "Packages of the Horizon project"

db-provision-advisories: ## Load HSEC advisories in the database
	@cabal run -- flora-cli provision advisories

db-provision-packages: ## Load development data in the dev database
	@cabal run -- flora-cli provision test-packages --repository "hackage"
	@cabal run -- flora-cli provision test-packages --repository "cardano"

db-test-create: ## Create the test database
	./scripts/run-with-test-config.sh db-create

db-test-setup: db-test-create db-test-init db-test-migrate ## Setup the dev database

db-test-drop: ## Drop the test database
	./scripts/run-with-test-config.sh db-drop

db-test-init: ## Create the test database schema
	./scripts/run-with-test-config.sh db-init

db-test-migrate: ## Apply test database migrations
	./scripts/run-with-test-config.sh db-migrate

db-test-reset: db-test-drop db-test-setup db-test-provision ## Reset the test database

db-test-provision: ## Create categories and repositories
	./scripts/run-with-test-config.sh db-provision

db-test-provision-advisories: ## Load HSEC advisories in the test database
	./scripts/run-with-test-config.sh db-provision-advisories

db-test-provision-packages: ## Load development data in the database
	./scripts/run-with-test-config.sh db-provision-packages

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

lint-hs: ## Run the code linter (HLint)
	@find app test src -name "*.hs" | xargs -P $(PROCS) -I {} hlint --refactor-options="-i" --refactor {}

style-hs-quick: ## Run the haskell code formatters (fourmolu, cabal-fmt)
	@cabal-fmt -i flora.cabal
	@git diff origin --name-only src test/**/*.hs app | xargs -P $(PROCS) -I {} fourmolu -q -i {}

style-hs: ## Run the haskell code formatters (fourmolu, cabal-fmt)
	@cabal-fmt -i flora.cabal
	@find app test src -name '*.hs' | xargs -P $(PROCS) -I {} fourmolu -q -i {}

style-css: ## Run the CSS code formatter (stylelint)
	@cd assets ; yarn stylelint --fix css --ignore-path .stylelintignore

style: style-hs style-css ## Run all the code formatters

nix-shell: ## Enter the Nix shell
	@nix-shell

docker-build: ## Build and start the container cluster
	@docker build .

docker-up: ## Start the container cluster
	@docker compose up -d

docker-stop: ## Stop the container cluster without removing the containers
	@docker compose stop

docker-down: ## Stop and remove the container cluster
	@docker compose down

docker-enter: ## Enter the docker environment
	docker compose exec server zsh

start-tmux: ## Start a Tmux session with hot code reloading
	./scripts/start-tmux.sh

tags: ## Generate ctags for the project with `ghc-tags`
	@ghc-tags -c

design-system: ## Generate the HTML components used by the design system
	@cabal run -- flora-cli gen-design-system

start-design-sysytem: ## Start storybook.js
	@cd design; yarn storybook

migration: ## Generate timestamped database migration boilerplate files
	@if test -z "$$name"; then \
	  echo "Usage: make migration name=some-name"; \
	else \
	  migName="`date -u '+%Y%m%d%H%M%S'`_$$name"; \
	  fname="migrations/$$migName.sql"; \
	  touch "$$fname"; \
	  echo "Touched $$fname";\
	fi

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

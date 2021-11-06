db-init: ## Initialize the dev database
	@initdb -D _database

db-start: ## Start the dev database
	@postgres -D _database

db-create: ## Create the database
	@createdb hpkg_dev

db-setup: ## Setup the dev database
	@migrate init "$(PG_CONNSTRING)" 
	@migrate migrate "$(PG_CONNSTRING)" migrations

db-reset: ## Reset the dev database
	@dropdb --if-exists hpkg_dev
	@make db-create
	@make db-setup

repl: ## Start a REPL
	@cabal repl

test: ## Run the test suite
	@cabal test

ghcid-test: ## Load the tests in ghcid and reload them on file change
	@ghcid --command='cabal v2-repl hpkg-test' --test 'Main.main'

lint: ## Run the code linter (HLint)
	@find app test src -name "*.hs" | xargs -P $(PROCS) -I {} hlint --refactor-options="-i" --refactor {}

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.* ?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

UNAME := $(shell uname)

ifeq ($(UNAME), Darwin)
	PROCS := $(shell sysctl -n hw.logicalcpu)
else
	PROCS := $(shell nproc)
endif

.PHONY: all $(MAKECMDGOALS)

.DEFAULT_GOAL := help

start: ## Start flora-server
	@cabal run exe:flora-server

build: ## Build the server without optimisations
	@cabal build -O0

assets-deps: ## Install the dependencies of the frontend
	@cd assets/ && yarn

assets-build: ## Build the web assets
	@cd assets/ && yarn build

assets-watch: ## Continuously rebuild the web assets
	@cd assets/ && yarn watch

assets-clean: ## Remove JS artifacts
	@cd assets/ && rm -R node_modules

db-init: ## Initialize the dev database
	@initdb -D _database

db-start: ## Start the dev database
	@postgres -D _database

db-create: ## Create the database
	@createdb -h $(FLORA_DB_HOST) -p $(FLORA_DB_PORT) -U $(FLORA_DB_USER) $(FLORA_DB_DATABASE)

db-drop: ## Drop the database
	@dropdb --if-exists -h $(FLORA_DB_HOST) -p $(FLORA_DB_PORT) -U $(FLORA_DB_USER) $(FLORA_DB_DATABASE)

db-setup: db-create ## Setup the dev database
	@migrate init "$(FLORA_PG_CONNSTRING)" 
	@migrate migrate "$(FLORA_PG_CONNSTRING)" migrations

db-reset: db-drop db-setup ## Reset the dev database
	@cabal run -- flora-cli provision-fixtures

repl: ## Start a REPL
	@cabal repl lib:flora

ghci: repl ## Start a REPL (alias for `make repl`)

test: ## Run the test suite
	@cabal test

ghcid-test: ## Load the tests in ghcid and reload them on file change
	@ghcid --command='cabal v2-repl flora-test' --test 'Main.main'

ghcid-server: ## Start flora-server in ghcid
	@ghcid --target=flora-server --restart="src" --test 'FloraWeb.Server.runFlora'

lint: ## Run the code linter (HLint)
	@find app test src -name "*.hs" | xargs -P $(PROCS) -I {} hlint --refactor-options="-i" --refactor {}

style: ## Run the code styler (stylish-haskell)
	@stylish-haskell -i -r src app test
	@cabal-fmt -i flora.cabal

nix-shell: ### Enter the nix shell
	@nix-shell

tags:
	@ghc-tags -c

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

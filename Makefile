start: ## Start flora-server
	@cabal run exe:flora-server

build: ## Build the backend
	@cabal build -O0

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

repl: ## Start a REPL
	@cabal repl lib:flora

ghci: repl ## Start a REPL (alias for `make repl`)

test: ## Run the test suite
	@cabal run -- flora-test

ghcid-test: ## Load the tests in ghcid and reload them on file change
	@ghcid --command='cabal v2-repl flora-test' --test 'Main.main'

ghcid-server: ## Start flora-server in ghcid
	@ghcid --target=flora-server --restart="src" --test 'FloraWeb.Server.runFlora'

lint: ## Run the code linter (HLint)
	@find app test src -name "*.hs" | xargs -P $(PROCS) -I {} hlint --refactor-options="-i" --refactor {}

style: ## Run the code formatters (stylish-haskell, cabal-fmt, nixfmt)
	@stylish-haskell -i -r src app test
	@cabal-fmt -i flora.cabal
	@nixfmt *.nix nix/*.nix

nix-build: ## Build the backend with Nix
	@nix-build

nix-start: ## Start the server build with Nix. Does not source the environment.
	./result/bin/flora-server

nix-shell: ## Enter the Nix shell
	@nix-shell

nix-provision: ## 
	./result/bin/flora-cli provision-fixtures

nix-clean: ## Clean the Nix build artifacts
	@nix-store --delete --ignore-liveness result
	@rm result

nix-tmux: nix-build ## Start a tmux session with the nix build system
	@./scripts/start-tmux.sh

docker-build: ## Build the docker image
	@docker-compose build

docker-start: ## Start the container cluster
	@docker-compose up -d

docker-enter: ## Enter the docker environment
	docker-compose exec flora-server "nix-shell"

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

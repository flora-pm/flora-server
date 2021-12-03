$(if $(FLORA_DB_URI),,$(error FLORA_DB_URI is not defined))

start: ## Start flora-server
	@cabal run exe:flora-server

build: ## Build the server without optimisations
	@cabal build -O0

clean: assets-clean ## Clean the build artifact for backend and frontend
	@cabal clean

assets-deps: ## Install the dependencies of the frontend
	@cd assets/ && yarn

assets-build: ## Build the web assets
	@cd assets/ && yarn build

assets-watch: ## Continuously rebuild the web assets
	@cd assets/ && yarn watch

assets-clean: ## Remove JS artifacts
	@cd assets/ ; [[ -f node_modules ]] && rm -R node_modules || exit 0

db-create: ## Create the database
	dbmate --url "$(FLORA_DB_URI)" create

db-drop: ## Drop the database
	dbmate --url "$(FLORA_DB_URI)" drop

db-setup: ## Setup the dev database
	dbmate --url "$(FLORA_DB_URI)" up

db-reset: db-drop db-create db-setup ## Reset the dev database
	@cabal run -- flora-cli provision-fixtures

repl: ## Start a REPL
	@cabal repl

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

nix-shell: ## Enter the nix shell
	@nix-sell

tags: ## Generate Haskell tags with 'ghc-tags'
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

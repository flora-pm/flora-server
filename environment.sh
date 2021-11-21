export NIXPKGS_ALLOW_BROKEN=1

export FLORA_DB_HOST="localhost"
export FLORA_DB_PORT="5432"
export FLORA_DB_USER="postgres"
export FLORA_DB_PASSWORD="postgres"
export FLORA_DB_DATABASE="flora_dev"
export FLORA_DB_POOL_CONNECTIONS="10"
export FLORA_DB_SUB_POOLS="10"
export FLORA_DB_TIMEOUT="10"

export FLORA_PG_URI="postgresql://${FLORA_DB_USER}:${FLORA_DB_PASSWORD}@${FLORA_DB_HOST}:${FLORA_DB_PORT}/${FLORA_DB_DATABASE}"
export FLORA_PG_CONNSTRING="host=${FLORA_DB_HOST} dbname=${FLORA_DB_DATABASE} user=${FLORA_DB_USER} password=${FLORA_DB_PASSWORD}"

export FLORA_HTTP_PORT=8083
export FLORA_ENVIRONMENT="development"
export FLORA_DOMAIN="localhost"

# Compatibility mode for Hackage.
# This includes:
#
#   * Accept multiple packages with the same name but different case
#   * Accept multiple users with the same name but different case
export FLORA_COMPATIBILITY_MODE=true

# Set these variables in `environment.local.sh`, which is not tracked by git.
#export SENTRY_DSN="" # Set this variable to connecto to your Sentry instance
#export FLORA_PROMETHEUS_ENABLED="true" # Set this variable to true or false to enable Prometheus metrics export

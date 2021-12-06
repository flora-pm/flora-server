export DB_HOST="localhost"
export DB_PORT="5432"
export DB_USER="postgres"
export DB_PASSWORD="postgres"
export DB_DATABASE="flora_dev"
export DB_POOL_CONNECTIONS="10"
export DB_SUB_POOLS="10"
export DB_TIMEOUT="10"

export PG_URI="postgresql://${DB_USER}:${DB_PASSWORD}@${DB_HOST}:${DB_PORT}/${DB_DATABASE}"
export PG_CONNSTRING="host=${DB_HOST} dbname=${DB_DATABASE} user=${DB_USER} password=${DB_PASSWORD}"

export FLORA_PORT=8083
export FLORA_ENVIRONMENT="local"

# Set these variables in `environment.local.sh`, which is not tracked by git.
#export SENTRY_DSN="" # Set this variable to connecto to your Sentry instance
#export FLORA_PROMETHEUS_ENABLED="true" # Set this variable to true or false to enable Prometheus metrics export

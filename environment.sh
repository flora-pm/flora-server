export NIXPKGS_ALLOW_BROKEN=1
export DATALOG_DIR="cbits/"
export COMPOSE_PROFILES=local

export FLORA_DB_HOST="localhost"
export FLORA_DB_PORT="5432"
export FLORA_DB_USER="postgres"
export FLORA_DB_PASSWORD="postgres"
export FLORA_DB_DATABASE="flora_dev"
export FLORA_DB_POOL_CONNECTIONS="50"
export FLORA_DB_TIMEOUT="10"
export FLORA_DB_SSLMODE="allow"
export FLORA_DB_PARAMETERS="?sslmode=verify-ca"

export FLORA_DB_CONNSTRING="host=${FLORA_DB_HOST} dbname=${FLORA_DB_DATABASE} port=${FLORA_DB_PORT} \
  user=${FLORA_DB_USER} password=${FLORA_DB_PASSWORD} \
  sslmode=${FLORA_DB_SSLMODE:-allow}"

export FLORA_HTTP_PORT=8083
export FLORA_ENVIRONMENT="development"
export FLORA_DOMAIN="localhost"

# Either "stdout" or "json"
export FLORA_LOGGING_DESTINATION="stdout"

# Compatibility mode for Hackage.
# This includes:
#
#   * Accept multiple packages with the same name but different case
#   * Accept multiple users with the same name but different case
export FLORA_COMPATIBILITY_MODE="True"

# Set these variables in `environment.local.sh`, which is not tracked by git.
#export SENTRY_DSN="" # Set this variable to connect to to your Sentry instance
#export FLORA_PROMETHEUS_ENABLED="true" # Set this variable to true or false to enable Prometheus metrics export
#export FLORA_ZIPKIN_ENABLED="true" # Set this variable to true to enable Zipkin traces export
#export FLORA_ZIPKIN_AGENT_HOST="localhost" # Set this variable to true to set the hostname of the agent to which the traces are shipped
#export FLORA_ZIPKIN_AGENT_PORT="localhost" # Set this variable to true to set the port of the agent to which the traces are shipped

source environment.sh

export FLORA_DB_DATABASE="flora_test"
export FLORA_DB_PASSWORD="postgres"
export FLORA_DB_POOL_CONNECTIONS=20
export FLORA_DB_PORT=5432
export FLORA_DB_HOST="localhost"
export FLORA_DB_TIMEOUT=10
export FLORA_DB_USER="postgres"
export FLORA_LOGGING_DESTINATION="stdout"
export FLORA_HTTP_PORT=8083

export FLORA_DB_CONNSTRING="host=${FLORA_DB_HOST} dbname=${FLORA_DB_DATABASE} user=${FLORA_DB_USER} password=${FLORA_DB_PASSWORD}"
